{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}

{- | KQueue backend needs has to detect the change that occured within
the directory on its own. See:
See https://forums.freebsd.org/threads/kqueue-kevent-determine-filename-from-file-description.25547/#post-143319
-}
module System.Directory.Watch.BSD where

import qualified Control.Concurrent.STM as Stm
import Data.Foldable
import qualified Data.HashMap.Strict as Map
import Data.Hashable
import Foreign.C.Types (CULong (..))
import Foreign.Ptr (nullPtr)
import System.KQueue (FFlag (..), Filter (..), Flag (..), KEvent (..))
import Prelude hiding (init)

import Foreign.C.Types (CULong)
import qualified System.KQueue as KQueue
import System.Posix.IO (OpenMode (ReadOnly), closeFd, defaultFileFlags, openFd)
import System.Posix.Types (Fd)

import System.Directory.Watch.Portable


type Handle = (KQueue.KQueue, Stm.TVar [KEvent])


type BackendEvent = KEvent


type Id = KEvent


instance Hashable CULong where
    hash (CULong word) = hash word
    hashWithSalt i (CULong word) = hashWithSalt i word


instance Hashable KQueue.KEvent where
    -- From documentation:
    -- A kevent is iden-tified identifiedtified
    -- by an (ident, filter) pair and specifies the interesting conditions
    -- conditionstions to be notified about for that pair.  An (ident, filter) pair can
    -- only appear once is a given kqueue.
    hash KEvent{..} = hash (ident, fromEnum evfilter)
    hashWithSalt i KEvent{..} = hashWithSalt i (ident, fromEnum evfilter)


init :: IO Handle
init = do
    kq <- KQueue.kqueue
    events <- Stm.newTVarIO []
    pure (kq, events)
{-# INLINE init #-}


close :: Handle -> IO ()
close (_, tEvents) = do
    events <- Stm.atomically $ Stm.readTVar tEvents
    for_ events $ closeFd . fromIntegral . ident
{-# INLINE close #-}


toEvent :: FilePath -> BackendEvent -> Event
toEvent path KEvent{..} = Event{..}
  where
    filePath = path
    -- TODO: Will need detection
    eventType = Touch
{-# INLINE toEvent #-}


addTouch :: Handle -> FilePath -> IO Id
addTouch (_, events) path = do
    ident <- fromIntegral <$> openFd path ReadOnly Nothing defaultFileFlags
    let event = getEvent ident
    Stm.atomically $ Stm.modifyTVar' events ((:) event)
    pure event
  where
    -- TODO: encode event type
    getEvent ident =
        KEvent
            { ident = ident
            , evfilter = EvfiltVnode
            , flags = [EvAdd, EvOneshot]
            , fflags = [NoteWrite]
            , data_ = 0
            , udata = nullPtr
            }
{-# INLINE addTouch #-}


addMkDir :: Handle -> FilePath -> IO Id
addMkDir (_, events) path = do
    ident <- fromIntegral <$> openFd path ReadOnly Nothing defaultFileFlags
    let event = getEvent ident
    Stm.atomically $ Stm.modifyTVar' events ((:) event)
    pure event
  where
    -- TODO: encode event type
    getEvent ident =
        KEvent
            { ident = ident
            , evfilter = EvfiltVnode
            , flags = [EvAdd, EvOneshot]
            , fflags = [NoteWrite]
            , data_ = 0
            , udata = nullPtr
            }
{-# INLINE addMkDir #-}


addBoth :: Handle -> FilePath -> IO Id
addBoth handle path = undefined
{-# INLINE addBoth #-}


getEvent :: Handle -> IO BackendEvent
getEvent (kq, tEvents) = do
    events <- Stm.atomically $ Stm.readTVar tEvents
    [e] <- KQueue.kevent kq events 1 Nothing
    pure e
{-# INLINE getEvent #-}


isDirectory :: BackendEvent -> Bool
isDirectory KEvent{..} = evfilter == EvfiltVnode
{-# INLINE isDirectory #-}


getId :: BackendEvent -> Id
getId = id
{-# INLINE getId #-}


internalWatch :: Handle -> FilePath -> IO Id
internalWatch (_, events) path = do
    -- TODO: when file gets removed we should also close the FD and clean the @Handle@
    ident <- fromIntegral <$> openFd path ReadOnly Nothing defaultFileFlags
    let event = getEvent ident
    Stm.atomically $ Stm.modifyTVar' events ((:) event)
    pure event
  where
    getEvent ident =
        KEvent
            { ident = ident
            , evfilter = EvfiltVnode
            , flags = [EvAdd, EvOneshot]
            , fflags = [NoteDelete, NoteRevoke]
            , data_ = 0
            , udata = nullPtr
            }
{-# INLINE internalWatch #-}
