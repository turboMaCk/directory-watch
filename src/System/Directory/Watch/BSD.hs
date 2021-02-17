{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingVia #-}

module System.Directory.Watch.BSD (
    Id,
    Handle,
    initBackend,
    closeBackend,
    toEvent,
    addTouch,
    addMkDir,
    getBackendEvent,
    isDirectory,
    getId,
    internalWatch,
    addBoth,
) where

import Data.Hashable
import Foreign.C.Types (CULong (..))
import qualified Data.HashMap.Strict as Map

import qualified System.KQueue as KQueue
import Foreign.C.Types (CULong)

import System.Directory.Watch.Portable


type Handle = KQueue.KQueue


type BackendEvent = KQueue.KEvent


type Id = CULong


instance Hashable CULong where
    hash (CULong word) = hash word
    hashWithSalt i (CULong word) = hashWithSalt i word


initBackend :: IO Handle
initBackend = KQueue.kqueue
{-# INLINE initBackend #-}


closeBackend :: Handle -> IO ()
closeBackend _ = pure ()
{-# INLINE closeBackend #-}


toEvent :: FilePath -> BackendEvent -> Event
toEvent path _ = undefined
{-# INLINE toEvent #-}


addTouch :: Handle -> FilePath -> IO Id
addTouch handle path = undefined
{-# INLINE addTouch #-}


addMkDir :: Handle -> FilePath -> IO Id
addMkDir handle path = undefined
{-# INLINE addMkDir #-}


addBoth :: Handle -> FilePath -> IO Id
addBoth handle path = undefined
{-# INLINE addBoth #-}


getBackendEvent :: Handle -> IO BackendEvent
getBackendEvent = undefined
{-# INLINE getBackendEvent #-}


isDirectory :: BackendEvent -> Bool
isDirectory _ = undefined
{-# INLINE isDirectory #-}


getId :: BackendEvent -> Id
getId _ = undefined
{-# INLINE getId #-}


internalWatch :: Handle -> FilePath -> IO Id
internalWatch handle path = undefined
{-# INLINE internalWatch #-}
