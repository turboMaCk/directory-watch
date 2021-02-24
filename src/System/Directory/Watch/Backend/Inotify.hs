{-# LANGUAGE RecordWildCards #-}

module System.Directory.Watch.Backend.Inotify where

import qualified Data.ByteString.UTF8 as Utf8
import qualified Data.HashMap.Strict as Map
import qualified System.Linux.Inotify as Inotify
import Prelude hiding (init)

import System.Directory.Watch.Portable


type Handle = Inotify.Inotify


type BackendEvent = Inotify.Event


type Id = Inotify.Watch


init :: IO Handle
init = Inotify.init
{-# INLINE init #-}


close :: Handle -> IO ()
close = Inotify.close
{-# INLINE close #-}


toEvent :: FilePath -> BackendEvent -> Maybe Event
toEvent path Inotify.Event{..} = (\eventType -> Event{..}) <$> mEventType
  where
    decodedName = Utf8.toString name
    filePath
        | null decodedName = path
        | otherwise = path <> "/" <> decodedName
    mEventType
        | Inotify.isSubset Inotify.in_ISDIR mask = Just DirectoryCreated
        | Inotify.isSubset Inotify.in_CLOSE_WRITE mask = Just FileModified
        | Inotify.isSubset Inotify.in_CREATE mask = Just FileCreated
        | otherwise = Nothing
{-# INLINE toEvent #-}


watchDirectory :: Handle -> FilePath -> IO [Id]
watchDirectory handle path =
    pure <$> Inotify.addWatch handle path Inotify.in_CREATE
{-# INLINE watchDirectory #-}


watchFile :: Handle -> FilePath -> IO [Id]
watchFile handle path =
    pure <$> Inotify.addWatch handle path Inotify.in_CLOSE_WRITE
{-# INLINE watchFile #-}


getEvent :: Handle -> IO BackendEvent
getEvent = Inotify.getEvent
{-# INLINE getEvent #-}


isDirectory :: BackendEvent -> Bool
isDirectory Inotify.Event{..} =
    Inotify.isSubset Inotify.in_ISDIR mask
{-# INLINE isDirectory #-}


getId :: BackendEvent -> Id
getId Inotify.Event{..} = wd
{-# INLINE getId #-}


internalWatch :: Handle -> FilePath -> IO Id
internalWatch handle path =
    Inotify.addWatch handle path Inotify.in_DELETE_SELF
{-# INLINE internalWatch #-}
