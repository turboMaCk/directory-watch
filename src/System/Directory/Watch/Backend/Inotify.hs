{-# LANGUAGE RecordWildCards #-}

module System.Directory.Watch.Backend.Inotify where

import qualified Data.ByteString.UTF8 as Utf8
import qualified Data.HashMap.Strict as Map
import Data.Traversable (for)
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


toEvent :: FilePath -> BackendEvent -> Event
toEvent path Inotify.Event{..} = Event{..}
  where
    filePath = path <> "/" <> Utf8.toString name
    eventType
        | Inotify.isSubset Inotify.in_ISDIR mask = DirectoryCreated
        | otherwise = FileCreated
{-# INLINE toEvent #-}


watchModify :: Handle -> FilePath -> IO Id
watchModify handle path =
    Inotify.addWatch handle path Inotify.in_MODIFY
{-# INLINE watchModify #-}


watchCreate :: Handle -> FilePath -> IO Id
watchCreate handle path =
    Inotify.addWatch handle path Inotify.in_CREATE
{-# INLINE watchCreate #-}


watchDirectory :: Handle -> FilePath -> IO [Id]
watchDirectory handle path =
    for [watchModify, watchCreate] $
        \f -> f handle path
{-# INLINE watchDirectory #-}


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
