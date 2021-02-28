{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Directory.Watch.Backend.Inotify where

import qualified Data.ByteString as BS
import qualified System.Linux.Inotify as Inotify
import System.Posix.ByteString.FilePath (RawFilePath)

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


toEvent :: RawFilePath -> BackendEvent -> Maybe Action
toEvent path Inotify.Event{..} = (\actionType -> Action{..}) <$> mActionType
  where
    rawFilePath
        | BS.null name = path
        | otherwise = path <> "/" <> name
    mActionType
        | Inotify.isSubset Inotify.in_IGNORED mask = Just Removed
        | Inotify.isSubset Inotify.in_ISDIR mask = Just $ Created Directory
        | Inotify.isSubset Inotify.in_CLOSE_WRITE mask = Just Modified
        | Inotify.isSubset Inotify.in_CREATE mask = Just $ Created File
        | otherwise = Nothing
{-# INLINE toEvent #-}


watchDirectory :: Handle -> RawFilePath -> IO Id
watchDirectory handle path =
    Inotify.addWatch_ handle path Inotify.in_CREATE
{-# INLINE watchDirectory #-}


watchFile :: Handle -> RawFilePath -> IO Id
watchFile handle path =
    Inotify.addWatch_ handle path Inotify.in_CLOSE_WRITE
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


internalWatch :: Handle -> RawFilePath -> IO Id
internalWatch handle path =
    Inotify.addWatch_ handle path Inotify.in_DELETE_SELF
{-# INLINE internalWatch #-}
