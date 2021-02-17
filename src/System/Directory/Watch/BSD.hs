{-# LANGUAGE RecordWildCards #-}

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
) where

import qualified Data.HashMap.Strict as Map

import System.Directory.Watch.Portable


type Handle = ()


type BackendEvent = ()


type Id = ()


initBackend :: IO Handle
initBackend = undefined
{-# INLINE initBackend #-}


closeBackend :: Handle -> IO ()
closeBackend = undefined
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
