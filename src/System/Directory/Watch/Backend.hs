{-# LANGUAGE CPP #-}

module System.Directory.Watch.Backend (
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


#ifdef OS_Linux
import System.Directory.Watch.Backend.Inotify
#endif

#ifdef OS_MacOS
import System.Directory.Watch.Backend.KQueue
#endif
