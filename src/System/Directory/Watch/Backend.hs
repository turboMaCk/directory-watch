{-# LANGUAGE CPP #-}

module System.Directory.Watch.Backend (
    Id,
    Handle,
    init,
    close,
    toEvent,
    getEvent,
    isDirectory,
    getId,
    internalWatch,
    watchDirectory,
    watchFile,
) where

import Prelude hiding (init)


#ifdef OS_Linux
import System.Directory.Watch.Backend.Inotify
#endif

#ifdef OS_MacOS
import System.Directory.Watch.Backend.KQueue
#endif
