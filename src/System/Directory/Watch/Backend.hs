{-# LANGUAGE CPP #-}

module System.Directory.Watch.Backend (
    Id,
    Handle,
    init,
    close,
    toEvent,
    addTouch,
    addMkDir,
    getEvent,
    isDirectory,
    getId,
    internalWatch,
    addBoth,
) where

import Prelude hiding (init)


#ifdef OS_Linux
import System.Directory.Watch.Backend.Inotify
#endif

#ifdef OS_MacOS
import System.Directory.Watch.Backend.KQueue
#endif
