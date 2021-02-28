-- for FilePath literals
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import System.Environment (getArgs)
import System.FSNotify


main :: IO ()
main = do
    [path] <- getArgs

    withManager $ \mgr -> do
        -- start a watching job (in the background)
        _ <- watchTree
            mgr -- manager
            path -- directory to watch
            (const True) -- predicate
            $ \event -> putStrLn $ "Event: " <> show event

        -- sleep forever (until interrupted)
        forever $ threadDelay 1000000
