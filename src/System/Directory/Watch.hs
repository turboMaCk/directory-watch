{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module System.Directory.Watch (
    Event (..),
    EventType (..),
    Manager,
    withManager,
    watchTouch,
    watchMkDir,
    watchBoth,
    getEvent,
    keepWatching,
) where

import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import Control.Monad (forever, when)
import Data.Functor (void)
import Data.Maybe (fromMaybe)

import qualified Control.Concurrent.STM as Stm
import qualified Data.HashMap.Strict as Map

import qualified System.Directory.Watch.Backend as Backend
import System.Directory.Watch.Portable


type Registery = Map.HashMap Backend.Id FilePath


{- | In theory these 2 should aline in a way that
 3 in internal map will be 3 in the registery as well
 however this seems to be more robust especially since
 we allow for concurrency
-}
type InternalRegMap = Map.HashMap Backend.Id Backend.Id


data Manager = Manager
    { handle :: !Backend.Handle
    , internalHandle :: !Backend.Handle
    , registery :: !(Stm.TVar Registery)
    , internalRegMap :: !(Stm.TVar InternalRegMap)
    }


withManager :: (Manager -> IO a) -> IO a
withManager action =
    bracket Backend.init Backend.close $ \handle ->
        bracket Backend.init Backend.close $ \internalHandle -> do
            registery <- Stm.newTVarIO Map.empty
            internalRegMap <- Stm.newTVarIO Map.empty

            -- Handle removal of watched directories
            forkIO $
                forever $ do
                    backendEvent <- Backend.getEvent internalHandle
                    putStrLn $ "[INTERNAL] INotify event: " <> show backendEvent

                    when (Backend.isDirectory backendEvent) $
                        Stm.atomically $ do
                            mWatch <- Map.lookup (Backend.getId backendEvent) <$> Stm.readTVar internalRegMap

                            -- Remove from registery
                            whenJust mWatch $
                                Stm.modifyTVar' registery . Map.delete

                            -- Remove from internal map
                            Stm.modifyTVar' internalRegMap $ Map.delete $ Backend.getId backendEvent

            action $ Manager{..}


watching :: Manager -> FilePath -> Backend.Id -> IO ()
watching Manager{..} path watch = do
    internalWatch <- Backend.internalWatch internalHandle path
    Stm.atomically $ do
        -- Add to registery
        Stm.modifyTVar' registery $ Map.insert watch path
        -- Add to internal reg map
        Stm.modifyTVar' internalRegMap $ Map.insert internalWatch watch


watchTouch :: Manager -> FilePath -> IO ()
watchTouch Manager{..} path = do
    -- TODO: check if directory
    putStrLn $ "watchTouch: " <> path
    watch <- Backend.addTouch handle path
    watching Manager{..} path watch


watchMkDir :: Manager -> FilePath -> IO ()
watchMkDir Manager{..} path = do
    -- TODO: check if directory
    putStrLn $ "watchMkdir: " <> path
    watch <- Backend.addMkDir handle path
    watching Manager{..} path watch


watchBoth :: Manager -> FilePath -> IO ()
watchBoth Manager{..} path = do
    putStrLn $ "watchBoth: " <> path
    watch <- Backend.addBoth handle path
    watching Manager{..} path watch


getEvent :: Manager -> (Event -> IO ()) -> IO ()
getEvent Manager{..} f = do
    iEvent <- Backend.getEvent handle
    putStrLn $ "Backend event: " <> show iEvent
    snapshot <- Stm.readTVarIO registery

    let mPath = Map.lookup (Backend.getId iEvent) snapshot
    case mPath of
        Just path -> f $ Backend.toEvent path iEvent
        Nothing -> putStrLn $ "[ERROR] can't find path for " <> show iEvent


keepWatching :: Manager -> (Event -> IO ()) -> IO ()
keepWatching manager = forever . getEvent manager


-- Helpers that should really be in the base but aren't

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just v) f = f v
whenJust Nothing _ = pure ()
