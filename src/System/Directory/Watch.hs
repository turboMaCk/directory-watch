{-# LANGUAGE RecordWildCards #-}

module System.Directory.Watch (
    Event (..),
    EventType (..),
    Manager,
    withManager,
    watchTouch,
    watchMkDir,
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

import System.Directory.Watch.Linux
import System.Directory.Watch.Portable


data Manager = Manager
    { handle :: !Handle
    , internalHandle :: !Handle
    , registery :: !(Stm.TVar Registery)
    , internalRegMap :: !(Stm.TVar InternalRegMap)
    }


withManager :: (Manager -> IO a) -> IO a
withManager action =
    bracket initBackend closeBackend $ \handle ->
        bracket initBackend closeBackend $ \internalHandle -> do
            registery <- Stm.newTVarIO Map.empty
            internalRegMap <- Stm.newTVarIO Map.empty

            -- Handle removal of watched directories
            forkIO $
                forever $ do
                    backendEvent <- getBackendEvent internalHandle
                    putStrLn $ "[INTERNAL] INotify event: " <> show backendEvent

                    when (isDirectory backendEvent) $
                        Stm.atomically $ do
                            mWatch <- Map.lookup (getId backendEvent) <$> Stm.readTVar internalRegMap

                            -- Remove from registery
                            whenJust mWatch $
                                Stm.modifyTVar' registery . Map.delete

                            -- Remove from internal map
                            Stm.modifyTVar' internalRegMap $ Map.delete $ getId backendEvent

            action $ Manager{..}


watching :: Manager -> FilePath -> Id -> IO ()
watching Manager{..} path watch = do
    internalWatch <- internalWatch internalHandle path
    Stm.atomically $ do
        -- Add to registery
        Stm.modifyTVar' registery $ Map.insert watch path
        -- Add to internal reg map
        Stm.modifyTVar' internalRegMap $ Map.insert internalWatch watch


watchTouch :: Manager -> FilePath -> IO ()
watchTouch Manager{..} path = do
    -- TODO: check if directory
    putStrLn $ "watchTouch: " <> path
    watch <- addTouch handle path
    watching Manager{..} path watch


watchMkDir :: Manager -> FilePath -> IO ()
watchMkDir Manager{..} path = do
    -- TODO: check if directory
    putStrLn $ "watchMkdir: " <> path
    watch <- addMkDir handle path
    watching Manager{..} path watch


getEvent :: Manager -> (Event -> IO ()) -> IO ()
getEvent Manager{..} f = do
    iEvent <- getBackendEvent handle
    putStrLn $ "INotify event: " <> show iEvent
    snapshot <- Stm.readTVarIO registery

    let mPath = Map.lookup (getId iEvent) snapshot
    case mPath of
        Just path -> f $ toEvent path iEvent
        Nothing -> putStrLn $ "[ERROR] can't find path for " <> show iEvent


keepWatching :: Manager -> (Event -> IO ()) -> IO ()
keepWatching manager = forever . getEvent manager


-- Helpers that should really be in the base but aren't

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just v) f = f v
whenJust Nothing _ = pure ()
