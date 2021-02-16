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
import qualified Control.Concurrent.STM as Stm
import Control.Exception (bracket)
import Control.Monad (forever, when)
import qualified Data.ByteString.UTF8 as Utf8
import Data.Functor (void)
import qualified Data.HashMap.Strict as Map
import Data.Maybe (fromMaybe)
import qualified System.Linux.Inotify as Inotify


data EventType
    = MkDir
    | Touch
    deriving (Show)


data Event = Event
    { eventType :: !EventType
    , filePath :: !FilePath
    }
    deriving (Show)


toEvent :: FilePath -> Inotify.Event -> Event
toEvent path Inotify.Event{..} = Event{..}
  where
    filePath = path <> "/" <> Utf8.toString name
    eventType
        | Inotify.isSubset Inotify.in_ISDIR mask = MkDir
        | otherwise = Touch


type Registery = Map.HashMap Inotify.Watch FilePath


{- | In theory these 2 should aline in a way that
 3 in internal map will be 3 in the registery as well
 however this seems to be more robust especially since
 we allow for concurrency
-}
type InternalRegMap = Map.HashMap Inotify.Watch Inotify.Watch


data Manager = Manager
    { handle :: !Inotify.Inotify
    , internalHandle :: !Inotify.Inotify
    , registery :: !(Stm.TVar Registery)
    , internalRegMap :: !(Stm.TVar InternalRegMap)
    }


withManager :: (Manager -> IO a) -> IO a
withManager action =
    bracket Inotify.init Inotify.close $ \handle ->
        bracket Inotify.init Inotify.close $ \internalHandle -> do
            registery <- Stm.newTVarIO Map.empty
            internalRegMap <- Stm.newTVarIO Map.empty

            -- Handle removal of watched directories
            forkIO $
                forever $ do
                    Inotify.Event{..} <- Inotify.getEvent internalHandle
                    putStrLn $ "[INTERNAL] INotify event: " <> show Inotify.Event{..}

                    when (Inotify.isSubset Inotify.in_ISDIR mask) $
                        Stm.atomically $ do
                            mWatch <- Map.lookup wd <$> Stm.readTVar internalRegMap

                            -- Remove from registery
                            whenJust mWatch $
                                Stm.modifyTVar' registery . Map.delete

                            -- Remove from internal map
                            Stm.modifyTVar' internalRegMap $ Map.delete wd

            action $ Manager{..}


watching :: Manager -> FilePath -> Inotify.Watch -> IO ()
watching Manager{..} path watch = do
    internalWatch <- Inotify.addWatch internalHandle path Inotify.in_DELETE_SELF
    Stm.atomically $ do
        -- Add to registery
        Stm.modifyTVar' registery $ Map.insert watch path
        -- Add to internal reg map
        Stm.modifyTVar' internalRegMap $ Map.insert internalWatch watch


watchTouch :: Manager -> FilePath -> IO ()
watchTouch Manager{..} path = do
    -- TODO: check if directory
    putStrLn $ "watchTouch: " <> path
    watch <- Inotify.addWatch handle path Inotify.in_MODIFY
    watching Manager{..} path watch


watchMkDir :: Manager -> FilePath -> IO ()
watchMkDir Manager{..} path = do
    -- TODO: check if directory
    putStrLn $ "watchMkdir: " <> path
    watch <- Inotify.addWatch handle path Inotify.in_CREATE
    watching Manager{..} path watch


getEvent :: Manager -> (Event -> IO ()) -> IO ()
getEvent Manager{..} f = do
    iEvent <- Inotify.getEvent handle
    putStrLn $ "INotify event: " <> show iEvent
    snapshot <- Stm.readTVarIO registery

    let mPath = Map.lookup (Inotify.wd iEvent) snapshot
    case mPath of
        Just path -> f $ toEvent path iEvent
        Nothing -> putStrLn $ "[ERROR] can't find path for " <> show iEvent


keepWatching :: Manager -> (Event -> IO ()) -> IO ()
keepWatching manager = forever . getEvent manager


-- Helpers that should really be in the base but aren't

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just v) f = f v
whenJust Nothing _ = pure ()
