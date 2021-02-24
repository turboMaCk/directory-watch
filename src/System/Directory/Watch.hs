{-# LANGUAGE RecordWildCards #-}

module System.Directory.Watch (
    Event (..),
    EventType (..),
    Manager,
    withManager,
    watchDirectory,
    watchFile,
    getEvent,
    keepWatching,
    poolSize,
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


type Registery = Map.HashMap Backend.Id (FileType, FilePath)


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


data EventType
    = DirectoryCreated
    | FileCreated
    | FileModified
    | FileRemoved
    | DirectoryRemoved
    deriving (Show, Eq)


data Event = Event
    { eventType :: !EventType
    , filePath :: !FilePath
    }
    deriving (Show, Eq)


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
                    putStrLn $ "[Internal] INotify event: " <> show backendEvent

                    Stm.atomically $ do
                        mWatch <- Map.lookup (Backend.getId backendEvent) <$> Stm.readTVar internalRegMap

                        -- Remove from registery
                        whenJust mWatch $
                            Stm.modifyTVar' registery . Map.delete

                        -- Remove from internal map
                        Stm.modifyTVar' internalRegMap $ Map.delete $ Backend.getId backendEvent

                    regSize <- Map.size <$> Stm.readTVarIO registery
                    intRegSize <- Map.size <$> Stm.readTVarIO internalRegMap
                    putStrLn $ "[Info] Size of registery: " <> show regSize
                    putStrLn $ "[Info] Size of intRegMap: " <> show intRegSize

            action $ Manager{..}


watching :: Manager -> FileType -> FilePath -> Backend.Id -> IO ()
watching Manager{..} fileType path watch = do
    internalWatch <- Backend.internalWatch internalHandle path
    Stm.atomically $ do
        -- Add to registery
        Stm.modifyTVar' registery $ Map.insert watch (fileType, path)
        -- Add to internal reg map
        Stm.modifyTVar' internalRegMap $ Map.insert internalWatch watch


watchDirectory :: Manager -> FilePath -> IO ()
watchDirectory Manager{..} path = do
    putStrLn $ "Watching new dir: " <> path
    Backend.watchDirectory handle path >>= watching Manager{..} Directory path


watchFile :: Manager -> FilePath -> IO ()
watchFile Manager{..} path = do
    putStrLn $ "Watching new file: " <> path
    Backend.watchFile handle path >>= watching Manager{..} File path


getEvent :: Manager -> (Event -> IO ()) -> IO ()
getEvent Manager{..} f = do
    iEvent <- Backend.getEvent handle
    snapshot <- Stm.readTVarIO registery

    let mPath = Map.lookup (Backend.getId iEvent) snapshot
    case mPath of
        Just (ft, path) -> case Backend.toEvent path iEvent of
            Nothing -> putStrLn $ "[Warning] Unknown event" <> show iEvent
            Just Action{..} ->
                let act eventType = f $ Event{..}
                 in case actionType of
                        (Created File) -> act FileCreated
                        (Created Directory) -> act DirectoryCreated
                        Modified -> act FileModified
                        Removed -> case ft of
                            File -> act FileRemoved
                            Directory -> act DirectoryRemoved
        Nothing -> putStrLn $ "[ERROR] can't find path for " <> show iEvent


keepWatching :: Manager -> (Event -> IO ()) -> IO ()
keepWatching manager = forever . getEvent manager


poolSize :: Manager -> IO Int
poolSize Manager{..} =
    Map.size <$> Stm.readTVarIO registery


-- Helpers that should really be in the base but aren't

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just v) f = f v
whenJust Nothing _ = pure ()
