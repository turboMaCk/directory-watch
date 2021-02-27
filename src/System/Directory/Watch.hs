{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Directory.Watch (
    Event (..),
    EventType (..),
    Manager,
    withManager,
    watch,
    watchDirectory,
    watchFile,
    getEvent,
    keepWatching,
    poolSize,
) where

import Control.Concurrent (forkIO)
import qualified Control.Exception as Exception
import Control.Monad (forever, when)
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import GHC.IO.Exception (IOException (..))

import qualified System.Posix.Files as Posix

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
    Exception.bracket Backend.init Backend.close $ \handle ->
        Exception.bracket Backend.init Backend.close $ \internalHandle -> do
            registery <- Stm.newTVarIO Map.empty
            internalRegMap <- Stm.newTVarIO Map.empty

            -- Perform removal of watched directories
            forkIO $
                -- backend handle can be already close
                -- if that's the case we should gracefully exit
                Exception.handle
                    ( \(IOError{..} :: IOException) ->
                        if ioe_description == "Bad file descriptor"
                            then do
                                logD "[Internal] Bad file descriptor handled " IOError{..}
                                pure ()
                            else do
                                logD "[Internal] exception occured" ioe_description
                                Exception.throwIO IOError{..}
                    )
                    $ forever $ do
                        backendEvent <- Backend.getEvent internalHandle
                        logD "[Internal] INotify event: " backendEvent

                        Stm.atomically $ do
                            mWatch <- Map.lookup (Backend.getId backendEvent) <$> Stm.readTVar internalRegMap

                            -- Remove from registery
                            whenJust mWatch $
                                Stm.modifyTVar' registery . Map.delete

                            -- Remove from internal map
                            Stm.modifyTVar' internalRegMap $ Map.delete $ Backend.getId backendEvent
#ifdef Log
                        regSize <- Map.size <$> Stm.readTVarIO registery
                        intRegSize <- Map.size <$> Stm.readTVarIO internalRegMap
                        logD "[Info] Size of registery: " regSize
                        logD "[Info] Size of intRegMap: " intRegSize
#endif

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
    Backend.watchDirectory handle path >>= watching Manager{..} Directory path


watchFile :: Manager -> FilePath -> IO ()
watchFile Manager{..} path = do
    Backend.watchFile handle path >>= watching Manager{..} File path


watch :: Manager -> FilePath -> IO ()
watch manager path = do
    isDir <- Posix.isDirectory <$> Posix.getFileStatus path
    if isDir
        then watchDirectory manager path
        else watchFile manager path


getEvent :: Manager -> (Event -> IO ()) -> IO ()
getEvent Manager{..} f = do
    iEvent <- Backend.getEvent handle
    snapshot <- Stm.readTVarIO registery

    let mPath = Map.lookup (Backend.getId iEvent) snapshot
    case mPath of
        Just (ft, path) -> case Backend.toEvent path iEvent of
            Nothing -> logD "[Warning] Unknown event" iEvent
            Just Action{..} ->
                let act eventType = f $ Event{..}
                 in case actionType of
                        (Created File) -> act FileCreated
                        (Created Directory) -> act DirectoryCreated
                        Modified -> act FileModified
                        Removed -> case ft of
                            File -> act FileRemoved
                            Directory -> act DirectoryRemoved
        Nothing -> logD "[ERROR] can't find path for " iEvent


keepWatching :: Manager -> (Event -> IO ()) -> IO ()
keepWatching manager = forever . getEvent manager


poolSize :: Manager -> IO Int
poolSize Manager{..} =
    Map.size <$> Stm.readTVarIO registery


-- Helpers that should really be in the base but aren't

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just v) f = f v
whenJust Nothing _ = pure ()


{-# INLINE logD #-}
logD :: Show a => String -> a -> IO ()

#ifdef Log
logD desc val =
    putStrLn $ "[[Watcher]] " <> desc <> " " <> show val
#else
logD _ _ =
    pure ()
#endif
