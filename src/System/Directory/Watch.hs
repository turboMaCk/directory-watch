{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

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

import Control.Concurrent (forkIO, threadDelay)
import qualified Control.Exception as Exception
import Control.Monad (forever)
import Data.Functor (void)
import GHC.IO.Exception (IOException (..))

import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.UTF8 as Utf8

import System.Posix.ByteString.FilePath (RawFilePath)
import qualified System.Posix.Files as Posix

import qualified Control.Concurrent.STM as Stm
import qualified Data.HashMap.Strict as Map

import qualified System.Directory.Watch.Backend as Backend
import System.Directory.Watch.Portable


type Registery = Map.HashMap Backend.Id (FileType, RawFilePath)


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
            void $
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


watching :: Manager -> FileType -> RawFilePath -> Backend.Id -> IO ()
watching Manager{..} fileType path wId = do
    internalWatch <- Backend.internalWatch internalHandle path
    Stm.atomically $ do
        -- Add to registery
        Stm.modifyTVar' registery $ Map.insert wId (fileType, path)
        -- Add to internal reg map
        Stm.modifyTVar' internalRegMap $ Map.insert internalWatch wId


watchDirectory :: Manager -> FilePath -> IO ()
watchDirectory Manager{..} (pack -> path) = do
    logD "[Info] watching directory" path
    Backend.watchDirectory handle path >>= watching Manager{..} Directory path


watchFile :: Manager -> FilePath -> IO ()
watchFile Manager{..} (pack -> path) = do
    logD "[Info] watching file" path
    Backend.watchFile handle path >>= watching Manager{..} File path


watch :: Manager -> FilePath -> IO ()
watch manager path = do
    isDir <- Posix.isDirectory <$> Posix.getFileStatus path
    if isDir
        then watchDirectory manager path
        else watchFile manager path


getEvent :: Manager -> (Event -> IO a) -> IO a
getEvent Manager{..} f = do
    iEvent <- Backend.getEvent handle
    snapshot <- Stm.readTVarIO registery

    let mPath = Map.lookup (Backend.getId iEvent) snapshot
    case mPath of
        Just (ft, path) -> case Backend.toEvent path iEvent of
            Nothing -> do
                logD "[Warning] Unknown event" iEvent
                getEvent Manager{..} f
            Just Action{rawFilePath, actionType} ->
                let filePath = Utf8.toString rawFilePath
                    act eventType = f $ Event{..}
                 in case actionType of
                        (Created File) -> act FileCreated
                        (Created Directory) -> act DirectoryCreated
                        Modified -> act FileModified
                        Removed -> case ft of
                            File -> act FileRemoved
                            Directory -> act DirectoryRemoved
        Nothing -> do
            logD "[ERROR] can't find path for " iEvent
            -- give it some time
            threadDelay 100_000
            getEvent Manager{..} f


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
