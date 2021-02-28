module System.Directory.Watch.Portable (
    ActionType (..),
    Action (..),
    FileType (..),
) where

import System.Posix.ByteString.FilePath (RawFilePath)


data FileType
    = File
    | Directory
    deriving (Show, Eq)


data ActionType
    = Created FileType
    | Modified
    | Removed
    deriving (Show, Eq)


data Action = Action
    { actionType :: !ActionType
    , rawFilePath :: !RawFilePath
    }
    deriving (Show, Eq)
