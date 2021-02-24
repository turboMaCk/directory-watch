module System.Directory.Watch.Portable (
    ActionType (..),
    Action (..),
    FileType (..),
) where


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
    , filePath :: !FilePath
    }
    deriving (Show, Eq)
