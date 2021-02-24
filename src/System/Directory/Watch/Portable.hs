module System.Directory.Watch.Portable (
    EventType (..),
    Event (..),
) where


data EventType
    = DirectoryCreated
    | FileCreated
    | FileModified
    deriving (Show, Eq)


data Event = Event
    { eventType :: !EventType
    , filePath :: !FilePath
    }
    deriving (Show, Eq)
