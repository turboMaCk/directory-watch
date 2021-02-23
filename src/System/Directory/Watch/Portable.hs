module System.Directory.Watch.Portable (
    EventType (..),
    Event (..),
) where


data EventType
    = DirectoryCreated
    | FileCreated
    deriving (Show, Eq)


data Event = Event
    { eventType :: !EventType
    , filePath :: !FilePath
    }
    deriving (Show, Eq)
