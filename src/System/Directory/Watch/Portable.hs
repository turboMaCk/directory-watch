module System.Directory.Watch.Portable (
    EventType (..),
    Event (..),
) where


data EventType
    = MkDir
    | Touch
    deriving (Show)


data Event = Event
    { eventType :: !EventType
    , filePath :: !FilePath
    }
    deriving (Show)
