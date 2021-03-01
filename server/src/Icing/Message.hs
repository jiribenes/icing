{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Icing.Message where

import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as T
import           GHC.Generics
import           Icing.Client
import qualified Network.WebSockets            as WS

data HelloMessage = HelloMessage
  { helloName :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data OllehMessage = OllehMessage
  { ollehName   :: Text
  , ollehColour :: Colour
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data DeleteMessage = DeleteMessage
  { deleteIndex  :: Int
  , deleteLength :: Int
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data InsertMessage = InsertMessage
  { insertIndex :: Int
  , insertValue :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ReplaceMessage = ReplaceMessage
  { replaceIndex  :: Int
  , replaceLength :: Int
  , replaceValue  :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ClientSetCursorMessage = ClientSetCursorMessage
  { clientSetCursorOffset :: Int
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SetCursorMessage = SetCursorMessage
  { setCursorOffset :: Int
  , setCursorName   :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ClientSetSelectionMessage = ClientSetSelectionMessage
  { clientSetSelectionStart :: Int
  , clientSetSelectionEnd   :: Int
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SetSelectionMessage = SetSelectionMessage
  { setSelectionStart :: Int
  , setSelectionEnd   :: Int
  , setSelectionName  :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ClearSelectionMessage = ClearSelectionMessage
  { clearSelectionName  :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ByeMessage = ByeMessage
  { byeName  :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data UsersMessage = UsersMessage
  { users :: [User]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data User = User
  { userName   :: Text
  , userColour :: Colour
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

clientToUser c = User (clientName c) (clientColour c)

data ClientMessage = ClientHello HelloMessage
                   | ClientInsert InsertMessage
                   | ClientDelete DeleteMessage
                   | ClientReplace ReplaceMessage
                   | ClientListUsers
                   | ClientSetCursor ClientSetCursorMessage
                   | ClientSetSelection ClientSetSelectionMessage
                   | ClientClearSelection
                   | ClientCurrentText
                   | ClientCompilerCall
                   | ClientTerminal Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ServerMessage = RespondOlleh OllehMessage
                   | RespondUsers UsersMessage
                   | SendCurrentText Text
                   | BroadcastOlleh OllehMessage
                   | BroadcastInsert InsertMessage
                   | BroadcastDelete DeleteMessage
                   | BroadcastSetCursor SetCursorMessage
                   | BroadcastSetSelection SetSelectionMessage
                   | BroadcastClearSelection ClearSelectionMessage
                   | BroadcastCompilerOutput Text
                   | BroadcastTerminal Text
                   | BroadcastBye ByeMessage
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data MessageTarget = OnlyThis Text | ExceptThis Text | Broadcast | None
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

sendMessage :: MonadIO m => WS.Connection -> ServerMessage -> m ()
sendMessage connection message =
  liftIO $ WS.sendTextData connection $ encode message

sendMessages :: MonadIO m => WS.Connection -> [ServerMessage] -> m ()
sendMessages connection messages =
  liftIO $ WS.sendTextDatas connection $ encode <$> messages

parseMessage :: MonadIO m => WS.Connection -> m (Either String ClientMessage)
parseMessage connection = do
  msg <- liftIO $ WS.receiveData connection
  pure $ eitherDecode msg
