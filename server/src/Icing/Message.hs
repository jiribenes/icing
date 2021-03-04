{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | This module contains a whole lot of different messages,
-- which are used to communicate between this server and the clients.
--
-- The individual messages aren't really documented, sorry.
module Icing.Message where

import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                , eitherDecode
                                                , encode
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import qualified Network.WebSockets            as WS

import           Icing.Client                   ( Client(..)
                                                , Colour
                                                )

data InsertAction = InsertAction
  { insertPosition :: Int
  , insertText     :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data DeleteAction = DeleteAction
  { deletePosition :: Int
  , deleteLen      :: Int
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Represents some action (a sum type for 'InsertAction' and 'DeleteAction'
data SomeAction = ActionInsert InsertAction | ActionDelete DeleteAction
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data User = User
  { userName   :: Text
  , userColour :: Colour
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Simply convert from internal 'Client' to external 'User'.
--
-- This is in order to skip the connection which is present in 'Client'.
-- Damn, I wish we had row polymorphism...
clientToUser :: Client -> User
clientToUser c = User (clientName c) (clientColour c)

---------------
--- MESSAGES:
---------------

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

data OllehUserMessage = OllehUserMessage
  { ollehMessage     :: OllehMessage
  , ollehCurrentText :: Text
  , ollehRevision    :: Int
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
  { clearSelectionName :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ByeMessage = ByeMessage
  { byeName :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data UsersMessage = UsersMessage
  { users :: [User]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ChangeMessage = ChangeMessage
  { changeRevision :: Int
  , changeActions  :: [SomeAction]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Messages _recieved_ from the client by the server
data ClientMessage = ClientHello HelloMessage
                   | ClientSendChanges ChangeMessage
                   | ClientListUsers
                   | ClientSetCursor ClientSetCursorMessage
                   | ClientSetSelection ClientSetSelectionMessage
                   | ClientClearSelection
                   | ClientCurrentText
                   | ClientCompilerCall
                   | ClientTerminal Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Messages _sent_ from the server to the clients
--
-- RespondXXX just respond to a single client
-- BroadcastXXX is sent either to all clients or all clients except one
-- SendXXX is an unexpected one-sided message from the server
data ServerMessage = RespondOlleh OllehUserMessage
                   | RespondUsers UsersMessage
                   | RespondAck Int -- ^ ACK changes, send new revision no.
                   | SendCurrentText Text
                   | BroadcastOlleh OllehMessage
                   | BroadcastChanges ChangeMessage
                   | BroadcastSetCursor SetCursorMessage
                   | BroadcastSetSelection SetSelectionMessage
                   | BroadcastClearSelection ClearSelectionMessage
                   | BroadcastCompilerOutput Text
                   | BroadcastTerminal Text
                   | BroadcastBye ByeMessage
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Who is a message sent to?
data MessageTarget = OnlyThis Text | ExceptThis Text | Broadcast
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Sends a single 'ServerMessage' through a 'WS.Connection'
sendMessage :: MonadIO m => WS.Connection -> ServerMessage -> m ()
sendMessage connection message =
  liftIO $ WS.sendTextData connection $ encode message

-- | Sends multiple 'ServerMessage' through a 'WS.Connection'
sendMessages :: MonadIO m => WS.Connection -> [ServerMessage] -> m ()
sendMessages connection messages =
  liftIO $ WS.sendTextDatas connection $ encode <$> messages

-- | Parses a 'ClientMessage' from a 'WS.Connection'
parseMessage :: MonadIO m => WS.Connection -> m (Either String ClientMessage)
parseMessage connection = do
  msg <- liftIO $ WS.receiveData connection
  pure $ eitherDecode msg
