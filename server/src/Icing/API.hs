{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Icing.API
  ( API
  , server
  , State
  ) where

import           Control.Concurrent
import           Control.Exception              ( handle )
import           Control.Monad                  ( forever
                                                , unless
                                                )
import           Control.Monad.IO.Class
import           Data.Foldable
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as T
import           Database.Selda
import qualified Network.WebSockets            as WS
import           Servant.API
import           Servant.API.WebSocket
import           Servant.Server

import           Icing.Client
import           Icing.Message
import           Icing.Prolog
import           Icing.State

type API = "stream" :> WebSocketPending

broadcastMessage :: MonadIO m => State -> ServerMessage -> m ()
broadcastMessage clients message = liftIO $ do
  putStrLn $ "broadcasting message: " <> show message
  for_ (getAllClients clients)
    $ \client -> sendMessage (clientConnection client) message

broadcastMessages :: MonadIO m => State -> [ServerMessage] -> m ()
broadcastMessages clients messages = liftIO $ do
  putStrLn $ "broadcasting messages: " <> show messages
  for_ (getAllClients clients)
    $ \client -> sendMessages (clientConnection client) messages

broadcastMessageExcept :: MonadIO m => State -> Text -> ServerMessage -> m ()
broadcastMessageExcept clients exceptName message = liftIO $ do
  putStrLn $ "broadcasting message: " <> show message
  for_ (getAllClientsExcept clients exceptName)
    $ \client -> sendMessage (clientConnection client) message

broadcastMessagesExcept
  :: MonadIO m => State -> Text -> [ServerMessage] -> m ()
broadcastMessagesExcept clients exceptName messages = liftIO $ do
  putStrLn $ "broadcasting messages: " <> show messages
  for_ (getAllClientsExcept clients exceptName)
    $ \client -> sendMessages (clientConnection client) messages

-- TODO: Catch 'CloseRequest' exception!
handleCloseRequest serverStateVar _ name (WS.CloseRequest i closeMsg) = do
  putStrLn "Closing!"
  putStrLn $ show i
  putStrLn $ show closeMsg
  disconnect serverStateVar name
  pure ()
handleCloseRequest serverStateVar _ name exc = do
  putStrLn "Some exception!"
  liftIO $ print exc
  disconnect serverStateVar name
  pure ()

disconnect serverStateVar name = liftIO $ do
  state <- readMVar serverStateVar
  modifyMVar_ serverStateVar $ \st -> pure $ removeClient st name
  broadcastMessage state $ BroadcastBye (ByeMessage name)

withExceptions serverStateVar connection name = handle $ handleCloseRequest serverStateVar connection name

initializeConnection
  :: MonadIO m => WS.Connection -> MVar State -> m (Maybe Text)
initializeConnection connection serverStateVar = do
  clients <- liftIO $ readMVar serverStateVar
  liftIO $ putStrLn $ "New connection!: Clients so far: " <> show clients

  errOrFirstMsg <- parseMessage connection
  case errOrFirstMsg of
    Left err -> do
      liftIO $ putStrLn $ "Error: " <> show err
      pure Nothing
    Right xs -> case xs of
      ClientHello (HelloMessage wantedName) -> do
        state       <- liftIO $ readMVar serverStateVar
        maybeColour <- tryPickColour state
        case maybeColour of
          Just colour -> do
            let client = Client wantedName colour connection
            liftIO $ print client

            let givenName = createValidName state wantedName

            -- response
            let olleh = OllehMessage givenName colour

            -- send the respond to the user and add them to clients
            sendMessage connection (RespondOlleh olleh)
            liftIO $ modifyMVar_ serverStateVar $ \st ->
              pure $ addClient st client

            -- send the "user connected" message to all clients
            newState <- liftIO $ readMVar serverStateVar
            broadcastMessageExcept newState givenName $ BroadcastOlleh olleh
            liftIO $ putStrLn $ "New state: " <> show newState

            -- send current text so that the user can catch up!
            let currentText = getCurrentText newState
            sendMessage connection (SendCurrentText currentText)

            pure $ Just givenName
          Nothing -> do
            liftIO $ WS.sendClose connection ("Not enough colours!" :: Text)
            liftIO $ putStrLn "Closing because: not enough colours!"
            pure Nothing
      _ -> do
        liftIO $ WS.sendClose connection ("Expected hello!" :: Text)
        liftIO $ putStrLn "Closing because: expected hello!"
        pure Nothing

loop connection serverStateVar name = do
  liftIO $ putStrLn "loop"
  errOrMsg <- parseMessage connection
  case errOrMsg of
    Left  err -> putStrLn $ "Error: " <> err
    Right msg -> do
      print msg
      serverState                  <- readMVar serverStateVar
      (replyMessages, replyTarget) <- reply serverStateVar name msg
      modifyMVar_ serverStateVar
        $ \st -> pure $ foldl' addChange st replyMessages
      case replyMessages of
        [] -> do
          liftIO $ putStrLn $ "[???] Got :" <> show msg
          pure ()
        [oneMessage] ->
          sendMessageTo connection serverStateVar oneMessage replyTarget
        moreMessages ->
          sendMessagesTo connection serverStateVar moreMessages replyTarget

sendMessageTo connection serverStateVar oneMessage replyTarget = do
  serverState <- liftIO $ readMVar serverStateVar
  case replyTarget of
    OnlyThis   _    -> sendMessage connection oneMessage
    ExceptThis name -> broadcastMessageExcept serverState name oneMessage
    Broadcast       -> broadcastMessage serverState oneMessage
    None            -> pure ()

sendMessagesTo connection serverStateVar moreMessages replyTarget = do
  serverState <- liftIO $ readMVar serverStateVar
  case replyTarget of
    OnlyThis   _    -> sendMessages connection moreMessages
    ExceptThis name -> broadcastMessagesExcept serverState name moreMessages
    Broadcast       -> broadcastMessages serverState moreMessages
    None            -> pure ()

reply
  :: MonadIO m
  => MVar State
  -> Text
  -> ClientMessage
  -> m ([ServerMessage], MessageTarget)
reply serverStateVar name msg = do
  serverState <- liftIO $ readMVar serverStateVar
  case msg of
    ClientInsert ins -> pure ([BroadcastInsert ins], ExceptThis name)
    ClientDelete del -> pure ([BroadcastDelete del], ExceptThis name)
    ClientReplace (ReplaceMessage i len val) -> do
      let messages =
            [ BroadcastDelete (DeleteMessage i len)
            , BroadcastInsert (InsertMessage i val)
            ]
      pure (messages, ExceptThis name)
    ClientSetCursor (ClientSetCursorMessage offset) -> pure
      ([BroadcastSetCursor (SetCursorMessage offset name)], ExceptThis name)
    ClientSetSelection (ClientSetSelectionMessage start end) -> pure
      ( [BroadcastSetSelection (SetSelectionMessage start end name)]
      , ExceptThis name
      )
    ClientClearSelection ->
      pure
        ( [BroadcastClearSelection (ClearSelectionMessage name)]
        , ExceptThis name
        )
    ClientListUsers -> do
      let allUsers = clientToUser <$> getAllClients serverState
      pure ([RespondUsers (UsersMessage allUsers)], OnlyThis name)
    ClientCurrentText -> do
      let currentText = getCurrentText serverState
      pure ([SendCurrentText currentText], OnlyThis name)
    ClientTerminal t -> do
      let currentText = getCurrentText serverState
      setPrologText currentText

      result <- runPrologWithCommand t
      pure ([BroadcastTerminal t, BroadcastCompilerOutput result], Broadcast)
    _ -> pure ([], None)


server :: MonadSelda m => MVar State -> ServerT API m
server serverStateVar = streamData
 where
  streamData :: MonadIO m => WS.PendingConnection -> m ()
  streamData pendingConnection = do
    connection      <- liftIO $ WS.acceptRequest pendingConnection

    maybeClientName <- initializeConnection connection serverStateVar
    case maybeClientName of
      Just name -> do
        liftIO
          $ withExceptions serverStateVar connection name
          $ WS.withPingThread connection 10 (pure ())
          $ forever
          $ loop connection serverStateVar name
      Nothing -> pure ()
