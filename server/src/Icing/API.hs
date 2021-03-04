{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Icing.API
  ( API
  , server
  ) where

import           Control.Concurrent             ( MVar
                                                , modifyMVar
                                                , modifyMVar_
                                                , readMVar
                                                )
import           Control.Exception              ( handle )
import           Control.Monad                  ( forever )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Foldable                  ( for_
                                                , traverse_
                                                )
import           Data.Text                      ( Text )
import           Database.Selda                 ( MonadSelda )
import qualified Network.WebSockets            as WS
import           Servant.API                    ( type (:>) )
import           Servant.API.WebSocket          ( WebSocketPending )
import           Servant.Server                 ( HasServer(..) )

import           Icing.Client                   ( Client(..) )
import           Icing.Message                  ( ByeMessage(..)
                                                , ChangeMessage(..)
                                                , ClearSelectionMessage(..)
                                                , ClientMessage(..)
                                                , ClientSetCursorMessage(..)
                                                , ClientSetSelectionMessage(..)
                                                , HelloMessage(..)
                                                , MessageTarget(..)
                                                , OllehMessage(..)
                                                , OllehUserMessage(..)
                                                , ServerMessage(..)
                                                , SetCursorMessage(..)
                                                , SetSelectionMessage(..)
                                                , UsersMessage(..)
                                                , clientToUser
                                                , parseMessage
                                                , sendMessage
                                                )
import           Icing.Prolog                   ( runPrologWithQuery
                                                , setPrologText
                                                )
import           Icing.State                    ( State
                                                , addClient
                                                , createValidName
                                                , getAllClients
                                                , getAllClientsExcept
                                                , getCurrentRevision
                                                , getCurrentText
                                                , processActions'
                                                , removeClient
                                                , tryPickColour
                                                )

-- | The whole API is just @/stream@ which serves WebSockets.
-- Yes, it's THAT simple!
type API = "stream" :> WebSocketPending

-- | Broadcasts a 'ServerMessage' to every client
broadcastMessage :: MonadIO m => State -> ServerMessage -> m ()
broadcastMessage clients message = liftIO $ do
  for_ (getAllClients clients)
    $ \client -> sendMessage (clientConnection client) message

-- | Broadcasts a 'ServerMessage' to every client except the given one
broadcastMessageExcept :: MonadIO m => State -> Text -> ServerMessage -> m ()
broadcastMessageExcept clients exceptName message = liftIO $ do
  putStrLn $ "broadcasting message: " <> show message
  for_ (getAllClientsExcept clients exceptName)
    $ \client -> sendMessage (clientConnection client) message

-- | Disconnects the given user
disconnect :: MonadIO m => MVar State -> Text -> m ()
disconnect serverStateVar name = liftIO $ do
  state <- readMVar serverStateVar
  modifyMVar_ serverStateVar $ \st -> pure $ removeClient st name
  broadcastMessage state $ BroadcastBye (ByeMessage name)

-- | Runs a computation, catches all relevant exceptions gracefully.
withExceptions :: MVar State -> Text -> IO () -> IO ()
withExceptions serverStateVar name = handle handleCloseRequest
 where
  handleCloseRequest :: MonadIO m => WS.ConnectionException -> m ()
  handleCloseRequest (WS.CloseRequest _i _closeMsg) = do
    liftIO $ putStrLn "Closing!"
    disconnect serverStateVar name
    pure ()
  handleCloseRequest exc = do
    liftIO $ putStrLn "Some exception happened!"
    liftIO $ print exc
    disconnect serverStateVar name
    pure ()

-- | Attempts to intiialize a connection, returns a nickname if everything went well.
initializeConnection
  :: MonadIO m => WS.Connection -> MVar State -> m (Maybe Text)
initializeConnection connection serverStateVar = do
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
            let olleh     = OllehMessage givenName colour
            let completeOlleh = OllehUserMessage olleh
                                                 (getCurrentText state)
                                                 (getCurrentRevision state)

            -- send the respond to the user and add them to clients
            sendMessage connection (RespondOlleh completeOlleh)
            liftIO $ modifyMVar_ serverStateVar $ \st ->
              pure $ addClient st client

            -- send the "user connected" message to all clients
            newState <- liftIO $ readMVar serverStateVar
            broadcastMessageExcept newState givenName $ BroadcastOlleh olleh
            -- liftIO $ putStrLn $ "New state: " <> show newState

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

-- | This takes care of the ordinary "get message, respond to it" loop
loop :: WS.Connection -> MVar State -> Text -> IO ()
loop connection serverStateVar name = do
  liftIO $ putStrLn "loop"
  errOrMsg <- parseMessage connection
  case errOrMsg of
    Left  err -> putStrLn $ "Error: " <> err
    Right msg -> do
      replyMessages <- reply serverStateVar name msg

      case replyMessages of
        [] -> do
          liftIO $ putStrLn $ "[???] Got :" <> show msg
          pure ()
        [(oneMessage, replyTarget)] ->
          sendMessageTo connection serverStateVar oneMessage replyTarget
        moreMessages -> traverse_
          (uncurry (sendMessageTo connection serverStateVar))
          moreMessages

-- | A helpful wrapper which dispatches a 'ServerMessage' with a 'MessageTarget' to a proper sendX funtion
sendMessageTo
  :: MonadIO m
  => WS.Connection
  -> MVar State
  -> ServerMessage
  -> MessageTarget
  -> m ()
sendMessageTo connection serverStateVar oneMessage replyTarget = do
  serverState <- liftIO $ readMVar serverStateVar
  case replyTarget of
    OnlyThis   _    -> sendMessage connection oneMessage
    ExceptThis name -> broadcastMessageExcept serverState name oneMessage
    Broadcast       -> broadcastMessage serverState oneMessage


-- | Replies to a given 'ClientMessage'.
-- 
-- Beware, one 'ClientMessage' may result in more than one 'ServerMessage' as a response!
reply
  :: MonadIO m
  => MVar State
  -> Text
  -> ClientMessage
  -> m [(ServerMessage, MessageTarget)]
reply serverStateVar name msg = do
  serverState <- liftIO $ readMVar serverStateVar
  case msg of
    ClientSendChanges (ChangeMessage clientRev actions) -> do
      (sendMeToClients, newRevision) <-
        liftIO $ modifyMVar serverStateVar $ \st ->
          processActions' st clientRev actions
      pure
        [ ( BroadcastChanges (ChangeMessage newRevision sendMeToClients)
          , ExceptThis name
          )
        , (RespondAck newRevision, OnlyThis name)
        ]
    ClientSetCursor (ClientSetCursorMessage offset) -> pure
      [(BroadcastSetCursor (SetCursorMessage offset name), ExceptThis name)]
    ClientSetSelection (ClientSetSelectionMessage start end) -> pure
      [ ( BroadcastSetSelection (SetSelectionMessage start end name)
        , ExceptThis name
        )
      ]
    ClientClearSelection ->
      pure
        [ ( BroadcastClearSelection (ClearSelectionMessage name)
          , ExceptThis name
          )
        ]
    ClientListUsers -> do
      let allUsers = clientToUser <$> getAllClients serverState
      pure [(RespondUsers (UsersMessage allUsers), OnlyThis name)]
    ClientTerminal queryText -> do
      let currentText = getCurrentText serverState
      setPrologText currentText

      result <- runPrologWithQuery queryText
      pure $ zip [BroadcastTerminal queryText, BroadcastCompilerOutput result]
                 [Broadcast, Broadcast]
    _ -> do
      liftIO $ print msg
      pure []

-- | Serves a small server given by the type of 'API'.
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
          $ withExceptions serverStateVar name
          $ WS.withPingThread connection 10 (pure ())
          $ forever
          $ loop connection serverStateVar name
      Nothing -> pure ()
