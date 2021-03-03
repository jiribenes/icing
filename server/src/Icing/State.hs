{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Icing.State where

import           Control.Concurrent.MVar        ( MVar
                                                , newMVar
                                                )
import           Control.Monad.IO.Class
import           Data.List                      ( find )
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           System.Random

import           Icing.Client
import           Icing.Message
import           Icing.Operation

data State = State
  { stateClients        :: [Client]
  , stateOperationState :: OperationState
  }
  deriving stock Show

makeState :: MonadIO m => m (MVar State)
makeState = do
  let initialOperationState = OperationState 1 "% write here" []
  liftIO $ newMVar (State [] initialOperationState)

addClient :: State -> Client -> State
addClient st client = st { stateClients = client : oldClients }
  where oldClients = stateClients st

removeClient :: State -> Text -> State
removeClient st name = st
  { stateClients = filter (\c -> clientName c /= name) oldClients
  }
  where oldClients = stateClients st

getClientByName :: State -> Text -> Maybe Client
getClientByName state name =
  find (\c -> clientName c == name) $ stateClients state

getAllClientsExcept :: State -> Text -> [Client]
getAllClientsExcept state name =
  filter (\c -> clientName c /= name) $ stateClients state

getAllClients :: State -> [Client]
getAllClients = stateClients

usedNames :: State -> Set Text
usedNames state = Set.fromList $ clientName <$> stateClients state

processActions
  :: State -> Revision -> [SomeAction] -> Either String ([SomeAction], State)
processActions state revision actions = do
  let operation = someActionToAction <$> actions
  (op', operationState') <- applyOperation (stateOperationState state)
                                           revision
                                           operation
  let actions' = actionToSomeAction <$> op'
  let state'   = state { stateOperationState = operationState' }
  pure (actions', state')

processActions'
  :: MonadIO m
  => State
  -> Revision
  -> [SomeAction]
  -> m (State, ([SomeAction], Revision))
processActions' st rev actions = case processActions st rev actions of
  Left err -> do
    liftIO $ putStrLn "error happened while processing action:"
    liftIO $ putStrLn err
    pure (st, ([], serverRev $ stateOperationState st))
  Right (actions', st') ->
    pure (st', (actions', serverRev $ stateOperationState st'))

getCurrentText :: State -> Text
getCurrentText = serverDoc . stateOperationState

getCurrentRevision :: State -> Revision
getCurrentRevision = serverRev . stateOperationState

createValidName :: State -> Text -> Text
createValidName st wantedUsername
  | wantedUsername `Set.notMember` usedNames st = wantedUsername
  | otherwise = go st wantedUsername 1
 where
  go state username i
    | makeUsername username i `Set.notMember` usedNames state = makeUsername
      username
      i
    | otherwise = go state username (i + 1)
  makeUsername :: Text -> Int -> Text
  makeUsername username i = username <> " " <> T.pack (show i)

usedColours :: State -> Set Colour
usedColours state = Set.fromList $ clientColour <$> stateClients state

freeColours :: State -> Set Colour
freeColours state = Set.difference darkPallete $ usedColours state

tryPickColour :: MonadIO m => State -> m (Maybe Colour)
tryPickColour state
  | null free = pure Nothing
  | otherwise = do
    randomIndex <- liftIO $ getStdRandom (randomR (0, length free - 1))
    pure $ Just (free !! randomIndex)
  where free = Set.toList $ freeColours state


someActionToAction :: SomeAction -> Action
someActionToAction (ActionInsert ins) =
  Insert (insertPosition ins) (insertText ins)
someActionToAction (ActionDelete del) =
  Delete (deletePosition del) (deleteLen del)

actionToSomeAction :: Action -> SomeAction
actionToSomeAction (Insert i t) = ActionInsert $ InsertAction i t
actionToSomeAction (Delete i l) = ActionDelete $ DeleteAction i l
