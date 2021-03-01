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

data State = State
  { stateClients     :: [Client]
  , stateAllChanges  :: [ServerMessage]
  , stateCurrentText :: Text
  }
  deriving stock Show

makeState :: MonadIO m => m (MVar State)
makeState = liftIO $ newMVar (State [] [] "% write here")

addClient :: State -> Client -> State
addClient st client = st { stateClients = client : oldClients }
  where oldClients = stateClients st

removeClient :: State -> Text -> State
removeClient st name = st
  { stateClients = filter (\c -> clientName c /= name) oldClients
  }
  where oldClients = stateClients st

addChange :: State -> ServerMessage -> State
addChange st change
  | isChangeRelevant = st { stateAllChanges  = change : oldChanges
                          , stateCurrentText = applyChange change oldText
                          }
  | otherwise = st
 where
  oldChanges       = stateAllChanges st
  oldText          = stateCurrentText st
  isChangeRelevant = case change of
    BroadcastInsert _ -> True
    BroadcastDelete _ -> True
    _                 -> False
  applyChange (BroadcastInsert (InsertMessage i val)) text =
    let (l, r) = T.splitAt i text in l <> val <> r
  applyChange (BroadcastDelete (DeleteMessage i len)) text =
    let (l, r) = T.splitAt i text in l <> T.drop len r
  applyChange _ text = text

getAllChanges :: State -> [ServerMessage]
getAllChanges = reverse . stateAllChanges

getClientByName :: State -> Text -> Maybe Client
getClientByName state name =
  find (\c -> clientName c == name) $ stateClients state

getAllClientsExcept :: State -> Text -> [Client]
getAllClientsExcept state name =
  filter (\c -> clientName c /= name) $ stateClients state

getAllClients :: State -> [Client]
getAllClients = stateClients

getCurrentText :: State -> Text
getCurrentText = stateCurrentText

usedNames :: State -> Set Text
usedNames state = Set.fromList $ clientName <$> stateClients state

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
