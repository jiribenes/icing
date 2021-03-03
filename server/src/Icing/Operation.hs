{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Icing.Operation
  ( Action(..)
  , Operation
  , applyAction
  , transformAction
  , Revision
  , OperationState(..)
  , applyOperation
    -- * for testing purposes
  , testState
  , process
  , testDoc
  , one
  , two
  , oneDoc
  , twoDoc
  ) where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans.Except
import           Data.Foldable
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Tuple                     ( swap )

data Action = Insert Int Text
            | Delete Int Int
  deriving stock (Show, Eq, Ord)

type Operation = [Action]

--  TODO: do a bounds check
applyAction :: Action -> Text -> Text
applyAction action doc = case action of
  Insert i t -> let (start, end) = T.splitAt i doc in start <> t <> end
  Delete i l -> let (start, end) = T.splitAt i doc in start <> T.drop l end

shiftIndexBy :: Action -> Int -> Action
shiftIndexBy (Insert i t) j = Insert (i + j) t
shiftIndexBy (Delete i l) j = Delete (i + j) l

getIndex :: Action -> Int
getIndex (Insert i _) = i
getIndex (Delete i _) = i

todo :: a
todo = error "todo"

-- Takes actions @a@, @b@.
-- If we denote the transformation as @f@, satisfies @f b \circ a === f a \circ b@ 
--
-- Most of the operations are from [reduce98]
transformAction :: Action -> Action -> ([Action], [Action])
transformAction a b = case (a, b) of
  (Insert i ta, Insert j tb) -> case compare i j of
    LT -> ([Insert i ta], [Insert (j + T.length ta) tb])
    EQ -> ([Insert i ta], [Insert (j + T.length ta) tb])
    GT -> ([Insert (i + T.length tb) ta], [Insert j tb])
  (Insert i ta, Delete j lb)
    | i <= j
    -> ([Insert i ta], [Delete (j + T.length ta) lb])
    | i > j + lb
    -> ([Insert (i - lb) ta], [Delete j lb])
    | otherwise
    -> ( [Insert j ta] -- we might lose some info here
       , [Delete j (i - j), Delete (i + T.length ta) (lb - (i - j))]
       )
  (Delete _ _ , Insert _ _ ) -> swap $ transformAction b a -- symmetric?
  (Delete i la, Delete j lb) -> ([dd i la j lb], [dd j lb i la]) -- the interaction of these is complicated, let's write a helpful lemma
 where
  -- loses information from third rule onwards!
  dd i la j lb | j >= (i + la)                  = Delete i la
               | i >= (j + lb)                  = Delete (i - lb) la
               | j <= i && (i + la) <= (j + lb) = Delete i 0
               | j <= i && (i + la) > (j + lb)  = Delete j (i + la - (j + lb))
               | j > i && (j + lb) >= (i + la)  = Delete i (j - i)
               | otherwise                      = Delete i (la - lb)



transformOperation :: Operation -> Operation -> (Operation, Operation)
transformOperation xs ys = help $ go xs ys
 where
  go :: [Action] -> [Action] -> [([Action], [Action])]
  go []       []       = [([], [])]
  go as       []       = [(as, [])]
  go []       bs       = [([], bs)]
  go (a : as) (b : bs) = transformAction a b : go as bs

  help :: [([a], [b])] -> ([a], [b])
  help zs = (zs >>= fst, zs >>= snd)

type Revision = Int

data OperationState = OperationState
  { serverRev :: Revision
  , serverDoc :: Text
  , serverOps :: [Operation]
  }
  deriving stock (Eq, Show, Ord)

type ServerOpState = (Int, Text, [Operation])

testState :: ServerOpState
testState = (1, "ahojky", [])

runError :: Except e a -> Either e a
runError = runExcept

applyOperation
  :: OperationState
  -> Revision -- ^ last revision that the client saw
  -> Operation -- ^ operation recieved from client
  -> Either String (Operation, OperationState)
applyOperation operationState clientRev clientOp = runError $ do
  let numberOfRevs = serverRev operationState - clientRev
  concurrentOps :: [Operation] <-
    if clientRev > serverRev operationState || numberOfRevs > length
         (serverOps operationState)
      then throwError "Invalid revision number!"
      else pure $ take numberOfRevs (serverOps operationState)
  let megaOp :: Operation = concat $ reverse concurrentOps
  let op'                 = fst $ transformOperation clientOp megaOp
  let serverDoc'          = applyAllActions (serverDoc operationState) op'
  let operationState' = OperationState
        { serverRev = serverRev operationState + 1
        , serverDoc = serverDoc'
        , serverOps = op' : serverOps operationState
        }
  pure (op', operationState')

-- testing data:
--
testDoc :: Text
testDoc = "ahojky"

one :: Action
one = Insert 2 "aaa"

two :: Action
two = Insert 2 "bbb"

oneDoc = applyAction one testDoc
twoDoc = applyAction two testDoc

applyAllActions :: Text -> [Action] -> Text
applyAllActions = foldl' (flip applyAction)

process :: Action -> Action -> Text -> IO ()
process a b doc = do
  putStrLn "initial document:"
  print doc
  putStrLn ""
  putStrLn "Action a:"
  print a
  putStrLn ""
  putStrLn "a's document:"
  print aDoc
  putStrLn ""
  putStrLn "Action b:"
  print b
  putStrLn ""
  putStrLn "b's document:"
  print bDoc
  putStrLn ""
  putStrLn "------------------"
  putStrLn "b's changes for a:"
  print b'
  putStrLn ""
  putStrLn "a's final document:"
  print $ applyAllActions aDoc b'
  putStrLn ""
  putStrLn "a's changes for b:"
  print a'
  putStrLn ""
  putStrLn "b's final document:"
  print $ applyAllActions bDoc a'
  putStrLn ""
 where
  (a', b') = transformAction a b
  aDoc     = applyAction a doc
  bDoc     = applyAction b doc

