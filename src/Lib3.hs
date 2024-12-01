{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
    genStatements,
    testRenderParse
    ) where

import Control.Concurrent (Chan, readChan, writeChan, newChan)
import Data.Maybe (isNothing)
import Control.Concurrent.STM(STM, TVar, atomically, readTVar, writeTVar, readTVarIO)
import qualified Lib2
import Control.Exception (IOException, catch)
import System.IO (openFile, IOMode(ReadMode), hClose)
import Data.List (isSuffixOf, isPrefixOf, intersperse, partition)
import Data.Char (toLower)
import Control.Monad (forever)
import Test.QuickCheck 
import Control.Concurrent.STM.TVar


data StorageOp = Save String (Chan ()) | Load (Chan String)
-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop opChan = forever $ do
  op <- readChan opChan
  case op of

    Save content replyChan -> do
      writeFile "state.txt" content
      writeChan replyChan ()

    Load replyChan -> do
      fileExists <- fileExists "state.txt"
      if fileExists
        then do
          content <- readFile "state.txt"
          writeChan replyChan content
        else
          writeChan replyChan ""

fileExists :: FilePath -> IO Bool
fileExists path = catch (do
  handle <- openFile path ReadMode
  hClose handle
  return True) handleException
  where
    handleException :: IOException -> IO Bool
    handleException _ = return False

data Statements = Batch [Lib2.Query]
                | Single Lib2.Query
                  deriving (Show, Eq)

data Command = StatementCommand Statements
             | LoadCommand
             | SaveCommand
               deriving (Show, Eq)

-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
parseCommand input
    | "load" `isPrefixOf` input = Right (LoadCommand, dropWhile (== ' ') (drop 4 input))
    | "save" `isPrefixOf` input = Right (SaveCommand, dropWhile (== ' ') (drop 4 input))
    | otherwise = fmap (first StatementCommand) (parseStatements input)
  where
    first f (x, y) = (f x, y)

-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: String -> Either String (Statements, String)
parseStatements input
    | null input = Left "Empty input"
    | "BEGIN" `isPrefixOf` input = parseBatch input
    | otherwise = parseSingle input
  where
    parseBatch :: String -> Either String (Statements, String)
    parseBatch batchInput =
        let trimmedInput = reverse (dropWhile (\c -> c == ' ' || c == '\n') (reverse (drop 5 batchInput)))
        in if "END" `isSuffixOf` trimmedInput
           then
               let inputWithoutEnd = reverse (drop 3 (reverse trimmedInput))
                   queries = splitBy ';' inputWithoutEnd
                   nonEmptyQueries = if length queries > 1 then init queries else queries                   
                   parsedQueries = map Lib2.parseQuery nonEmptyQueries
                   successfulQueries = rights parsedQueries
                   failedQueries = lefts parsedQueries
               in if all isRight parsedQueries
                  then Right (Batch (map fst successfulQueries), "")
                  else Left $ "Parsing error in queries: " ++ show failedQueries
           else Left "Missing END"

    parseSingle :: String -> Either String (Statements, String)
    parseSingle inp =
        case Lib2.parseQuery inp of
            Right (query, rest) -> Right (Single query, rest)
            Left _ -> Left "Parsing error"

-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.State -> Statements
marshallState (Lib2.State orders _) =
    let queries = map (uncurry toQuery) orders
    in if length queries == 1
       then Single (head queries)
       else Batch queries

toQuery :: Int -> Lib2.Order -> Lib2.Query
toQuery _ order = Lib2.MakeOrder [order]

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements (Single query) = renderQuery query
renderStatements (Batch queries) = 
    let (makeOrders, otherQueries) = partition isMakeOrder queries
        combinedMakeOrder = Lib2.MakeOrder (concatMap getOrders makeOrders)
    in unlines (renderQuery combinedMakeOrder : map renderQuery otherQueries)

renderQuery :: Lib2.Query -> String
renderQuery (Lib2.MakeOrder orders) = unwords (map renderOrder orders)
renderQuery Lib2.ListOrders = "list orders"
renderQuery (Lib2.CancelOrder orderId) = "cancel order " ++ show orderId

renderOrder :: Lib2.Order -> String
renderOrder order = removeMultipleSpaces (unwords
    [ "make"
    , renderCoffeeAndFilterType (Lib2.orderCoffeeType order)
    , renderShotCount order
    , renderMilkAndExtras order
    , renderCupType (Lib2.orderCupType order)
    ] )


-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- State update must be executed atomically (STM).
-- Right contains an optional message to print, updated state
-- is stored in transactinal variable
stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp ->
                   IO (Either String (Maybe String))
stateTransition stateVar command ioChan = case command of

  -- Handle StatementCommand (Single or Batch)
  StatementCommand statements -> atomically $ do
    currentState <- readTVar stateVar
    let result = processStatements currentState statements
    case result of
      Left err -> return $ Left err
      Right (msgs, newState) -> do
        writeTVar stateVar newState
        return $ Right (Just (unlines msgs))

  -- Handle LoadCommand
  LoadCommand -> do
    replyChan <- newChan
    writeChan ioChan (Load replyChan)
    content <- readChan replyChan
    case parseStatements content of
      Right (statements, _) -> atomically $ do
        currentState <- readTVar stateVar
        let result = processStatements currentState statements
        case result of
          Left err -> return $ Left err
          Right (_, newState) -> do
            writeTVar stateVar newState
            return $ Right (Just "State loaded")
      Left err -> return $ Left err

  -- Handle SaveCommand
  SaveCommand -> do
    clearStateFile
    currentState <- readTVarIO stateVar
    let content = "BEGIN\n" ++ renderStatements (marshallState currentState) ++ "END"
    replyChan <- newChan
    writeChan ioChan (Save content replyChan)
    _ <- readChan replyChan
    return $ Right (Just "State saved")

-- | Helper function to process a batch or single statement
processStatements :: Lib2.State -> Statements -> Either String ([String], Lib2.State)
processStatements state (Single query) =
    case Lib2.stateTransition state query of
        Left err -> Left err
        Right (msg, newState) -> Right (maybeToList msg, newState)

processStatements state (Batch queries) = 
    foldl processQuery (Right ([], state)) queries
  where
    processQuery :: Either String ([String], Lib2.State) -> Lib2.Query -> Either String ([String], Lib2.State)
    processQuery (Left err) _ = Left err
    processQuery (Right (msgs, st)) query = 
        case Lib2.stateTransition st query of
            Left err -> Left err
            Right (msg, newState) -> Right (msgs ++ maybeToList msg, newState)

------------------------
-- | HELPER METHODS | --
------------------------

isMakeOrder :: Lib2.Query -> Bool
isMakeOrder (Lib2.MakeOrder _) = True
isMakeOrder _ = False

getOrders :: Lib2.Query -> [Lib2.Order]
getOrders (Lib2.MakeOrder orders) = orders
getOrders _ = []

-- | Converts Maybe String to [String]
maybeToList :: Maybe String -> [String]
maybeToList Nothing = []
maybeToList (Just s) = [s]

-- | Function to remove multiple spaces from a string
removeMultipleSpaces :: String -> String
removeMultipleSpaces [] = []
removeMultipleSpaces (x:xs)
    | x == ' ' && head xs == ' ' = removeMultipleSpaces xs
    | otherwise = x : removeMultipleSpaces xs

-- | Function to render coffee and filter type
renderCoffeeAndFilterType :: Lib2.CoffeeType -> String
renderCoffeeAndFilterType Lib2.Espresso = "espresso "
renderCoffeeAndFilterType (Lib2.Filter filterCoffee) = "filter coffee " ++
    case filterCoffee of
        Lib2.Aeropress -> "using aeropress "
        Lib2.Chemex -> "using chemex "
        _ -> "using v60 "

-- | Function to render shot count
renderShotCount :: Lib2.Order -> String
renderShotCount order = if Lib2.orderShots order > 1
                        then "with " ++ show (Lib2.orderShots order) ++ " shots"
                        else ""

-- | Function to render milk and extras
renderMilkAndExtras :: Lib2.Order -> String
renderMilkAndExtras order
  | null (Lib2.orderExtras order) = showMilkType (Lib2.orderMilkType order)
  | isNothing (Lib2.orderMilkType order) = "with " ++ showExtras (Lib2.orderExtras order)
  | otherwise = showMilkType (Lib2.orderMilkType order) ++ " and " ++ showExtras (Lib2.orderExtras order)
  where
      showMilkType :: Maybe Lib2.MilkType -> String
      showMilkType (Just Lib2.Milk) = "with milk "
      showMilkType (Just milkType) = "with " ++ decapitalize (show milkType) ++ " milk "
      showMilkType Nothing = ""

      showExtras :: [Lib2.Extra] -> String
      showExtras extras = unwords $ intersperse "and" (map f extras)
        where
          f :: Lib2.Extra -> String
          f Lib2.Sugar = "sugar"
          f (Lib2.Syrup Lib2.SaltedCaramel) = "salted caramel syrup"
          f (Lib2.Syrup Lib2.Strawberry) = "strawberry syrup"
          f (Lib2.Syrup Lib2.Coconut) = "coconut syrup"
          f (Lib2.Syrup Lib2.Hazelnut) = "hazelnut syrup"
          f (Lib2.Syrup Lib2.Maple) = "maple syrup"

-- | Function to render cup type
renderCupType :: Lib2.CupType -> String
renderCupType Lib2.Takeout = "for takeout;"
renderCupType Lib2.SitIn = "for sitting in;"

-- | Decapitalizes the first character of a string
decapitalize :: String -> String
decapitalize [] = []
decapitalize (x:xs) = toLower x : xs

-- | Checks if the input is a Right value (parsed successfully)
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

-- | Collects all successful queries
rights :: [Either a (b, c)] -> [(b, c)]
rights = foldr (\x acc -> case x of Right v -> v : acc; _ -> acc) []

lefts :: [Either a b] -> [a]
lefts = foldr (\x acc -> case x of Left v -> v : acc; _ -> acc) []

-- | Custom split function
splitBy :: Char -> String -> [String]
splitBy delimiter = foldr f [[]]
  where
    f _ [] = error "This should never happen"
    f c l@(x:xs)
      | c == delimiter = [] : l
      | otherwise = (c : x) : xs

-- Function to clear the state.txt file
clearStateFile :: IO ()
clearStateFile = writeFile "state.txt" ""

------------------------
-- | PROPERTY TESTS | --
------------------------

-- Generator for Statements
genStatements :: Gen Statements
genStatements = Batch <$> listOf genQuery

-- Generator for Query
genQuery :: Gen Lib2.Query
genQuery = Lib2.MakeOrder <$> listOf genOrder

-- Generator for Order
genOrder :: Gen Lib2.Order
genOrder = do
  coffeeType <- genCoffeeType
  milkType <- genMilkType
  shots <- if isFilterCoffee coffeeType then return 0 else choose (1, 3)
  cupType <- genCupType
  extras <- resize 2 (listOf genExtra)
  return $ Lib2.Order coffeeType milkType shots cupType extras

-- Helper function to check if the coffee type is filter coffee
isFilterCoffee :: Lib2.CoffeeType -> Bool
isFilterCoffee (Lib2.Filter _) = True
isFilterCoffee _ = False

-- Generator for CoffeeType
genCoffeeType :: Gen Lib2.CoffeeType
genCoffeeType = elements [Lib2.Espresso, Lib2.Filter Lib2.Aeropress, Lib2.Filter Lib2.Chemex, Lib2.Filter Lib2.V60]

-- Generator for MilkType
genMilkType :: Gen (Maybe Lib2.MilkType)
genMilkType = elements [Just Lib2.Whole, Just Lib2.Skim, Just Lib2.Soy, Just Lib2.Almond, Just Lib2.Oat, Just Lib2.Milk, Nothing]

-- Generator for CupType
genCupType :: Gen Lib2.CupType
genCupType = elements [Lib2.Takeout, Lib2.SitIn]

-- Generator for Extra
genExtra :: Gen Lib2.Extra
genExtra = elements [Lib2.Sugar, Lib2.Syrup Lib2.SaltedCaramel, Lib2.Syrup Lib2.Strawberry, Lib2.Syrup Lib2.Coconut, Lib2.Syrup Lib2.Hazelnut, Lib2.Syrup Lib2.Maple]

-- Test for render/parse round-trip
testRenderParse :: IO ()
testRenderParse = do
  statements <- generate genStatements
  let rendered = "BEGIN\n" ++ renderStatements statements ++ "END"
  let parsed = parseStatements rendered
  case parsed of
    Right (parsedStatements, _) -> 
      if renderStatements parsedStatements == renderStatements statements 
      then putStrLn "Render/Parse tests passed!"
      else do
        putStrLn "Render/Parse tests failed. Statements do not match"
        putStrLn $ "Original: " ++ show (renderStatements statements)
        putStrLn $ "Parsed: " ++ show (renderStatements parsedStatements)
    Left err -> do
      putStrLn $ "Render/Parse tests failed: " ++ err
