{-# LANGUAGE InstanceSigs #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition,
    Order(..),
    CoffeeType(..),
    MilkType(..),
    CupType(..),
    Extra(..),
    SyrupType(..),
    FilterType(..)
    ) where

import GHC.Conc (par)
import GHC.RTS.Flags (ParFlags(parGcNoSyncWithIdle))
import qualified Data.Char as C
import qualified Data.List as L

-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
-- | Represents a user query in the coffee ordering system.
data Query
  = MakeOrder [Order]  -- MakeOrder <order> ["and" <order>]
  | ListOrders
  | CancelOrder Int
  deriving (Show, Eq)

-- | Represents an order with all its details.
data Order = Order
  { orderCoffeeType :: CoffeeType
  , orderMilkType   :: Maybe MilkType
  , orderShots      :: Int
  , orderCupType    :: CupType
  , orderExtras     :: [Extra]
  } deriving (Show, Eq)

-- | Represents different types of coffee.
data CoffeeType
  = Espresso
  | Filter FilterType  -- Filter coffee can have different brewing methods.
  deriving (Show, Eq)

-- | Represents different types of filter coffee.
data FilterType
  = Aeropress
  | V60 -- V60 by default if not specified
  | Chemex
  deriving (Show, Eq)

-- | Represents types of milk.
data MilkType
  = Whole
  | Skim
  | Almond
  | Oat
  | Soy
  | Milk
  deriving (Show, Eq)

-- | Represents the type of cup.
data CupType
  = Takeout
  | SitIn -- SitIn by default if not specified
  deriving (Show, Eq)

-- | Represents extras in the order (e.g., syrup, sugar).
data Extra
  = Sugar
  | Syrup SyrupType
  deriving (Show, Eq)

-- | Represents different types of syrup.
data SyrupType
  = SaltedCaramel
  | Strawberry
  | Coconut
  | Hazelnut
  | Maple
  deriving (Show, Eq)

type Parser a = String -> Either String (a, String)

-- | Main parser for user input.
parseQuery :: String -> Either String (Query, String)
parseQuery input =
    case parseMakeOrder input of
        Right (orderList, rest) -> Right (MakeOrder orderList, rest)
        Left err1 -> 
            case parseListOrders input of
                Right (_, rest) -> Right (ListOrders, rest)
                Left err2 -> 
                    case parseCancelOrder input of
                        Right (orderId, rest) -> Right (CancelOrder orderId, rest)
                        Left err3 -> Left ("parsing failed: " ++ err1 ++ " or " ++ err2 ++ " or " ++ err3)

------------------------
-- |  MAKE PARSERS  | --
------------------------

-- | Parses a make order query.
-- | <order> ::= <coffee> [<cup>] [<extras>] ["and" <order>]
parseMakeOrder :: Parser [Order]
parseMakeOrder input =
    case parseExactWord "make" input of
        Right (_, rest) ->          
            case parseSingleOrder rest of
                Right (order, rest') -> 
                    case parseNextOrder rest' of
                        Right (orderList, rest'') -> Right (order : orderList, rest'')
                        Left err -> Left err
                Left err -> Left err 
        Left _ -> Left "expected 'make' at the beginning"

-- Helper function to parse the next order recursively.
parseNextOrder :: Parser [Order]
parseNextOrder input =
    case parseExactSymbol ',' input of
        Right (_, rest) -> 
            case parseSingleOrder rest of
                Right (nextOrder, rest') -> 
                    case parseNextOrder rest' of
                        Right (nextOrders, finalRest) -> Right (nextOrder : nextOrders, finalRest)  -- Combine orders
                        Left err -> Left err
                Left err -> Left err 
        Left _ -> Right ([], input)


-- | Parses a single order query.
parseSingleOrder :: Parser Order
parseSingleOrder input =
    case parseExactWord "espresso" input of
        Right (_, rest) -> parseMakeEspresso rest
        Left _ ->
            case parseExactWord "filter" input of
                Right (_, rest') ->
                    case parseExactWord "coffee" rest' of
                        Right (_, rest'') ->
                            case parseMakeFilterCoffee rest'' of
                                Right (order, rest''') -> Right (order, rest''')
                                Left err -> Left err
                        Left _ -> Left "expected 'coffee' after 'filter'" 
                Left _ -> Left "expected 'espresso' or 'filter' at the beginning" 

-- | Parses a make espresso query.
-- | <espressoDrink> ::= "espresso" [<shots>] [<milk>]  -- Optional milk
parseMakeEspresso :: Parser Order
parseMakeEspresso input =
    case parseExactWord "with" input of
        Right (_, rest) ->
            case parseShotCount rest of
                Right (numberOfShots, rest') ->
                    case parseExactWord "shots" rest' of
                        Right (_, rest'') ->
                            case parseExactWord "and" rest'' of
                                Right (_, rest''') -> 
                                    case parseMilkType rest''' of
                                        Right (milkType, rest'''') ->
                                            case parseExactWord "and" rest'''' of
                                                Right (_, rest''''') -> parseExtrasAndCup numberOfShots Espresso (Just milkType) rest'''''
                                                Left _ ->
                                                    case parseCupType rest'''' of
                                                        Right (cupType, rest''''') -> Right (Order Espresso (Just milkType) numberOfShots cupType [], rest''''')
                                                        Left _ -> Right (Order Espresso (Just milkType) numberOfShots SitIn [], rest'''')
                                        Left _ -> 
                                            parseExtrasAndCup numberOfShots Espresso Nothing rest'''
                                Left _ ->
                                        case parseExactWord "with" rest'' of
                                            Right (_, rest''') -> 
                                                case parseMilkType rest''' of
                                                    Right (milkType, rest'''') -> 
                                                        case parseExactWord "and" rest'''' of
                                                            Right (_, rest''''') -> parseExtrasAndCup numberOfShots Espresso (Just milkType) rest''''' 
                                                            Left _ -> 
                                                                case parseCupType rest'''' of
                                                                    Right (cupType, rest''''') -> Right (Order Espresso (Just milkType) numberOfShots cupType [], rest''''')
                                                                    Left _ -> Right (Order Espresso (Just milkType) numberOfShots SitIn [], rest''')
                                                    Left _ -> 
                                                        parseExtrasAndCup numberOfShots Espresso Nothing rest'''
                                            Left _ -> 
                                                case parseCupType rest'' of
                                                    Right (cupType, rest'''') -> Right (Order Espresso Nothing numberOfShots cupType [], rest'''')
                                                    Left _ -> Right (Order Espresso Nothing numberOfShots SitIn [], rest'')
                        Left _ -> Left "expected 'shots' after the number"
                Left _ -> 
                    case parseMilkType rest of
                        Right (milkType, rest'') -> 
                            case parseExactWord "and" rest'' of
                                Right (_, rest''') -> parseExtrasAndCup 1 Espresso (Just milkType) rest'''
                                Left _ ->
                                    case parseCupType rest'' of
                                    Right (cupType, rest''') -> Right (Order Espresso (Just milkType) 1 cupType [], rest''')
                                    Left _ -> Right (Order Espresso (Just milkType) 1 SitIn [], rest'')
                        Left _ -> 
                            parseExtrasAndCup 1 Espresso Nothing rest
        Left _ -> 
            case parseCupType input of
                Right (cupType, rest) -> Right (Order Espresso Nothing 1 cupType [], rest)
                Left _ -> Right (Order Espresso Nothing 1 SitIn [], input)

-- | Parses a make filter coffee query.
-- | <filterCoffee> ::= "filter coffee using" <type>
parseMakeFilterCoffee :: Parser Order
parseMakeFilterCoffee input =
    case parseExactWord "using" input of
        Right (_, rest) -> 
            case parseFilterCoffeeType rest of
                Right (filterType, rest') -> 
                    case parseExactWord "with" rest' of
                        Right (_, rest'') -> 
                            case parseMilkType rest'' of
                                Right (milkType, rest''') -> 
                                    case parseExactWord "and" rest''' of
                                        Right (_, rest'''') -> parseExtrasAndCup 0 (Filter filterType) (Just milkType) rest''''
                                        Left _ -> 
                                            case parseCupType rest''' of
                                                Right (cupType, rest'''') -> Right (Order (Filter filterType) (Just milkType) 0 cupType [], rest'''')
                                                Left _ -> Right (Order (Filter filterType) Nothing 0 SitIn [], rest''')
                                Left _ -> parseExtrasAndCup 0 (Filter filterType) Nothing rest''
                        Left _ ->
                            case parseCupType rest' of
                            Right (cupType, rest'') -> Right (Order (Filter filterType) Nothing 0 cupType [], rest'')
                            Left _ -> Right (Order (Filter filterType) Nothing 0 SitIn [], rest')
                Left _ -> Left "expected filter coffee type after 'using'"
        Left _ -> 
            case parseExactWord "with" input of
                Right (_, rest'') -> 
                    case parseMilkType rest'' of
                        Right (_, rest''') -> 
                            case parseExactWord "and" rest''' of
                                Right (_, rest'''') -> parseExtrasAndCup 0 (Filter V60) Nothing rest''''
                                Left _ -> 
                                    case parseCupType rest''' of
                                        Right (cupType, rest'''') -> Right (Order (Filter V60) Nothing 0 cupType [], rest'''')
                                        Left _ -> Right (Order (Filter V60) Nothing 0 SitIn [], rest''')
                        Left _ -> parseExtrasAndCup 0 (Filter V60) Nothing rest''
                Left _ ->
                    case parseCupType input of
                    Right (cupType, rest'') -> Right (Order (Filter V60) Nothing 0 cupType [], rest'')
                    Left _ -> Right (Order (Filter V60) Nothing 0 SitIn [], input)

-- | Parses the extras and cup type for the order
-- | <extras> ::= "with" <extra> ["and" <extra>]
-- | <cup> ::= "for takeout" | "for sitting in" 
parseExtrasAndCup :: Int -> CoffeeType -> Maybe MilkType -> String -> Either String (Order, String)
parseExtrasAndCup numberOfShots coffeeType milkType rest =
    case parseExtrasType rest of
        Right (extra1, rest') ->
            case parseExactWord "and" rest' of
                Right (_, rest'') ->
                    case parseExtrasType rest'' of
                        Right (extra2, rest''') ->
                            case parseCupType rest''' of
                                Right (cupType, rest'''') ->
                                    Right (Order coffeeType milkType numberOfShots cupType [extra1, extra2], rest'''')
                                Left _ -> Right (Order coffeeType milkType numberOfShots SitIn [extra1, extra2], rest''')
                        Left _ -> 
                            case parseCupType rest'' of
                                Right (cupType, rest''') ->
                                    Right (Order coffeeType milkType numberOfShots cupType [extra1], rest''')
                                Left _ -> Right (Order coffeeType milkType numberOfShots SitIn [extra1], rest'')
                Left _ -> 
                    case parseCupType rest' of
                        Right (cupType, rest'') ->
                            Right (Order coffeeType milkType numberOfShots cupType [extra1], rest'')
                        Left _ -> Right (Order coffeeType milkType numberOfShots SitIn [extra1], rest')
        Left _ -> 
            case parseCupType rest of
                Right (cupType, rest') ->
                    Right (Order coffeeType milkType numberOfShots cupType [], rest')
                Left _ -> Right (Order coffeeType milkType numberOfShots SitIn [], rest)

-- | Parses the number of shots which must be between 2 and 4.
-- | <shots> ::= "with" <number> "shots"
-- | <number> ::= "0" | "1" | "2" | "3" | "4" 
parseShotCount :: Parser Int
parseShotCount input = 
    case parseNumber input of
        Right (numberOfShots, rest) 
            | numberOfShots > 1 && numberOfShots < 5 -> Right (numberOfShots, rest)
            | otherwise -> Left "number of shots must be between 2 and 4"
        Left _ -> Left "expected number of shots after 'with'"

-- | Parses the cup type which can either be "takeout" or "sitting in".
-- | <cup> ::= "for takeout" | "for sitting in" 
parseCupType :: Parser CupType
parseCupType input = 
    case parseExactWord "for" input of
        Right (_, rest) -> 
            case parseExactWord "takeout" rest of
                Right (_, rest') -> Right (Takeout, rest')
                Left _ -> 
                    case parseExactWord "sitting" rest of
                        Right (_, rest') -> 
                            case parseExactWord "in" rest' of
                                Right (_, rest'') -> Right (SitIn, rest'')
                                Left _ -> Left "expected 'in' after 'sitting'"
                        Left _ -> Left "expected 'takeout' or 'sitting' after 'for'"
        Left _ -> Left "expected cup type"

-- | Parses the milk type which can be "whole", "skim", "almond", "oat", or "soy".
-- | <milk> ::= "with" <milkType> "milk"
-- | <milkType> ::= "whole" | "skim" | "almond" | "oat" | "soy" | ""  -- Types of milk available
parseMilkType :: Parser MilkType
parseMilkType input =
    case parseWord input of
        Right (word, rest) -> 
            case word of
                "whole"  -> checkMilk rest Whole
                "skim"   -> checkMilk rest Skim
                "almond" -> checkMilk rest Almond
                "oat"    -> checkMilk rest Oat
                "soy"    -> checkMilk rest Soy
                "milk"   -> Right (Milk, rest)
                _ -> Left "expected milk type or just basic milk"
        Left _ -> Left "expected milk type after with"

-- | Parses the extras type which can be "sugar" or a syrup type.
-- | <extra> ::= "sugar" | `<syrupType>` "syrup"  -- Syrup options
parseExtrasType :: Parser Extra
parseExtrasType input =
    case parseExactWord "sugar" input of
        Right (_, rest') -> Right (Sugar, rest')
        Left _ ->
            case parseSyrupType input of
                Right (syrupType, rest') -> Right (Syrup syrupType, rest')
                Left _ -> Left "expected extras type"

-- | Parses the syrup type which can be "salted caramel", "strawberry", "coconut", "hazelnut" or "maple".
-- | <syrupType> ::= "salted caramel" | "strawberry" | "coconut" | "hazelnut" | "maple"
parseSyrupType :: Parser SyrupType
parseSyrupType input =
    case parseWord input of
        Right (word, rest) -> 
            case word of
                "salted" -> 
                    case parseExactWord "caramel" rest of
                        Right (_, rest') -> checkSyrup rest' SaltedCaramel
                        Left _ -> Left "expected 'caramel' after 'salted'"
                "strawberry" -> checkSyrup rest Strawberry
                "coconut" -> checkSyrup rest Coconut
                "hazelnut" -> checkSyrup rest Hazelnut
                "maple" -> checkSyrup rest Maple
                _ -> Left "expected syrup type"
        Left _ -> Left "expected syrup type"

-- | Parses the filter coffee type which can be "aeropress", "v60", or "chemex".
-- <filterCoffee> ::= "filter coffee using" `<type>`
-- <type> ::= "aeropress" | "v60" | "chemex"
parseFilterCoffeeType :: Parser FilterType
parseFilterCoffeeType input =
    case parseWord input of
        Right (word, rest) -> 
            case word of
                "aeropress" -> Right (Aeropress, rest)
                "v" -> 
                    case parseExactNumber 60 rest of
                        Right (_, rest') -> Right (V60, rest')
                        Left _ -> Left "expected 'v60'"
                "chemex" -> Right (Chemex, rest)
                _ -> Left "expected filter coffee type"
        Left _ -> Left "expected filter coffee type after 'using'"

------------------------
-- |  LIST PARSERS  | --
------------------------

-- | Parses a list orders query.
parseListOrders :: Parser Query
parseListOrders input =
    case parseExactWord "list" input of
        Right (_, rest) ->
            case parseExactWord "orders" rest of
                Right (_, rest') -> Right (ListOrders, rest')
                Left _ -> Left "expected 'orders' after 'list'"
        Left _ -> Left "expected 'list orders' at the beginning"

------------------------
-- | CANCEL PARSERS | --
------------------------

-- | Parses a cancel order query.
parseCancelOrder :: Parser Int
parseCancelOrder input =
    case parseExactWord "cancel" input of
        Right (_, rest) ->
            case parseExactWord "order" rest of
                Right (_, rest') ->
                    case parseNumber rest' of
                        Right (orderId, rest'') -> 
                            if null (L.dropWhile C.isSpace rest'')
                            then Right (orderId, rest'')  -- Return order ID
                            else Left "expected end of input after order ID"
                        Left _ -> Left "expected order ID after 'cancel'"
                Left _ -> Left "expected 'order' after 'cancel'"
        Left _ -> Left "expected 'cancel' at the beginning"

------------------------
-- |  STATE FUNCS.  | --
------------------------

-- | An entity which represents your program's state.
data State = State
  { orders :: [(Int,Order)]
  , nextOrderId :: Int
  } deriving (Show, Eq)

-- | Creates an initial program's state.
emptyState :: State
emptyState = State [] 1 

-- | Updates a state according to a query.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition state (MakeOrder newOrders) = 
    let newOrderId = nextOrderId state  
        updatedOrders = zip [newOrderId..] newOrders
        combinedOrders = updatedOrders ++ orders state
        updatedState = State combinedOrders (newOrderId + length newOrders)
    in Right (Just $ "Orders placed: " ++ show (map snd updatedOrders), updatedState)

stateTransition state ListOrders = 
    let orderList = orders state
        message = if null orderList 
                  then Just "No current orders."
                  else Just $ "Current orders:\n" ++ unlines [show orderId ++ ": " ++ show order | (orderId, order) <- orderList]
    in Right (message, state)

stateTransition state (CancelOrder orderId) =
    let (toCancel, remaining) = L.partition (\(oid, _) -> oid == orderId) (orders state)
        updatedState = State remaining (nextOrderId state)  -- Update the state with remaining orders
        message = if null toCancel
                  then Just $ "No order found with ID: " ++ show orderId
                  else Just $ "Order " ++ show orderId ++ " canceled."
    in Right (message, updatedState)


------------------------
-- | HELPER PARSERS | --
------------------------

-- | Parses a specific exact word from the input.
parseExactWord :: String -> Parser String
parseExactWord expectedWord input =
    case parseWord input of
        Right (word, rest) ->
            if word == expectedWord
                then Right (word, rest)
                else Left ("expected '" ++ expectedWord ++ "', but got '" ++ word ++ "'")
        Left err -> Left err

-- | Parses a specific exact symbol (character) from the input.
parseExactSymbol :: Char -> Parser Char
parseExactSymbol expectedSymbol input =
    let trimmedInput = L.dropWhile C.isSpace input
    in case trimmedInput of
         [] -> Left "input is empty, cannot parse a symbol"
         (firstChar:rest) ->
             if firstChar == expectedSymbol
             then Right (firstChar, L.dropWhile C.isSpace rest)
             else Left ("expected '" ++ [expectedSymbol] ++ "', but got '" ++ [firstChar] ++ "'")

-- | Parses a word from the input, looking for letters.
parseWord :: Parser String
parseWord input =
    let trimmedInput = L.dropWhile C.isSpace input  -- Skip leading whitespace
        letters = L.takeWhile C.isLetter trimmedInput
        rest = L.drop (length letters) trimmedInput
    in if not (null letters)
        then Right (letters, rest)
        else Left (input ++ " does not start with a letter")

-- | Parses a number from the input, looking for digits.
parseNumber :: Parser Int
parseNumber [] = Left "empty input, cannot parse a number"
parseNumber input =
    let trimmedInput = L.dropWhile C.isSpace input 
        digits = L.takeWhile C.isDigit trimmedInput
        rest = drop (length digits) trimmedInput
    in
        case digits of
            [] -> Left "not a number"
            _ -> Right (read digits, rest)

-- | Parses a specific exact number from the input.
parseExactNumber :: Int -> Parser Int
parseExactNumber expectedNumber input =
    case parseNumber input of
        Right (number, rest) ->
            if number == expectedNumber
                then Right (number, rest)
                else Left ("expected '" ++ show expectedNumber ++ "', but got '" ++ show number ++ "'")
        Left err -> Left err

-- | Checks for keyword 'milk' after the milk type.
checkMilk :: String -> MilkType -> Either String (MilkType, String)
checkMilk rest milkType =
    case parseExactWord "milk" rest of
        Right (_, rest') -> Right (milkType, rest')
        Left _ -> Left "expected 'milk' after milk type"

-- | Checks for keyword 'syrup' after the syrup type.
checkSyrup :: String -> SyrupType -> Either String (SyrupType, String)
checkSyrup rest syrupType =
    case parseExactWord "syrup" rest of
        Right (_, rest') -> Right (syrupType, rest')
        Left _ -> Left "expected 'syrup' after syrup type"
