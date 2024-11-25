{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord

import Lib1 qualified
import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Lib1 tests"
  [ testCase "List of completions is not empty" $
      null Lib1.completions @?= False,
    testCase "Parsing case 1 - making an espresso drink" $
      Lib2.parseQuery "make espresso with 3 shots and milk and sugar and maple syrup for sitting in" @?= Right (Lib2.MakeOrder [Lib2.Order {Lib2.orderCoffeeType = Lib2.Espresso, Lib2.orderMilkType = Just Lib2.Milk, Lib2.orderShots = 3, Lib2.orderCupType = Lib2.SitIn, Lib2.orderExtras = [Lib2.Sugar, Lib2.Syrup Lib2.Maple]}]),
    testCase "Parsing case 2 - making a filter coffee drink" $
      Lib2.parseQuery "make filter coffee with aeropress and almond milk and sugar and sugar for takeout" @?= Right (Lib2.MakeOrder [Lib2.Order {Lib2.orderCoffeeType = Lib2.Filter Lib2.V60, Lib2.orderMilkType = Nothing, Lib2.orderShots = 0, Lib2.orderCupType = Lib2.SitIn, Lib2.orderExtras = []}]),
    testCase "Parsing case 3 - making a filter coffee drink and 2 espresso drinks" $
      Lib2.parseQuery "make filter coffee, espresso, espresso" @?= Right (Lib2.MakeOrder [Lib2.Order {Lib2.orderCoffeeType = Lib2.Filter Lib2.V60, Lib2.orderMilkType = Nothing, Lib2.orderShots = 0, Lib2.orderCupType = Lib2.SitIn, Lib2.orderExtras = []}, Lib2.Order {Lib2.orderCoffeeType = Lib2.Espresso, Lib2.orderMilkType = Nothing, Lib2.orderShots = 1, Lib2.orderCupType = Lib2.SitIn, Lib2.orderExtras = []}, Lib2.Order {Lib2.orderCoffeeType = Lib2.Espresso, Lib2.orderMilkType = Nothing, Lib2.orderShots = 1, Lib2.orderCupType = Lib2.SitIn, Lib2.orderExtras = []}])
  ]