# fp-2024

## Task 1

## Setup

To get started, you first need to open the project using Visual Studio Code and having Docker Desktop
1. `Ctrl + Shift + P`
2. `Dev Containers: Open Folder in Container`

To Build & Test the Project, run the following commands
1. `stack build`- to build the project
2. `stack test` - to run tests
3. `stack run fp2024-three --allow-different-user` - to run project with a different user context (more intuitive)

## REPL's Grammar (BNF)

This is the BNF my program is structured on: 

`<command>` ::= "make" `<order>` | "list" | "cancel"

`<order>` ::= `<coffee>` [`<cup>`] [`<extras>`] ["," `<order>`]

`<coffee>` ::= `<espressoDrink>` | `<filterCoffee>`

`<espressoDrink>` ::= "espresso" [`<shots>`] [`<milk>`] 

`<filterCoffee>` ::= "filter coffee using" `<type>`
`<type>` ::= "aeropress" | "v60" | "chemex"

`<shots>` ::= "with" `<number>` "shots"
`<number>` ::= "0" | "1" | "2" | "3" | "4" 

`<milk>` ::= "with" `<milkType>` "milk"
`<milkType>` ::= "whole" | "skim" | "almond" | "oat" | "soy" | ""

`<extras>` ::= "with" `<extra>` ["and" `<extra>`]
`<extra>` ::= "sugar" | `<syrupType>` "syrup"
`<syrupType>` ::= "salted caramel" | "strawberry" | "coconut" | "hazelnut" | "maple"

`<cup>` ::= "for takeout" | "for sitting in" 

#### Task 2

## Motivation for BNF changes

Between the first and second lab assignments, I made several changes to my BNF grammar. While these modifications were relatively minor, they were intended to make the grammar feel more natural, as if you were actually ordering from a café.

## Example workflow 

| make espresso with almond milk and sugar for takeout
> Orders placed: [Order {orderCoffeeType = Espresso, orderMilkType = Just Almond, orderShots = 1, orderCupType = Takeout, orderExtras = [Sugar]}]

| make filter coffee with v60 and sugar for sitting in
> Orders placed: [Order {orderCoffeeType = Filter V60, orderMilkType = Nothing, orderShots = 0, orderCupType = SitIn, orderExtras = []}]

| make espresso with 4 shots, espresso with 3 shots, espresso with 2 shots, espresso
> Orders placed: [Order {orderCoffeeType = Espresso, orderMilkType = Nothing, orderShots = 4, orderCupType = SitIn, orderExtras = []},Order {orderCoffeeType = Espresso, orderMilkType = Nothing, orderShots = 3, orderCupType = SitIn, orderExtras = []},Order {orderCoffeeType = Espresso, orderMilkType = Nothing, orderShots = 2, orderCupType = SitIn, orderExtras = []},Order {orderCoffeeType = Espresso, orderMilkType = Nothing, orderShots = 1, orderCupType = SitIn, orderExtras = []}]

| list orders
> 3: Order {orderCoffeeType = Espresso, orderMilkType = Nothing, orderShots = 4, orderCupType = SitIn, orderExtras = []}
> 4: Order {orderCoffeeType = Espresso, orderMilkType = Nothing, orderShots = 3, orderCupType = SitIn, orderExtras = []}
> 5: Order {orderCoffeeType = Espresso, orderMilkType = Nothing, orderShots = 2, orderCupType = SitIn, orderExtras = []}
> 6: Order {orderCoffeeType = Espresso, orderMilkType = Nothing, orderShots = 1, orderCupType = SitIn, orderExtras = []}
> 2: Order {orderCoffeeType = Filter V60, orderMilkType = Nothing, orderShots = 0, orderCupType = SitIn, orderExtras = []}
> 1: Order {orderCoffeeType = Espresso, orderMilkType = Just Almond, orderShots = 1, orderCupType = Takeout, orderExtras = [Sugar]}

| cancel order 2
> Order 1 canceled.

| show orders
> 3: Order {orderCoffeeType = Espresso, orderMilkType = Nothing, orderShots = 4, orderCupType = SitIn, orderExtras = []}
> 4: Order {orderCoffeeType = Espresso, orderMilkType = Nothing, orderShots = 3, orderCupType = SitIn, orderExtras = []}
> 5: Order {orderCoffeeType = Espresso, orderMilkType = Nothing, orderShots = 2, orderCupType = SitIn, orderExtras = []}
> 6: Order {orderCoffeeType = Espresso, orderMilkType = Nothing, orderShots = 1, orderCupType = SitIn, orderExtras = []}
> 1: Order {orderCoffeeType = Espresso, orderMilkType = Just Almond, orderShots = 1, orderCupType = Takeout, orderExtras = [Sugar]}

#### Task 3

## Program now accepts batch queries and can save and load states

| BEGIN 
| make espresso, filter coffee;
| make espresso with 2 shots with almond milk and sugar and maple syrup;
| make filter coffee using aeropress for takeout;
| END

TIP: use :paste to submit multi-line queries.

NEW ADDITIONS: Command `<save>` saves the state, command `<load>` loads the state

## Example workflow 

state.txt
> 

| make espresso
> Orders placed: [Order {orderCoffeeType = Espresso, orderMilkType = Nothing, orderShots = 1, orderCupType = SitIn, orderExtras = []}]

| make filter coffee
> Orders placed: [Order {orderCoffeeType = Filter V60, orderMilkType = Nothing, orderShots = 0, orderCupType = SitIn, orderExtras = []}]

| save
> State saved

state.txt
> BEGIN
> make filter coffee using v60 for sitting in; make espresso for sitting in;
> END

| :paste
| BEGIN
| make espresso with milk;
| make filter coffee with milk;
| END

> Orders placed: [Order {orderCoffeeType = Espresso, orderMilkType = Just Milk, orderShots = 1, orderCupType = SitIn, orderExtras = []}]
> Orders placed: [Order {orderCoffeeType = Filter V60, orderMilkType = Nothing, orderShots = 0, orderCupType = SitIn, orderExtras = []}]

| cancel order 1
> Order 1 canceled.

| list orders
> 4: Order {orderCoffeeType = Filter V60, orderMilkType = Nothing, orderShots = 0, orderCupType = SitIn, orderExtras = []}
> 3: Order {orderCoffeeType = Espresso, orderMilkType = Just Milk, orderShots = 1, orderCupType = SitIn, orderExtras = []}
> 2: Order {orderCoffeeType = Filter V60, orderMilkType = Nothing, orderShots = 0, orderCupType = SitIn, orderExtras = []}

| save
> State saved

state.txt
> BEGIN
> make filter coffee using v60 for sitting in; make espresso with milk for sitting in; make filter coffee using v60 for sitting in;
> END

~ terminal reboot ~

| load

state.txt
> BEGIN
> make filter coffee using v60 for sitting in; make espresso with milk for sitting in; make filter coffee using v60 for sitting in;
> END