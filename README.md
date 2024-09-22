# fp-2024

## Setup

### To get started, you first need to open the project using Visual Studio Code and having Docker Desktop
1. `Ctrl + Shift + P`
2. `Dev Containers: Open Folder in Container`

### To Build & Test the Project, run the following commands
1. `stack build`
2. `stack test`

## REPL's Grammar (BNF)

`<coffeeMachine>` ::= `<order>`  
`<order>` ::= `<coffeeType>` `<cup>` | `<coffeeType>` `<cup>` `<addons>`  
`<coffeeType>` ::= `<espressoDrink>` | `<filterCoffee>`  
`<filterCoffee>` ::= "aeropress" | "v60" | "chemex"  
`<espressoDrink>` ::= `<shotCount>` | `<shotCount>` `<milk>`  
`<shotCount>` ::= [1-4]  
`<milk>` ::= "frothed" | "steamed" | `<frothedAndSteamedMilkRatio>`  
`<frothedAndSteamedMilkRatio>` ::= [1-4] ":" [1-4]  
`<cup>` ::= "disposable" | "reusable"  
`<addons>` ::= "sugar" | `<syrup>` | "sugar" `<syrup>`  
`<syrup>` ::= "saltedcaramel" | "strawberry" | "coconut" | "almond"