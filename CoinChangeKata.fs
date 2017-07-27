module CoinChanger.Tests

open FsCheck
open FsCheck.Xunit

type Coin = One | Five | Ten | TwentyFive | Fifty

let valueOfCoin = function 
    | One -> 1 
    | Five -> 5 
    | Ten -> 10 
    | TwentyFive -> 25 
    | Fifty -> 50

let changeof coins amount =
    let coins = 
        coins |> List.sortByDescending valueOfCoin
    
    let folder (accAmount, result) coin =
        let remainAmount = accAmount % valueOfCoin coin
        let coinClones = accAmount / valueOfCoin coin |> List.replicate <| coin     
        remainAmount, coinClones @ result
    
    let init = amount, []
    List.fold folder init coins

[<Property>]
let ``Change amount with nothing to change gives back the initial amount`` (amount : byte) =    
    let expected = int amount
    let actual, _ = changeof [] expected
    expected = actual

[<Property>]
let ``Change zero amount result in original coin values`` (coins : Coin list) =
    let _, actual = changeof coins 0
    [] = actual

[<Property>]
let ``Change is always One's if there's no other coins values`` (amount : byte) =
    let amount = int amount
    let remain, actual = changeof [One] amount

    let expected = List.replicate amount One
    
    (expected = actual) |@ "Change is list of 'One' coins" .&. 
    (0 = remain)        |@ "There isn't any remain for change of 'One' coins"

[<Property>]
let ``Sum of changed coins and remaining amount is always the initial to-be-changed amount``
    (coins : Coin list)
    (amount : byte) =
    List.length coins <> 0 ==> lazy
    let expected = int amount

    let remain, change = changeof coins expected

    change
    |> List.map valueOfCoin
    |> List.sum
    |> (+) remain
    |> (=) expected

[<Property>]
let ``Non-One Coin value is always part of the change when the amount is that Coin value`` 
    (nonOneCoin : Coin) 
    (coins : Coin list) =
    (nonOneCoin <> One && List.length coins <> 0) ==> lazy

    Gen.shuffle (nonOneCoin :: coins)
    |> Gen.map Array.toList
    |> Arb.fromGen
    |> Prop.forAll <| fun coins ->
        valueOfCoin nonOneCoin
        |> changeof coins
        |> snd
        |> List.exists ((=) nonOneCoin)
