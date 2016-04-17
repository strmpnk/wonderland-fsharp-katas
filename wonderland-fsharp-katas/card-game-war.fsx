open System

type Suit =
    | Spade
    | Club
    | Diamond
    | Heart

type Rank =
    | Value of int
    | Jack
    | Queen
    | King
    | Ace

type Card = Suit * Rank

type Game =
    | Playing of Card list * Card list
    | Player1Win
    | Player2Win

// Make the values of cards explicit
let value card =
    let suiteValue =
        match fst card with
        | Spade -> 1
        | Club -> 2
        | Diamond -> 3
        | Heart -> 4
    let rankValue =
        match snd card with
        | Value n -> n
        | Jack -> 11
        | Queen -> 13
        | King -> 14
        | Ace -> 15
    (rankValue, suiteValue)

let playRound game =
    match game with
    | Player1Win | Player2Win -> failwith "game over"
    | Playing (_, []) -> Player1Win
    | Playing ([], _) -> Player2Win
    | Playing (card1::rest1, card2::rest2) when value card1 < value card2 ->
        Playing (rest1, rest2 @ [card2; card1])
    | Playing (card1::rest1, card2::rest2) ->
        Playing (rest1 @ [card1; card2], rest2)

let playGame deck1 deck2 =
    let rec loop game =
        match game with
        | Playing (p1, p2) ->
            loop <| playRound game
        | Player1Win ->
            printfn "Player 1 wins"
            game
        | Player2Win ->
            printfn "Player 2 wins"
            game
    loop <| Playing (deck1, deck2)

let fullDeck : Card list =
    let suits = [Spade; Club; Diamond; Heart]
    let ranks = [Jack; Queen; King; Ace] @ [for n in 2 .. 10 -> Value n]
    [ for suit in suits do
        for rank in ranks do
            yield (suit, rank) ]

let shuffle deck : Card list =
    let rand = new Random()
    List.sortBy (fun _ -> rand.NextDouble()) deck

let deal deck : Card list * Card list =
    deck |> List.fold (fun  (left, right) card -> (right, card :: left)) ([], [])

let uncurry f (x, y) = f x y

fullDeck
|> List.take 8
|> shuffle
|> deal
|> uncurry playGame

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =

    test <@ playGame [(Spade, Ace)] [(Spade, King)] = Player1Win @>
    test <@ playGame [] [(Spade, King)] = Player2Win @>
    test <@ playGame [(Spade, Ace)] [] = Player1Win @>

// run the tests
tests ()
