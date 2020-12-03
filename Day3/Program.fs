// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

type MapObject =
    | Tree
    | Free

type Map = MapObject list list
type Position = int * int

let parseMap input =
    let parseCharacter c =
        match c with
        | '.' -> Free
        | '#' -> Tree
        | _ -> raise (Exception("Not a known object"))

    input
    |> Seq.map (fun row -> row |> Seq.map parseCharacter |> List.ofSeq)
    |> List.ofSeq

let getThingAtPos (x, y) (map: Map) =
    let mapX = x % map.[0].Length
    map.[y].[mapX]

let countTrees (map: Map) (positionMovement: Position -> Position) =
    let rec countTrees' (map: Map) (curPos: Position) (acc) =
        let mapLength = map.Length
        let (_, y) = curPos
        match y with
        | y when y >= mapLength -> acc
        | _ ->
            let newPos = positionMovement curPos
            match (getThingAtPos curPos map) with
            | Tree -> countTrees' map newPos (acc + 1L)
            | Free -> countTrees' map newPos acc

    countTrees' map (0, 0) 0L

let movement x_movement y_movement (x, y) = (x + x_movement, y + y_movement)

[<EntryPoint>]
let main argv =
    let input =
        File.ReadAllLines "input.txt"
        |> parseMap
        |> countTrees

    let movements_1 = [ movement 3 1 ]

    let movements_2 =
        [ movement 1 1
          movement 3 1
          movement 5 1
          movement 7 1
          movement 1 2 ]

    printfn "Task 1: %i" (movements_1 |> Seq.map input |> Seq.sum)
    printfn
        "Task 2: %i"
        (movements_2
         |> Seq.map input
         |> Seq.fold (fun x y -> x * y) 1L)
    0 // return an integer exit code
