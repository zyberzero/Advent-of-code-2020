open System.IO

let adjacentDifference input = input |> List.tail |> List.zip (input |> List.take ((List.length input) - 1)) |> List.map (fun (x, y) -> y - x)

let task1 = adjacentDifference >> List.groupBy id >> Map.ofList >> (fun c -> c.[1].Length * c.[3].Length)

let task2 input =
    let options map x = map|> Map.add x ([ x .. x + 3 ] |> List.choose (fun c -> map |> Map.tryFind c) |> List.sum)
    input |> List.rev |> List.fold options (Map.empty |> Map.add (input |> List.last) 1L) |>  Map.find 0 


[<EntryPoint>]
let main argv =
    let inputs = File.ReadAllLines "input.txt" |> Seq.map int |> Seq.sort |> Seq.toList
        
    let startAndStopAdded = 0 :: inputs @ [ (Seq.max inputs) + 3 ]
    printfn "Task 1: %i" (startAndStopAdded |> task1)
    printfn "Task 2: %i" (startAndStopAdded |> task2)
    0 // return an integer exit code
