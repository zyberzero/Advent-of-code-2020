open System
open System.IO


let task1 = Seq.filter (Char.IsLetter) >> Seq.distinct

let task2 input = 
    let passengerCount = input |> Seq.filter (fun x -> x = '\n') |> Seq.length |> (+) 1 //We're counting line breaks, so add one for the "last" line as well
    input |> Seq.filter (Char.IsLetter) |> Seq.countBy id |> Seq.filter (fun (_,y) -> y = passengerCount) 


[<EntryPoint>]
let main argv =
    let groups = (File.ReadAllText "input.txt").Split (Environment.NewLine + Environment.NewLine) 
    let summarize task = Seq.map (task >> Seq.length) >> Seq.sum

    printfn "Task 1: %i" (summarize task1 groups)
    printfn "Task 2: %i" (summarize task2 groups)
    0 
