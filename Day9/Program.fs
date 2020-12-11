open System.IO

let task1 length (ints: int64 seq) =
    let verifyWindow (res, window) =
        let set = window |> Set.ofSeq
        window |> Seq.exists (fun i -> Set.contains (res - i) set)

    ints
    |> Seq.windowed length
    |> Seq.zip (ints |> Seq.skip length)
    |> Seq.find (verifyWindow >> not)
    |> fun (a, _) -> a

let task2 target (ints: int64 seq) =
    [ 2 .. (Seq.length ints) ]
    |> Seq.map (fun c -> ints |> Seq.windowed c)
    |> Seq.concat
    |> Seq.find (fun window -> ((window |> Seq.sum) = target))
    |> (fun list -> (Seq.min list + Seq.max list))


[<EntryPoint>]
let main argv =
    let input =
        File.ReadAllLines "input.txt" |> Seq.map int64

    let task1_result = input |> task1 25
    printfn "Task 1: %A" task1_result
    printfn "Task 2: %A" (task2 task1_result input)
    0 // return an integer exit code
