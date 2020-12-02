open System.IO

let day1_1 list target =
                    let listSet = Set.ofList list 
                    let factor = list |> List.find(fun i -> listSet.Contains(target-i))
                    (factor * (target-factor))
 
let rec combine n list = //Copied from SO
    match n, list with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (combine (k-1) xs) @ combine k xs

let day1_2 (input: int list) target =
                        let missingFactor factors = (target - List.sum factors)
                        let listSet = Set.ofList input
                        let factors = combine 2 input |> List.find(fun x -> listSet.Contains(missingFactor x)) 
                        (missingFactor factors)::factors |> List.fold (fun x y-> x * y) 1 

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines "input.txt" |> List.ofArray |> List.map (fun x-> x |> int)
    printfn "Puzzle 1: %i" (day1_1 input 2020) 
    printfn "Puzzle 2: %i" (day1_2 input 2020)
    0 // return an integer exit code
