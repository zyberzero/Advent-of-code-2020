open System.IO
open System.Text.RegularExpressions

type Bag = {BagColor: string; Holds: (int*Bag)list}

let stringSplit (token:string) (string:string) = string.Split(token)

let parseInput = 
    let parseBag line =
        let parseHoldsBags (holdsString:string) =
            let parseSingleBag input =
                let matches = Regex.Match(input, "([0-9]+) (.*) bag")
                let numberOfBags = matches.Groups.[1].Value |> int
                (numberOfBags, matches.Groups.[2].Value.Trim())
            match holdsString.Contains("no other bags") with
            | true -> []
            | false -> holdsString |> stringSplit "," |> List.ofSeq |> List.map parseSingleBag
            
        let parts = line |> stringSplit "contain"
        let colorOfBag = (parts.[0] |> stringSplit "bags" |> Seq.head).Trim()
        let holds = parseHoldsBags parts.[1]
        (colorOfBag, holds)
        
    let addToMap (parsedBags:Map<string, (int*string) list>) bagLineToParse =
        let parsedColor, bags = parseBag bagLineToParse
        parsedBags.Add(parsedColor, bags)
        
    Seq.fold addToMap Map.empty 

let rec bagCanContainGold (allBags: Map<string, (int*string) list>) (bag:string) =
    match bag with
    | "shiny gold" -> true
    | _ ->
            let CanHoldBags = allBags.[bag]
            (CanHoldBags |> List.exists (fun  (_,color) -> bagCanContainGold allBags color))

let countBags startBag  (allBags: Map<string, (int*string) list>)=
    let rec countBags' (curBag) =
        let rec sumBag = function
        | [] -> 1
        | (count, bag)::xs -> ((count * countBags' bag) + (sumBag xs))
        (sumBag allBags.[curBag])
        
    (countBags' startBag)-1
    
[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines "input.txt"
    let parsed = input |> parseInput
    let parsedWithoutGold = parsed |> Map.filter (fun key _ -> key <> "shiny gold")
    
    printfn "Task 1: %i" (parsed |> Map.toList |> List.map (fun (key,_) -> bagCanContainGold parsedWithoutGold key) |> List.filter id |> List.length)
    printfn "Task 2: %i" (parsed |> countBags "shiny gold" )
    0 // return an integer exit code