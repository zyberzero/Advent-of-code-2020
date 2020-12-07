open System
open System.IO

let splitBy f =
    let i = ref 0
    Seq.map (fun x ->
        if f x then incr i
        !i, x)
    >> Seq.groupBy fst
    >> Seq.map (fun (_, b) -> Seq.map snd b)

let parseChunk chunk =
    let mergedChunk =
        chunk |> Seq.fold (fun x y -> x + " " + y) ""
    let joinKey (strings: string list) = strings.[0], strings.[1]
    mergedChunk.Split(" ", StringSplitOptions.RemoveEmptyEntries)
    |> Seq.toList
    |> List.map (fun x -> x.Split ":" |> List.ofSeq |> joinKey)
    |> Map.ofList

let tryParseInt :(string -> int option) =
    Int32.TryParse
    >> function
    | true, v -> Some v
    | false, _ -> None

let validPassword_part_1 (passportParts: Map<string, string>) =
    [ "byr"
      "iyr"
      "eyr"
      "hgt"
      "hcl"
      "ecl"
      "pid" ]
    |> List.map (passportParts.ContainsKey)
    |> List.forall id

let validPassword_part_2 (passportParts: Map<string, string>) =

    let validEyr eyr =
        match (tryParseInt eyr) with
        | None -> false
        | Some v -> v >= 2020 && v <= 2030

    let validByr byr =
        match (tryParseInt byr) with
        | None -> false
        | Some v -> v >= 1920 && v <= 2002

    let validIyr iyr =
        match (tryParseInt iyr) with
        | None -> false
        | Some v -> v >= 2010 && v <= 2020

    let validHgt (hgt:string) =
        if hgt.EndsWith "in" then
            match (tryParseInt (hgt.Substring(0, hgt.Length-2))) with
            | None -> false
            | Some v ->  v >= 59 && v <= 76
        else if hgt.EndsWith "cm" then
            match (tryParseInt (hgt.Substring(0, hgt.Length-2))) with
            | None -> false
            | Some v ->  v >= 150 && v <= 193
        else
            false
                    
    let validHcl (hcl:string) =
        let isHex c =  (c >= '0' && c <= '9') || (c >= 'a' && c<='f')
        hcl.[0] = '#' && hcl |> Seq.skip 1 |> Seq.filter (isHex) |> Seq.length = hcl.Length - 1
        
    let validEcl ecl = ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"] |> List.exists (fun x-> x = ecl)

    let validPid pid =
        match (tryParseInt pid) with
        | None -> false
        | Some _ -> pid.Length = 9

    validPassword_part_1 passportParts
    && validByr passportParts.["byr"]
    && validIyr passportParts.["iyr"]
    && validEyr passportParts.["eyr"]
    && validPid passportParts.["pid"]
    && validEcl passportParts.["ecl"]
    && validHcl passportParts.["hcl"]
    && validHgt passportParts.["hgt"]

[<EntryPoint>]
let main argv =
    let chunks =
        File.ReadAllLines "input.txt"
        |> splitBy (String.IsNullOrWhiteSpace)

    let parsedPasswords = chunks |> Seq.map parseChunk
    printfn
        "Part 1: %i"
        (parsedPasswords
         |> Seq.map validPassword_part_1
         |> Seq.filter id
         |> Seq.length)
    printfn
        "Part 1: %i"
        (parsedPasswords
         |> Seq.map validPassword_part_2
         |> Seq.filter id
         |> Seq.length)
    0 // return an integer exit code