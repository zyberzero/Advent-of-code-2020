open System
open System.IO

// This code contains two approaches. One naive implementenation based on the AoC description, and one that interprets each line as a binary string and converts that straight to the seatId.

type Row = | Row of int
type Column = | Column of int

type Seat = {Row: Row; Column: Column}

let parseBinaryString height lowerchar upperchar input  =
        let rec parseBinaryString' input low high = 
            match input with
            | [] -> if low = high then Some high else None
            | x::xs ->
            let offset = ((high-low)/2 + 1)
            match x with
                | x when x=lowerchar -> parseBinaryString' xs low (high-offset)
                | x when x=upperchar -> parseBinaryString' xs (low+offset) high
                | _ -> None
        parseBinaryString' input 0 height

let parseRow  input = parseBinaryString 127 'F' 'B' input |> Option.map Row
let parseColumn input  = parseBinaryString 7 'L' 'R' input |> Option.map Column

let parseSeat input=
    let chars = input |> List.ofSeq
    let row = parseRow chars.[..6]
    let column = parseColumn chars.[7..]
    match row with
    | None -> None
    | Some r -> match column with
                | None -> None
                | Some c -> Some {Row = r; Column=c}


let getSeatId {Row = (Row row); Column = (Column column)} = 
   row*8 + column 

let rec findGap inputs =
    match inputs with
        | (Some x)::(Some y)::_ when y=x+2 -> Some (x+1)
        | _::xs -> findGap xs
        | [] -> None



let rec findNumericGap = function
    | x::y::_ when y=x+2 -> Some (x+1)
    | _::xs -> findNumericGap xs
    | [] -> None


let parseToIdFromBinary (input:string) =
    let converted = input.Replace('F', '0').Replace('B', '1').Replace('R', '1').Replace('L', '0')
    Convert.ToInt32(converted, 2)


[<EntryPoint>]
let main argv =
    let seatIds = File.ReadAllLines "input.txt" |>  List.ofSeq |> List.map (parseToIdFromBinary) 

    let findSeats = List.sort >> findNumericGap

    printfn "Task 1: %A" (seatIds |> List.max)
    printfn "Task 2: %A" (seatIds |> findSeats)
    0 // return an integer exit code
