open System
open System.IO

type Instruction = | NoOperation of int
                   | AccumulatorOperation of int
                   | JumpOperation of int
                   
type TerminationReason = | InfiniteLoop
                         | TerminatedSuccessfully
              
let stringSplit (token:string) (input:string) = input.Split(token, StringSplitOptions.RemoveEmptyEntries)
let words = stringSplit " "

let parseRow input =
    let parts = input |> words
    let param = parts.[1] |> int
    match parts.[0] with
    | "nop" -> (NoOperation param)
    | "acc" -> (AccumulatorOperation param)
    | "jmp" -> (JumpOperation param)
    | _ -> failwith "Unknown opcode";

let runProgram (program:Instruction array) =
    let rec runProgram' pc instructionsAlreadyRan acc = 
        if (Set.contains pc instructionsAlreadyRan) then
            (acc, InfiniteLoop)
        elif (pc >= program.Length) then
            (acc, TerminatedSuccessfully)
        else
            let instructionToRun = program.[pc]
            match instructionToRun with
            | NoOperation _ -> runProgram' (pc+1) (instructionsAlreadyRan.Add pc) acc
            | AccumulatorOperation value -> runProgram' (pc+1) (instructionsAlreadyRan.Add pc) (acc+value)
            | JumpOperation value -> runProgram' (pc+value) (instructionsAlreadyRan.Add pc) (acc)
    runProgram' 0 Set.empty 0

let fixProgram program =
    let swapOperation = function
        | JumpOperation value -> NoOperation value
        | NoOperation value -> JumpOperation value
        | other -> other
        
    let rec fixProgram' head instructions =
        match instructions with
        | [] -> None
        | currentOperation::restOfInstructions when (swapOperation currentOperation <> currentOperation)-> 
            let newProgram = (head |> List.rev) @ (swapOperation currentOperation)::restOfInstructions
            let (acc, terminationReason) = runProgram (newProgram |> Array.ofList)
            if(terminationReason = TerminatedSuccessfully) then
                Some acc
            else
                fixProgram' (currentOperation::head) restOfInstructions
        | currentOperation::restOfInstruction -> fixProgram' (currentOperation::head) restOfInstruction        
    fixProgram' [] (program |> List.ofArray)

[<EntryPoint>]
let main argv =
    let instructions = File.ReadAllLines "input.txt" |> Array.map parseRow
    
    let (acc, _)= instructions |> runProgram
    
    printfn "Task 1: %i" acc
    printfn "Task 2: %A" (instructions |> fixProgram)
    0 