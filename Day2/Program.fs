open System.IO
open System.Text.RegularExpressions

type PasswordRule =
    { Letter: char
      minOccurs: int
      maxOccurs: int }

type PasswordRow =
    { Rule: PasswordRule
      Password: string }

let parseRow (row: string): PasswordRow =
    let matches =
        Regex.Match(row, "([0-9]+)-([0-9]+) ([a-zA-Z]): (.*)")

    { Rule =
          { minOccurs = (matches.Groups.[1].Value |> int)
            maxOccurs = (matches.Groups.[2].Value |> int)
            Letter = (matches.Groups.[3].Value.[0]) }
      Password = matches.Groups.[4].Value }

let verifyRowPart1 { Rule = rule; Password = password } =
    let { minOccurs = min; maxOccurs = max; Letter = char } = rule

    let chars =
        password.ToCharArray()
        |> List.ofSeq
        |> List.filter (fun c -> c = char)
        |> List.length

    chars >= min && chars <= max

let verifyRowPart2 { Rule = rule; Password = password } =
    let { minOccurs = min; maxOccurs = max; Letter = char } = rule
    (password.[min] = char) <> (password.[max] = char)

[<EntryPoint>]
let main argv =
    let input = File.ReadLines "input.txt"

    let calculateValidPasswords verifier =
        (input
         |> List.ofSeq
         |> List.map (fun l -> l |> parseRow |> verifier)
         |> List.filter id
         |> List.length)

    printfn "Part 1: %i" (calculateValidPasswords verifyRowPart1)
    printfn "Part 2: %i" (calculateValidPasswords verifyRowPart2)
    0 // return an integer exit code
