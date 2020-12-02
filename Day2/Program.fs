open System.IO

type PasswordRule =
    { Letter: char
      minOccurs: int
      maxOccurs: int }

type PasswordRow =
    { Rule: PasswordRule
      Password: string }

let parseRow (row: string): PasswordRow =
    let parseRule (rule: string) =
        let parts = rule.Split ' '
        let char = parts.[1].[0]
        let occurs = (parts.[0]).Split '-'
        let min = occurs.[0] |> int
        let max = occurs.[1] |> int
        { Letter = char
          minOccurs = min
          maxOccurs = max }

    let parts = row.Split ':'
    let rule = parseRule parts.[0]
    let password = (parts.[1])
    { Rule = rule; Password = password }

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
