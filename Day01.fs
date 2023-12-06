// For more information see https://aka.ms/fsharp-console-apps
printfn "#1"

let readlines path = System.IO.File.ReadLines path

let lines = readlines "./Day01.txt"

let isDigit (c: char) = c <= '9' && c >= '0' 
let toDigit (c: char) = (int)(c - '0')

let decodeLine (line : string) = 
    line.ToCharArray() |> 
        Array.toList |> 
        List.filter isDigit |> 
        List.map toDigit

let lineNum (digits: List<int>) = digits.Head * 10 + (digits |> List.rev |> List.head) 

let sum = List.ofSeq lines |> List.map decodeLine |> List.map lineNum |> List.sum

printfn $"{sum}"

let nums = List.ofSeq (seq { 1..1..9 }) 
let numNames : List<string> = ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"]
let numberByName = Map.ofList (List.zip numNames nums) 

let decodePrefix (line: string) = 
    match isDigit (line[0]) with
    | true -> [toDigit(line[0])] 
    | false -> numNames |> List.filter line.StartsWith |> List.map numberByName.TryFind |> List.choose id 

let rec decodeLine2 (line: string) = 
    match line with
    | "" -> []
    | l -> List.append (decodePrefix l) (decodeLine2 (l.Substring 1))
let sum2 = List.ofSeq lines |> List.map decodeLine2 |> List.map lineNum  |> List.sum

printfn $"{sum2}"