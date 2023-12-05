let readlines path = System.IO.File.ReadLines path

let lines = readlines "./Day03.txt" |> List.ofSeq

let emptyLine =
    [0..1..lines.Head.Length + 2]
        |> List.fold (fun acc _ -> "." + acc) ""

let fixedLines =
    [emptyLine] @
    (lines |> List.map (fun l -> "." + l + ".")) @
    [emptyLine]

let isDigit (c: char) = c >= '0' && c <= '9'
let isSymbol (c: char) = (not (isDigit c)) && c <> '.'

let numbers (prevLine: string) (line: string) (nextLine: string) =
    let isMachinePartDigit (i: int) =
        isDigit(line[i]) && (
            isSymbol(line[i-1]) || isSymbol(line[i+1]) ||
            isSymbol(prevLine[i-1]) || isSymbol(prevLine[i]) || isSymbol(prevLine[i+1]) ||
            isSymbol(nextLine[i-1]) || isSymbol(nextLine[i]) || isSymbol(nextLine[i+1])
        )

    let rec findNumber (i: int) =
        if i >= line.Length then
            (false, "")
        else if isDigit line[i] then
            match findNumber (i + 1) with
            | (machinePart, number) -> (isMachinePartDigit i || machinePart, line[i].ToString() + number)
        else
            (false, "")

    [1..1..line.Length-1]
        |> List.filter (fun i -> not (isDigit line[i - 1]) && isDigit line[i])
        |> List.map findNumber
        |> List.filter fst
        |> List.map snd
        |> List.map System.Convert.ToInt32
        |> List.sum

let sum =
    [1..1..fixedLines.Length - 2]
        |> List.map (fun i -> numbers fixedLines[i-1] fixedLines[i] fixedLines[i+1])
        |> List.sum

printfn $"{sum}"