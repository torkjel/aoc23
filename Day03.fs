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

let isStar (c: char) = c = '*'

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

let numbers2 (lineNum: int) =

    let prevLine = fixedLines[lineNum - 1]
    let line = fixedLines[lineNum]
    let nextLine = fixedLines[lineNum + 1]

    let starPosition (i :int) =
        if isStar(line[i-1])  then
            Some (i-1, lineNum)
        else if isStar(line[i+1]) then
            Some (i+1, lineNum)
        else if isStar(prevLine[i-1]) then
            Some (i-1, lineNum-1)
        else if isStar(prevLine[i]) then
            Some (i, lineNum-1)
        else if isStar(prevLine[i+1]) then
            Some (i+1, lineNum-1)
        else if isStar(nextLine[i-1]) then
            Some (i-1, lineNum+1)
        else if isStar(nextLine[i]) then
            Some (i, lineNum+1)
        else if isStar(nextLine[i+1]) then
            Some (i+1, lineNum+1)
        else
            None

    let rec findNumber (i: int) =
        if i >= line.Length then
            (Set.empty, "")
        else if isDigit line[i] then
            match findNumber (i + 1) with
            | (stars, number) -> (
                match starPosition i with
                | Some pos -> stars.Add pos
                | None -> stars
                ,
                line[i].ToString() + number)
        else
            (Set.empty, "")

    [1..1..line.Length-1]
        |> List.filter (fun i -> not (isDigit line[i - 1]) && isDigit line[i])
        |> List.map findNumber

let sum2 =
    [1..1..fixedLines.Length - 2]
    |> List.map numbers2
    |> List.collect id
    |> List.filter (fun v -> v |> fst |> Set.isEmpty |> not)
    |> List.collect (fun v -> fst v |> Set.toList |> List.map (fun i -> (i, snd v))) // ( (fun s v -> s.Add((
    |> List.fold
        (fun s v ->
            Map.change
                (fst v)
                (fun old ->
                    match old with
                    | Some o -> Some ((snd v) :: o)
                    | None -> Some [snd v]
                )
                s
        )
        (Map.empty: Map<(int*int), List<string>>)
    |> Map.values
    |> List.ofSeq
    |> List.map (fun l ->
        match l with
        | [a: string; b: string] -> (System.Convert.ToInt32 a) * (System.Convert.ToInt32 b)
        | _ -> 0
    )
    |> List.sum

printfn $"{sum2}"
