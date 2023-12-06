let readlines path = System.IO.File.ReadLines path

let lines = readlines "./Day04.txt" |> List.ofSeq

let split (sep: char) (str: string) =
    str.Split sep
        |> List.ofArray
        |> List.map (fun t -> t.Trim())

let splitTuple2 (sep: char) (str: string) =
    match split sep str with
    | a::b::t -> (a, b)
    | _ -> invalidOp "adfads"

let parseNumbers (str: string) =
    split ' ' str
        |> List.filter (fun f -> not ("".Equals f))
        |> List.map System.Convert.ToInt32

let processList (lines: List<string>) =
    lines
        |> List.map (fun a -> List.last (split ':' a))
        |> List.map (splitTuple2 '|')
        |> List.map (fun l ->
            match l with
            | (a ,b) -> (parseNumbers a, parseNumbers b)
        )
        |> List.map (fun l ->
            match l with
            | (a ,b) -> (Set.toList >> List.length) (Set.intersect (Set.ofList a) (Set.ofList b))
        )
        |> List.filter (fun a -> a > 0)
        |> List.map (fun a -> pown 2 (a - 1))
        |> List.sum

let r = processList lines

//printfn $"{r}"

let parseScratchCards lines =
    lines
    |> List.map (fun a -> List.last (split ':' a))
    |> List.map (splitTuple2 '|')
    |> List.map (fun l ->
        match l with
        | (a ,b) -> (parseNumbers a, parseNumbers b)
    )
        |> List.map (fun l ->
            match l with
            | (a ,b) -> (Set.toList >> List.length) (Set.intersect (Set.ofList a) (Set.ofList b))
        )

let rec processWins (cards: List<int>) (depth: int)=
    match depth with
    | 0 -> 0
    | _ ->
        cards.Head + processWins cards.Tail (depth - 1) + processWins cards.Tail cards.Head 
 
let cards = parseScratchCards lines

let wins = processWins cards (List.length cards) + List.length cards

printfn "%A" wins
