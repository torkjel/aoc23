let readlines path = System.IO.File.ReadLines path

let lines = readlines "./Day02.txt" |> List.ofSeq

let split (str: string) (sep: char) =
    str.Split sep
        |> List.ofArray
        |> List.map (fun t -> t.Trim())

type Round = {
    red: int
    green: int
    blue: int
}

type Game = {
    id : int
    rounds : list<Round>
}

let parseGameId (header: string) =
    match List.ofArray (header.Split ' ') with
    | [_; id] -> System.Convert.ToInt32 id
    | _ -> invalidOp "bad header"

let parseDraw (draw: string) : (string * int) =
    match (split draw ' ') with
        | [n; color] -> (color, (System.Convert.ToInt32 n))
        | _ -> invalidOp "adf"

let rec parseRound (round: Round) (rounds: list<(string * int)>): Round =
    match rounds with
        | [] -> round
        | h :: t ->
            let updatedRound =
                match h with
                    | ("red", n) -> { round with Round.red = n }
                    | ("green", n) -> { round with Round.green = n }
                    | ("blue", n) -> { round with Round.blue = n }
                    | _ -> invalidOp "asd"
            parseRound updatedRound t


let parseRoundStr (round: string) : Round =
    let emptyRound = { Round.red = 0; Round.green = 0; Round.blue = 0 }
    split round ','
        |> List.map parseDraw
        |> parseRound emptyRound

let parseRounds (rounds: string) =
    split rounds ';' |> List.map parseRoundStr

let parseline (line: string): Game =
    match split line ':' with
    | [id; draws] -> { Game.id = parseGameId id; Game.rounds = parseRounds draws }
    | _ -> invalidOp "bad line"

let games = List.map parseline lines

let rec possibleGame (rounds: list<Round>) =
    match rounds with
        | [] -> true
        | h :: t -> (h.red <= 12 && h.green <= 13 && h.blue <= 14) && possibleGame t

let sum =
    games
        |> List.filter (fun g -> possibleGame (g.rounds))
        |> List.map (fun g -> g.id)
        |> List.sum

printfn $"{sum}"



