let readlines path = System.IO.File.ReadLines path

let lmap = List.map
let lrev = List.rev
let lhead = List.head

let lines = readlines "./Day05.txt" |> List.ofSeq

let split (sep: char) (str: string) =
    str.Split sep
        |> List.ofArray
        |> List.map (fun t -> t.Trim())

let parseSeeds (line: string) =
    split ':' line
    |> lrev
    |> lhead
    |> split ' '
    |> lmap System.Convert.ToUInt64

type Range = {
    dst: uint64
    src: uint64
    length: uint64
}

type Mapping = {
    src: string
    dst: string
    ranges: list<Range>
}

let parseRanges (lines: list<string>) =
    lines
        |> List.map (split ' ' >> List.map System.Convert.ToUInt64)
        |> List.map (fun r ->
            match r with
            | [ d; s; l] -> { Range.dst = d; src = s; length = l }
            | _ -> invalidOp "adsf"
        )


let parseMapping (lines: list<string>): Mapping =
    let (src, dst) =
        lines.Head
            |> (split ' ' >> List.head)
            |> split '-'
            |> (fun m ->
                match m with
                | [s;_;d] -> (s, d)
                | _ -> invalidOp "asdsf"
            )
    let ranges = parseRanges lines.Tail
    { Mapping.src = src; dst = dst; ranges = ranges }

let rec parseMappings (lines: list<string>) =
    let mlines = List.takeWhile (fun (l: string) -> l.Length > 0) lines
    let mapping = parseMapping (mlines)
    let remainingLines = List.skip mlines.Length lines
    match remainingLines with
        | l when l.Length <= 1 -> [mapping]
        | _ -> mapping :: parseMappings remainingLines.Tail

let seeds = parseSeeds lines.Head
let mappings = (List.skip 2 >> parseMappings) lines

let rec findRange (v: uint64) (ranges: List<Range>) : Range =
    match ranges with
    | [] -> { Range.src = v; dst = v; length = (uint64)1 }
    | h::t ->
        match h with
        | r when r.src <= v && v < r.src + r.length -> r
        | _ -> findRange v t

let rec findLocation (v: uint64) (src: string) =
    match src with
    | "location" -> v
    | _ ->
        let mapping = List.find (fun m -> m.src = src) mappings
        let range = findRange v mapping.ranges
        findLocation (v + range.dst - range.src) mapping.dst

let locations =
    seeds
        |> List.map (fun s -> findLocation s "seed")

printfn "%A" (List.min locations)

(*
let rec pairs l =
    match l with
    | [] -> []
    | a::b::t -> (a, b) :: pairs t
    | _ -> invalidOp "asdfsd"

printfn "%A" (pairs seeds)
let locations2 =
    pairs seeds
    |> Set.ofList
    |> Seq.collect (fun p ->
        match p with
        | (a,b) -> [a..1UL..(a + b)]
    )
    |> Seq.map (fun s -> findLocation s "seed")

printfn "%A" (Seq.min locations2)
*)