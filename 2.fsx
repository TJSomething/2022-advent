open System
open System.IO

let rec readLines() = seq {
    let line = Console.ReadLine()
    if line <> null then
        yield line
        yield! readLines ()
}

let interpretLine1 (line:string) =
    let elems = line.Trim().Split(" ")

    match Seq.toList elems with
    | [opponent; player] ->
        let normedOpponent =
            match opponent with
            | "A" -> "R"
            | "B" -> "P"
            | "C" -> "S"
            | a -> raise (System.ArgumentException($"{a} is not a valid play"))

        let normedPlayer =
            match player with
            | "X" -> "R"
            | "Y" -> "P"
            | "Z" -> "S"
            | a -> raise (System.ArgumentException($"{a} is not a valid play"))

        (normedPlayer, normedOpponent)
    | a -> raise (System.ArgumentException($"{a} is not a valid line"))

let interpretLine2 (line:string) =
    let elems = line.Trim().Split(" ")

    match Seq.toList elems with
    | [opponent; player] ->
        let normedOpponent =
            match opponent with
            | "A" -> "R"
            | "B" -> "P"
            | "C" -> "S"
            | a -> raise (System.ArgumentException($"{a} is not a valid play"))

        let normedPlayer =
            match (player, normedOpponent) with
            | ("Y", a) -> a
            | ("X", "R") | ("Z", "P") -> "S"
            | ("X", "P") | ("Z", "S") -> "R"
            | ("X", "S") | ("Z", "R") -> "P"
            | a -> raise (System.ArgumentException($"{a} is not a valid play"))

        (normedPlayer, normedOpponent)
    | a -> raise (System.ArgumentException($"{a} is not a valid line"))


let score (line:string) (lineToPlay:string -> string * string) =
    let elems = line.Trim().Split(" ")

    let (normedPlayer, normedOpponent) = lineToPlay line

    let outcomeScore =
        match (normedPlayer, normedOpponent) with
        | ("R", "P") | ("P", "S") | ("S", "R") -> 0
        | ("P", "R") | ("S", "P") | ("R", "S") -> 6
        | (a, b) when a = b -> 3
        | a -> raise (System.ArgumentException($"{a} is not a valid game"))

    let shapeScore =
        match normedPlayer with
        | "R" -> 1
        | "P" -> 2
        | "S" -> 3
        | a -> raise (System.ArgumentException($"{a} is not a valid play"))

    outcomeScore + shapeScore

let day2a() =
    readLines()
      |> Seq.filter (fun str -> not (String.IsNullOrEmpty str))
      |> Seq.fold (fun acc line ->
          acc + (score line interpretLine1)
      ) 0
      |> printfn "Score: %d"

let day2b() =
    readLines()
      |> Seq.filter (fun str -> not (String.IsNullOrEmpty str))
      |> Seq.fold (fun acc line ->
          acc + (score line interpretLine2)
      ) 0
      |> printfn "Score: %d"

printf """Pick part "a" or "b": """
let response = Console.ReadLine()

if response = "a" then
    day2a()
elif response = "b" then
    day2b()
