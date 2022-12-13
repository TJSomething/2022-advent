#!/usr/bin/env -S dotnet fsi

open System
open System.IO

let rec readLines () =
    seq {
        let line = Console.ReadLine()

        if line <> null then
            if line <> "" then
                yield line

            yield! readLines ()
    }

type Move =
    | Up of int
    | Right of int
    | Down of int
    | Left of int

let parseMoves (lines: string seq) =
    seq {
        for line in lines do
            let tokens = line.Split(' ')

            let move =
                try
                    match tokens with
                    | [| "R"; distStr |] -> Right(int distStr) |> Some
                    | [| "D"; distStr |] -> Down(int distStr) |> Some
                    | [| "U"; distStr |] -> Up(int distStr) |> Some
                    | [| "L"; distStr |] -> Left(int distStr) |> Some
                    | _ -> None
                with :? FormatException ->
                    None

            match move with
            | Some foundMove -> yield foundMove
            | _ -> ()
    }

let splitMoves (moves: Move seq) =
    seq {
        for move in moves do
            let (submove, count) =
                match move with
                | Up dist -> ((0, 1), dist)
                | Right dist -> ((1, 0), dist)
                | Left dist -> ((-1, 0), dist)
                | Down dist -> ((0, -1), dist)

            for _ = 1 to count do
                yield submove
    }

type RopeLink =
    { Head: (int * int); Tail: (int * int) }

let moveHeadWithTail (state: RopeLink) ((diffX, diffY): int * int) =
    let { Head = (headX, headY)
          Tail = (tailX, tailY) } =
        state

    let newHeadX = headX + diffX
    let newHeadY = headY + diffY
    let newDiffX = newHeadX - tailX
    let newDiffY = newHeadY - tailY

    let (tailDXMag, tailDYMag) =
        match (abs newDiffX, abs newDiffY) with
        | (0, 0) -> (0, 0)
        | (0, 1) -> (0, 0)
        | (0, 2) -> (0, 1)
        | (1, 0) -> (0, 0)
        | (1, 1) -> (0, 0)
        | (1, 2) -> (1, 1)
        | (2, 0) -> (1, 0)
        | (2, 1) -> (1, 1)
        | (2, 2) -> (1, 1)
        | (x, y) -> (x - 1, y - 1)

    let tailDX = (sign newDiffX) * tailDXMag
    let tailDY = (sign newDiffY) * tailDYMag

    let newTailX = tailX + tailDX
    let newTailY = tailY + tailDY

    { Head = (newHeadX, newHeadY)
      Tail = (newTailX, newTailY) }

let day9a () =
    let initialState = { Head = (0, 0); Tail = (0, 0) }

    readLines ()
    |> parseMoves
    |> splitMoves
    |> Seq.scan moveHeadWithTail initialState
    |> Seq.countBy (fun s -> s.Tail)
    |> Seq.length

let moveRope (rope: (int * int) array) ((diffX, diffY): int * int) =
    let newRope = Array.create rope.Length (0, 0)

    newRope[0] <- ((rope[0] |> fst) + diffX, (rope[0] |> snd) + diffY)

    for index = 1 to rope.Length - 1 do
        let (oldHeadX, oldHeadY) = rope[index - 1]
        let (newHeadX, newHeadY) = newRope[index - 1]
        let diffHeadX = newHeadX - oldHeadX
        let diffHeadY = newHeadY - oldHeadY

        let oldLink =
            { Head = rope[index - 1]
              Tail = rope[index] }

        let { Tail = newTail } = moveHeadWithTail oldLink (diffHeadX, diffHeadY)

        newRope[index] <- newTail

    newRope

let day9b () =
    let initialState = Array.create 10 (0, 0)

    let subMoves = readLines () |> parseMoves |> splitMoves |> Seq.cache

    let ropePositions = subMoves |> Seq.scan moveRope initialState |> Seq.cache

    let allTailPositions = ropePositions |> Seq.map Seq.last |> Seq.cache

    allTailPositions |> Seq.distinct |> Seq.length

let args = Environment.GetCommandLineArgs()

let part =
    if args.Length = 3 then
        Some(Environment.GetCommandLineArgs()[2])
    else
        None

match part with
| Some "a" -> printfn "Positions: %d" (day9a ())
| Some "b" -> printfn "Positions: %d" (day9b ())
| _ -> printfn "Usage: %s <a|b>" (args[1])
