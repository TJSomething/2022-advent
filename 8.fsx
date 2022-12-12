#!/usr/bin/env -S dotnet fsi

open System
open System.IO
open System.Collections.Generic

let rec readLines () =
    seq {
        let line = Console.ReadLine()

        if line <> null then
            if line <> "" then
                yield line

            yield! readLines ()
    }

let readTrees () =
    let treeBuf = List<int[]>()

    for line in readLines () do
        let row = Array.init line.Length (fun index -> (line[index] - '0') |> int)
        treeBuf.Add(row)

    Array2D.init (treeBuf.Count) (treeBuf[0].Length) (fun i j -> treeBuf[i][j])

let day8a () =
    let trees = readTrees ()
    let rows = Array2D.length1 trees
    let cols = Array2D.length2 trees
    let isVisible = Array2D.create cols rows false
    let mutable treeH = 0

    let updateVisibility row col =
        if treeH < trees[row, col] then
            isVisible[row, col] <- true
            treeH <- trees[row, col]

    for row = 0 to rows - 1 do
        treeH <- -1

        for col = 0 to cols - 1 do
            updateVisibility row col

    for row = 0 to rows - 1 do
        treeH <- -1

        for col = cols - 1 downto 0 do
            updateVisibility row col

    for col = 0 to cols - 1 do
        treeH <- -1

        for row = 0 to rows - 1 do
            updateVisibility row col

    for col = 0 to cols - 1 do
        treeH <- -1

        for row = rows - 1 downto 0 do
            updateVisibility row col

    let mutable count = 0

    Array2D.iter (fun v -> if v then count <- count + 1 else ()) isVisible

    printfn "Count: %d" count

let day8b () =
    let trees = readTrees ()
    let rows = Array2D.length1 trees
    let cols = Array2D.length2 trees

    let findFurthestTree initialHeight (coords: (int * int) seq) =
        let mutable distance = 0
        let mutable hasFoundTallerTree = false

        for (row, col) in coords do
            if not hasFoundTallerTree then
                distance <- distance + 1

                if initialHeight <= trees[row, col] then
                    hasFoundTallerTree <- true

        distance


    let rec coordsToEdge ((r, c): int * int) ((dr, dc): int * int) =
        seq {
            if 0 <= r && r < rows && 0 <= c && c < cols then
                yield (r, c)
                yield! coordsToEdge (r + dr, c + dc) (dr, dc)
        }

    let scores =
        seq {
            for row = 0 to rows - 1 do
                for col = 0 to cols - 1 do
                    let n = findFurthestTree trees[row, col] (coordsToEdge (row - 1, col) (-1, 0))
                    let s = findFurthestTree trees[row, col] (coordsToEdge (row + 1, col) (1, 0))
                    let w = findFurthestTree trees[row, col] (coordsToEdge (row, col - 1) (0, -1))
                    let e = findFurthestTree trees[row, col] (coordsToEdge (row, col + 1) (0, 1))

                    yield (row, col, (n * s * w * e))
        }

    let (row, col, topScore) = Seq.maxBy (fun (_, _, score) -> score) scores

    printfn "Top score at (%d, %d): %d" row col topScore

let args = Environment.GetCommandLineArgs()

let part =
    if args.Length = 3 then
        Some(Environment.GetCommandLineArgs()[2])
    else
        None

match part with
| Some "a" -> day8a ()
| Some "b" -> day8b ()
| _ -> printfn "Usage: %s <a|b>" (args[1])
