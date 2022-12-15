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
    |> Seq.cache

let parse (lines: string seq) =
    let cols = lines |> Seq.head |> String.length
    let rows = lines |> Seq.length
    let map = Array2D.zeroCreate rows cols
    let mutable startCoord = None
    let mutable endCoord = None

    for (row, line) in lines |> Seq.indexed do
        for (col, mapChar) in line |> Seq.indexed do
            let heightValue =
                match mapChar with
                | 'S' ->
                    startCoord <- Some(row, col)
                    'a'
                | 'E' ->
                    endCoord <- Some(row, col)
                    'z'
                | value -> value

            map[row, col] <- (heightValue - 'a') |> Convert.ToInt32

    (startCoord, endCoord, map)

let getTarget (map: int[,]) =
    let mutable max = None
    let mutable maxCoord = None

    map
    |> Array2D.iteri (fun row column value ->
        match max with
        | None ->
            max <- Some value
            maxCoord <- Some(row, column)
        | Some(foundMax) when foundMax < value ->
            max <- Some value
            maxCoord <- Some(row, column)
        | _ -> ())

    maxCoord

let edges (map: int[,]) ((row, col): int * int) =
    let maxRow = (Array2D.length1 map) - 1
    let maxCol = (Array2D.length2 map) - 1

    let up =
        if 0 <= row - 1 && map[row - 1, col] <= map[row, col] + 1 then
            [ (row - 1, col) ]
        else
            []

    let down =
        if row + 1 <= maxRow && map[row + 1, col] <= map[row, col] + 1 then
            [ (row + 1, col) ]
        else
            []

    let left =
        if 0 <= col - 1 && map[row, col - 1] <= map[row, col] + 1 then
            [ (row, col - 1) ]
        else
            []

    let right =
        if col + 1 <= maxCol && map[row, col + 1] <= map[row, col] + 1 then
            [ (row, col + 1) ]
        else
            []

    Seq.concat [ up; down; left; right ]

let bfs (startCoords: (int * int) seq) (endCoord: int * int) (map: int[,]) =
    let nodeQueue = Queue(startCoords)
    let startSet = Set.ofSeq startCoords
    let mutable prevNodeMap = Map.empty<int * int, int * int>

    while nodeQueue.Count > 0 do
        let node = nodeQueue.Dequeue()
        let edges = edges map node

        if node <> endCoord then
            for edge in edges do
                if (not (Map.containsKey edge prevNodeMap)) && (not (Set.contains edge startSet)) then
                    prevNodeMap <- Map.add edge node prevNodeMap
                    nodeQueue.Enqueue(edge)

    seq {
        let mutable currentNode = endCoord

        while (Map.containsKey currentNode prevNodeMap)
              && (not (Set.contains currentNode startSet)) do
            currentNode <- Map.find currentNode prevNodeMap
            yield currentNode
    }
    |> Seq.rev
    |> Seq.cache

let showPath (map: int[,]) (path: (int * int) seq) =
    printf "\n"

    for row = 0 to (Array2D.length1 map) - 1 do
        for col = 0 to (Array2D.length2 map) - 1 do
            if Seq.contains (row, col) path then
                printf "%c" ('A' + (Convert.ToChar map[row, col]))
            else
                printf "%c" ('a' + (Convert.ToChar map[row, col]))

        printf "\n"

let day12a () =
    let (optStartCoord, optEndCoord, map) = readLines () |> parse

    let path =
        match (optStartCoord, optEndCoord) with
        | (Some startCoord, Some endCoord) -> bfs [ startCoord ] endCoord map
        | _ -> []

    showPath map path

    path |> Seq.length

let day12b () =
    let (_, optEndCoord, map) = readLines () |> parse

    let mutable startCoords = []

    map
    |> Array2D.iteri (fun row col value ->
        if value = 0 then
            startCoords <- (row, col) :: startCoords)

    let path =
        match optEndCoord with
        | Some endCoord -> bfs startCoords endCoord map
        | _ -> []

    showPath map path

    path |> Seq.length

let args = Environment.GetCommandLineArgs()

let part =
    if args.Length = 3 then
        Some(Environment.GetCommandLineArgs()[2])
    else
        None

match part with
| Some "a" -> day12a () |> printfn "Distance: %d"
| Some "b" -> day12b () |> printfn "Distance: %d"
| _ -> printfn "Usage: %s <a|b>" (args[1])
