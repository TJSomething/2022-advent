#!/usr/bin/env -S dotnet fsi

open System

let rec readLines () =
    seq {
        let line = Console.ReadLine()

        if line <> null then
            if line <> "" then
                yield line

            yield! readLines ()
    }
    |> Seq.cache

let parse (line: string) =
    line.Split(" -> ")
    |> Seq.choose (fun s ->
        match s.Split(",") with
        | [| x; y |] -> Some(int x, int y)
        | _ -> None)

let segmentSeq ((startX, startY): int * int) ((endX, endY): int * int) =
    let xStep = Math.Sign(endX - startX)
    let yStep = Math.Sign(endY - startY)

    seq {
        let mutable x = startX
        let mutable y = startY

        while x <> endX || y <> endY do
            yield (x, y)
            x <- x + xStep
            y <- y + yStep

        yield (x, y)
    }

type Tile =
    | Empty
    | Sand
    | Wall

let initMap ((startX, startY): int * int) (wallPaths: (int * int) seq seq) =
    let cachedWallPaths = wallPaths |> Seq.cache

    let (minX, minY, maxX, maxY) =
        cachedWallPaths
        |> Seq.concat
        |> Seq.fold
            (fun (x1, y1, x2, y2) (x, y) -> (Math.Min(x1, x), Math.Min(y1, y), Math.Max(x2, x), Math.Max(y2, y)))
            (startX, startY, startX, startY)

    // Add extra width to allow running off side
    let w = maxX - minX + 3
    // Add some extra height to represent the abyss
    let h = maxY - minY + 2

    let map = Array2D.createBased (minX - 1) minY w h Empty

    for path in cachedWallPaths do
        for coords in Seq.windowed 2 path do
            for (x, y) in segmentSeq coords[0] coords[1] do
                map[x, y] <- Wall

    map

let showMap ((markerX, markerY): int * int) (map: Tile[,]) =
    for y = Array2D.base2 map to (Array2D.length2 map) + (Array2D.base2 map) - 1 do
        for x = Array2D.base1 map to (Array2D.length1 map) + (Array2D.base1 map) - 1 do
            let c =
                if x = markerX && y = markerY then
                    '+'
                else
                    match map[x, y] with
                    | Empty -> '.'
                    | Wall -> '#'
                    | Sand -> 'o'

            printf "%c" c

        printf "\n"

let rec addSand ((sandX, sandY): int * int) (map: Tile[,]) =
    let minX = (Array2D.base1 map)
    let minY = (Array2D.base2 map)
    let maxX = minX + (Array2D.length1 map) - 1
    let maxY = minY + (Array2D.length2 map) - 1

    if (sandY < minY || maxY < sandY + 1 || sandX - 1 < minX || maxX < sandX + 1) then
        false
    elif map[sandX, sandY] <> Empty then
        false
    elif map[sandX, sandY + 1] = Empty then
        addSand (sandX, sandY + 1) map
    elif map[sandX - 1, sandY + 1] = Empty then
        addSand (sandX - 1, sandY + 1) map
    elif map[sandX + 1, sandY + 1] = Empty then
        addSand (sandX + 1, sandY + 1) map
    else
        map[sandX, sandY] <- Sand
        true

let day14a () =
    let sandGenerator = (500, 0)

    let map = readLines () |> Seq.map parse |> initMap sandGenerator

    let mutable count = 0

    while addSand sandGenerator map do
        count <- count + 1

    showMap sandGenerator map

    count

let day14b () =
    let sandGenerator = (500, 0)

    let parsedSegments = readLines () |> Seq.map parse |> Seq.cache
    let map1 = initMap sandGenerator parsedSegments

    let newMaxY = (Array2D.base2 map1) + (Array2D.length2 map1)
    let pileHalfWidth = newMaxY - (snd sandGenerator) + 1
    let oldMinX = Array2D.base1 map1
    let oldMaxX = (Array2D.base1 map1) + (Array2D.length1 map1) - 1
    let newMaxX = Math.Max((fst sandGenerator) + pileHalfWidth, oldMaxX)
    let newMinX = Math.Min((fst sandGenerator) - pileHalfWidth, oldMinX)

    let segmentsWithFloor =
        [ (newMinX, newMaxY); (newMaxX, newMaxY) ]
        |> Seq.ofList
        |> Seq.singleton
        |> Seq.append parsedSegments

    let map2 = initMap sandGenerator segmentsWithFloor

    let mutable count = 0

    while addSand sandGenerator map2 do
        count <- count + 1

    showMap sandGenerator map2

    count

let args = Environment.GetCommandLineArgs()

let part =
    if args.Length = 3 then
        Some(Environment.GetCommandLineArgs()[2])
    else
        None

match part with
| Some "a" -> day14a () |> printfn "Units: %d"
| Some "b" -> day14b () |> printfn "Units: %d"
| _ -> printfn "Usage: %s <a|b>" (args[1])
