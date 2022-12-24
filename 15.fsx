#!/usr/bin/env -S dotnet fsi

open System
open System.Text.RegularExpressions
open System.Diagnostics

let rec readLines () =
    seq {
        let line = Console.ReadLine()

        if line <> null then
            if line <> "" then
                yield line

            yield! readLines ()
    }
    |> Seq.cache

type Sensor =
    { Sensor: int * int
      Beacon: int * int
      BeaconDist: int }

let linePattern =
    Regex(@"Sensor at x=([\d-]+), y=([\d-]+): closest beacon is at x=([\d-]+), y=([\d-]+)", RegexOptions.Compiled)

let manhattanDist ((x1, y1): int * int) ((x2, y2): int * int) = (abs (x1 - x2)) + (abs (y1 - y2))

let parse (line: string) =
    let lineGroups = (linePattern.Match line).Groups

    match lineGroups |> List.ofSeq with
    | [ _; sX; sY; bX; bY ] ->
        let s = (sX.Value |> int, sY.Value |> int)
        let b = (bX.Value |> int, bY.Value |> int)

        { Sensor = (sX.Value |> int, sY.Value |> int)
          Beacon = (bX.Value |> int, bY.Value |> int)
          BeaconDist = manhattanDist s b }
        |> Some
    | _ ->
        printfn "line did not match: %s" line
        None

let isBeaconPossible (coord: int * int) (map: Sensor[]) =
    map
    |> Array.tryFind (fun s ->
        let d = manhattanDist s.Sensor coord
        d <= s.BeaconDist && s.Beacon <> coord) = None

let beaconDist (coord: int * int) (map: Sensor[]) =
    map
    |> Array.map (fun s ->
        let d = manhattanDist s.Sensor coord
        d - s.BeaconDist)
    |> Array.min

let countImpossible (printDebug: bool) (y: int) (map: Sensor[]) =
    // The rightmost impossible beacon points must be within the distance of the
    // rightmost sensor to its nearest beacon. Same with the leftmost.

    let minImpossibleX =
        map |> Seq.map (fun s -> (fst (s.Sensor)) - s.BeaconDist) |> Seq.min

    let maxImpossibleX =
        map |> Seq.map (fun s -> (fst (s.Sensor)) + s.BeaconDist) |> Seq.max

    let mutable count = 0

    for x = minImpossibleX to maxImpossibleX do
        let possible = isBeaconPossible (x, y) map

        if printDebug then
            if map |> Array.tryFind (fun s -> s.Sensor = (x, y)) <> None then
                printf "B"
            elif map |> Array.tryFind (fun s -> s.Beacon = (x, y)) <> None then
                printf "S"
            elif not possible then
                printf "#"
            else
                printf "."

        if not possible then
            count <- count + 1

    count

let day15a () =
    let map = readLines () |> Seq.choose parse |> Array.ofSeq
    let y = 2_000_000

    countImpossible false y map

let day15b () =
    let map = readLines () |> Seq.choose parse |> Array.ofSeq
    let rnd = Random()
    let candidateCount = 10

    let candidates =
        Array.init candidateCount (fun _ -> (rnd.Next(0, 4_000_000), rnd.Next(0, 4_000_000)))

    let candidateDist = candidates |> Array.map (fun c -> beaconDist c map)

    let lastImproved = Array.create candidateCount 0

    let (rX, rY) =
        seq {
            let mutable lastTimestamp = Stopwatch.GetTimestamp()

            while true do
                if Stopwatch.GetElapsedTime(lastTimestamp).Seconds > 0 then
                    let (x, y, d) =
                        candidates
                        |> Array.map (fun (x, y) -> (x, y, (beaconDist (x, y) map)))
                        |> Array.maxBy (fun (_, _, c) -> c)

                    printfn "Best so far: (%d, %d) %d" x y d
                    lastTimestamp <- Stopwatch.GetTimestamp()

                for i = 0 to candidateCount - 1 do
                    let candidate = candidates[i]
                    let currentDist = candidateDist[i]
                    let x = fst candidate
                    let y = snd candidate
                    // Approximately implement Newton's method
                    let leftDist = beaconDist (x - 1, y) map
                    let rightDist = beaconDist (x + 1, y) map
                    let topDist = beaconDist (x, y - 1) map
                    let bottomDist = beaconDist (x, y + 1) map
                    let xSlope = (rightDist - leftDist) / 2
                    let ySlope = (bottomDist - topDist) / 2
                    // This isn't exactly right (that's the Euclidean
                    // distance), but since it's going to be used to divide the
                    // jump, it will prevent overshoot, since the Manhattan
                    // distance is always greater than or equal to the
                    // Euclidean distance. Also, max to prevent division by
                    // zero
                    let slopeMag = max ((abs xSlope) + (abs ySlope)) 1
                    let nextX = x + (rnd.Next(-3, 3)) - xSlope * currentDist / slopeMag
                    let nextY = y + (rnd.Next(-3, 3)) - ySlope * currentDist / slopeMag
                    let nextCandidate = (nextX, nextY)
                    let nextDist = beaconDist nextCandidate map

                    let isInBounds =
                        not (nextX < 0 || 4_000_000 < nextX || nextY < 0 || 4_000_000 < nextY)

                    if nextDist > 0 && isInBounds then
                        yield nextCandidate

                    if nextDist > currentDist then
                        candidates[i] <- nextCandidate
                        candidateDist[i] <- nextDist
                        lastImproved[i] <- 0
                    else
                        lastImproved[i] <- lastImproved[i] + 1

                    // If it falls off the map or it's been too long since improvement, restart
                    if (not isInBounds) || lastImproved[i] > 100 then
                        candidates[i] <- (rnd.Next(0, 4_000_000), rnd.Next(0, 4_000_000))
                        candidateDist[i] <- beaconDist candidates[i] map
        }
        |> Seq.head

    (int64 rX) * 4_000_000L + (int64 rY)

let args = Environment.GetCommandLineArgs()

let part =
    if args.Length = 3 then
        Some(Environment.GetCommandLineArgs()[2])
    else
        None

match part with
| Some "a" -> day15a () |> printfn "Count: %d"
| Some "b" -> day15b () |> printfn "Frequency: %d"
| Some "test1" ->
    let map = readLines () |> Seq.choose parse |> Array.ofSeq

    for i = 0 to 20 do
        printf "%02d:" i
        countImpossible true i map |> printfn " | %d"
| Some "test2" ->
    let map = readLines () |> Seq.choose parse |> Array.ofSeq

    for y = 0 to 20 do
        printf "%02d:" y

        for x = 0 to 20 do
            beaconDist (x, y) map |> printf "%02d,"

        printf "\n"
| _ -> printfn "Usage: %s <a|b>" (args[1])
