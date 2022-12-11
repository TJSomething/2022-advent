#!/usr/bin/env -S dotnet fsi

open System
open System.IO

let findMarker markerLength line =
    let firstChunkIndex =
        line
        |> Seq.windowed markerLength
        |> Seq.findIndex (fun chunk -> (Array.distinct chunk).Length = markerLength)

    firstChunkIndex + markerLength

let args = Environment.GetCommandLineArgs()

let part =
    if args.Length = 3 then
        Some(Environment.GetCommandLineArgs()[2])
    else
        None

if part = Some "a" then
    findMarker 4 (Console.ReadLine()) |> printfn "Start of packet: %d"
elif part = Some "b" then
    findMarker 14 (Console.ReadLine()) |> printfn "Start of packet: %d"
else
    printfn "Usage: %s <a|b>" (args[1])
