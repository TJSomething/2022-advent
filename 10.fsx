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

let parse (lines: string seq) =
    seq {
        for line in lines do
            match line.Split() with
            | [| "noop" |] -> yield 0
            | [| "addx"; operand |] ->
                yield 0
                yield (int operand)
            | _ -> ()
    }

let day10a () =
    readLines ()
    |> parse
    |> Seq.scan (fun acc diff -> acc + diff) 1
    |> Seq.indexed
    |> Seq.map (fun (index, value) -> (index + 1, value))
    |> Seq.filter (fun (index, value) -> index % 40 = 20)
    |> Seq.map (fun (index, value) -> index * value)
    |> Seq.sum

let day10b () =
    readLines ()
    |> parse
    |> Seq.scan (fun acc diff -> acc + diff) 1
    |> Seq.indexed
    |> Seq.map (fun (index, value) ->
        let scanCoord = index % 40
        if abs (scanCoord - value) < 2 then '#' else '.')
    |> Seq.chunkBySize 40
    |> Seq.map Array.ofSeq
    |> Seq.map String
    |> String.concat "\n"

let args = Environment.GetCommandLineArgs()

let part =
    if args.Length = 3 then
        Some(Environment.GetCommandLineArgs()[2])
    else
        None

match part with
| Some "a" -> day10a () |> printfn "Total: %d"
| Some "b" -> day10b () |> printfn "%s"
| _ -> printfn "Usage: %s <a|b>" (args[1])
