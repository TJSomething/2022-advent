#!/usr/bin/env -S dotnet fsi

open System
open System.IO

let day1a() =
    let rec go (maxTotal:int) (currentTotal:int) =
        let line = Console.ReadLine()

        if line = null then
            maxTotal
        elif line.Length > 2 then
            let newTotal = currentTotal + int line

            if newTotal > maxTotal then
                go newTotal newTotal
            else
                go maxTotal newTotal
        else
            go maxTotal 0

    printfn "Max: %d" (go 0 0)

let day1b() =
    let rec go (totals:seq<int>) (currentTotal:int) =
        let line = Console.ReadLine()

        if line = null then
            Seq.append totals (Seq.singleton currentTotal)
        elif line.Length > 1 then
            go totals (currentTotal + int line)
        else
            go (Seq.append totals (Seq.singleton currentTotal)) 0

    go Seq.empty 0
        |> Seq.sortDescending
        |> Seq.take 3
        |> Seq.sum
        |> printfn "Top 3: %d"

printf """Pick part "a" or "b": """
let response = Console.ReadLine()

if response = "a" then
    day1a()
elif response = "b" then
    day1b()
