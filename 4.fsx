#!/usr/bin/env -S dotnet fsi

open System
open System.IO
open System.Text.RegularExpressions

let rec readLines() = seq {
    let line = Console.ReadLine()
    if line <> null then
        if line <> "" then
            yield line.Trim()
        yield! readLines ()
}

let pattern1 = Regex(@"(\d+)-(\d+),(\d+)-(\d+)", RegexOptions.Compiled);

let rangeInside (n1:int) (n2:int) (m1:int) (m2:int) =
    n1 <= m1 && m1 <= n2 && n1 <= m2 && m2 <= n2

let rangeOverlaps (n1:int) (n2:int) (m1:int) (m2:int) =
    (n1 <= m1 && m1 <= n2) || (n1 <= m2 && m2 <= n2)

let interpretLine (check: int -> int -> int -> int -> bool) (line:string) =
    let m = pattern1.Match(line)
    let numbers =
        m.Groups
        |> Seq.skip 1
        |> Seq.map (fun s -> Int32.Parse s.Value)
        |> Seq.toArray

    if (check numbers[0] numbers[1] numbers[2] numbers[3]) || (check numbers[2] numbers[3] numbers[0] numbers[1]) then
        1
    else
        0

let day4a() =
    readLines()
      |> Seq.fold (fun acc line ->
          acc + (interpretLine rangeInside line)
      ) 0
      |> printfn "Count: %d"

let day4b() =
    readLines()
      |> Seq.fold (fun acc line ->
          acc + (interpretLine rangeOverlaps line)
      ) 0
      |> printfn "Count: %d"

printf """Pick part "a" or "b": """
let response = Console.ReadLine()

if response = "a" then
    day4a()
elif response = "b" then
    day4b()
