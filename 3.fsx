#!/usr/bin/env -S dotnet fsi

open System
open System.IO

let rec readLines() = seq {
    let line = Console.ReadLine()
    if line <> null then
        yield line
        yield! readLines ()
}

let getLetterPriority(letter:char) =
    if Char.IsLower letter then
        (Convert.ToInt32 letter) - (Convert.ToInt32 'a') + 1
    elif Char.IsUpper letter then
        (Convert.ToInt32 letter) - (Convert.ToInt32 'A') + 27
    else
        raise (ArgumentException $"\"{letter}\" is not a valid letter")


let interpretLine1 (line:string) =
    let trimmed = line.Trim()

    let halfLength = trimmed.Length / 2
    let compartment1 = set (trimmed.Substring(0, halfLength))
    let compartment2 = set (trimmed.Substring(halfLength, halfLength))

    let letterIntersection = Set.intersect compartment1 compartment2

    if letterIntersection.Count <> 1 then
        raise (ArgumentException $"intersection of compartments is \"{letterIntersection}\"")

    let duplicateLetter = letterIntersection.MinimumElement

    getLetterPriority duplicateLetter

let interpretLines2 (lines:seq<string>) =
    let letterIntersection =
        Seq.map (fun (s:string) -> Set(s.Trim())) lines
        |> Seq.reduce Set.intersect

    if letterIntersection.Count <> 1 then
        raise (ArgumentException $"intersection of packs is \"{letterIntersection}\"")

    let duplicateLetter = letterIntersection.MinimumElement

    getLetterPriority duplicateLetter

let day3a() =
    readLines()
      |> Seq.filter (fun str -> not (String.IsNullOrEmpty str))
      |> Seq.fold (fun acc line ->
          acc + (interpretLine1 line)
      ) 0
      |> printfn "Score: %d"

let day3b() =
    readLines()
      |> Seq.filter (fun str -> not (String.IsNullOrEmpty str))
      |> Seq.chunkBySize 3
      |> Seq.fold (fun acc chunk ->
          acc + (interpretLines2 chunk)
      ) 0
      |> printfn "Score: %d"

printf """Pick part "a" or "b": """
let response = Console.ReadLine()

if response = "a" then
    day3a()
elif response = "b" then
    day3b()
