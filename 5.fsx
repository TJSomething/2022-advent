#!/usr/bin/env -S dotnet fsi

open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Linq

let rec readLines() = seq {
    let line = Console.ReadLine()
    if line <> null then
        if line <> "" then
            yield line
        yield! readLines ()
}

let patternCrates = Regex(@"(   |\[[A-Z]\])( |$)", RegexOptions.Compiled)
let patternStacks = Regex(@"^ (\d +)+$", RegexOptions.Compiled)
let patternMove = Regex(@"move (\d+) from (\d+) to (\d+)", RegexOptions.Compiled)

let crateMover9000 count (source:LinkedList<char>) (dest:LinkedList<char>) =
    for _ = 1 to count do
        if source.Count <> 0 then
            let crate = source.First
            source.RemoveFirst()
            dest.AddFirst(crate.Value) |> ignore

let crateMover9001 count (source:LinkedList<char>) (dest:LinkedList<char>) =
    let tmpCrates = LinkedList<char>()

    for _ = 1 to count do
        if source.Count <> 0 then
            let crate = source.First
            source.RemoveFirst()
            tmpCrates.AddFirst(crate.Value) |> ignore

    for crate in tmpCrates do
        dest.AddFirst(crate) |> ignore

let day5 crateMover =
    let mutable parsingStacks = true
    let stacks = List<LinkedList<char>>()

    for line in readLines() do
        if parsingStacks then
            if patternStacks.IsMatch line then
                parsingStacks <- false
            else
                let crates = patternCrates.Matches line

                // Make sure stacks is as long as the line
                if stacks.Count < crates.Count then
                    for i = 0 to (crates.Count - stacks.Count - 1) do
                        stacks.Add(LinkedList<char>())

                // For each column, put the crate at the end of the list, which
                // is the bottom of the stack, assuming there is a crate.
                for crateIndex = 0 to (crates.Count - 1) do
                    let crateStr = crates[crateIndex].Value
                    
                    if crateStr.StartsWith "[" then
                        stacks[crateIndex].AddLast(crateStr[1]) |> ignore
        else
            let move = patternMove.Match line
            let count = move.Groups[1].Value |> int
            let source = (move.Groups[2].Value |> int) - 1
            let dest = (move.Groups[3].Value |> int) - 1

            crateMover count stacks[source] stacks[dest]
    
    for stack in stacks do
        for crate in Enumerable.Reverse(stack) do
            printf "%c" crate
        printf "\n"

let args = Environment.GetCommandLineArgs()

let part =
    if args.Length = 3 then
        Some (Environment.GetCommandLineArgs()[2])
    else
        None

if part = Some "a" then
    day5 crateMover9000
elif part = Some "b" then
    day5 crateMover9001
else
    printfn "Usage: %s <a|b>" (args[1])

