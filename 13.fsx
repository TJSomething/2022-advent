#!/usr/bin/env -S dotnet fsi

#r "nuget:FSharp.Json"

open System
open System.IO
open FSharp.Json

let rec readLines () =
    seq {
        let line = Console.ReadLine()

        if line <> null then
            if line <> "" then
                yield line

            yield! readLines ()
    }
    |> Seq.cache

type Packet =
    | PacketNum of int
    | PacketList of Packet list

let rec comparePacket (a: Packet) (b: Packet) =
    match (a, b) with
    | (PacketNum(aNum), PacketNum(bNum)) -> aNum.CompareTo(bNum)
    | (PacketNum(aNum), PacketList(_)) -> comparePacket (PacketList [ PacketNum aNum ]) b
    | (PacketList(_), PacketNum(bNum)) -> comparePacket a (PacketList [ PacketNum bNum ])
    | (PacketList(aHead :: aTail), PacketList(bHead :: bTail)) ->
        let comparison = comparePacket aHead bHead

        if comparison <> 0 then
            comparison
        else
            comparePacket (PacketList aTail) (PacketList bTail)
    | (PacketList([]), PacketList(_ :: _)) -> -1
    | (PacketList(_ :: _), PacketList([])) -> 1
    | (PacketList([]), PacketList([])) -> 0

let jsonConfig = JsonConfig.create (allowUntyped = true)

let parse (line: string) =
    let data = Json.deserializeEx<obj list> jsonConfig line

    let rec buildPacket (anyList: obj list) =
        List.choose
            (fun (item: obj) ->
                match item with
                | :? (decimal) as n -> Some(PacketNum(int n))
                | :? (obj list) as headList -> headList |> buildPacket |> PacketList |> Some
                | _ -> None)
            anyList

    buildPacket data |> PacketList

let day13a () =
    readLines ()
    |> Seq.map parse
    |> Seq.chunkBySize 2
    |> Seq.indexed
    |> Seq.choose (fun (index, pair) ->
        match pair with
        | [| a; b |] -> if comparePacket a b = 1 then None else Some(index + 1)
        | _ -> None)
    |> Seq.sum

let day13b () =
    let divider1 = parse "[[2]]"
    let divider2 = parse "[[6]]"

    readLines ()
    |> Seq.map parse
    |> Seq.append [ divider1; divider2 ]
    |> Seq.sortWith comparePacket
    |> Seq.indexed
    |> Seq.choose (fun (index, packet) ->
        if (packet = divider1) || (packet = divider2) then
            Some(index + 1)
        else
            None)
    |> Seq.reduce (*)

let args = Environment.GetCommandLineArgs()

let part =
    if args.Length = 3 then
        Some(Environment.GetCommandLineArgs()[2])
    else
        None

match part with
| Some "a" -> day13a () |> printfn "Sum: %d"
| Some "b" -> day13b () |> printfn "Decoder key: %d"
| _ -> printfn "Usage: %s <a|b>" (args[1])
