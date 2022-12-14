#!/usr/bin/env -S dotnet fsi

open System
open System.IO
open System.Collections.Generic

type Operand =
    | OldValue
    | Literal of int

type Operation =
    | Plus of Operand * Operand
    | Minus of Operand * Operand
    | Times of Operand * Operand

type Monkey =
    { HeldItems: LinkedList<bigint>
      UpdateOp: Operation
      TestDivisor: int
      TestSuccessMonkey: int
      TestFailMonkey: int }

let rec readLines () =
    seq {
        let line = Console.ReadLine()

        if line <> null then
            if line <> "" then
                yield line

            yield! readLines ()
    }

let interpretUpdate (op: Operation) (oldValue: bigint) =
    let get (value: Operand) =
        match value with
        | OldValue -> oldValue
        | Literal(n) -> bigint n

    match op with
    | Plus(op1, op2) -> (get op1) + (get op2)
    | Minus(op1, op2) -> (get op1) - (get op2)
    | Times(op1, op2) -> (get op1) * (get op2)

let parse (lines: string seq) =
    seq {
        let mutable items = None
        let mutable update = None
        let mutable testDivisor = None
        let mutable testSuccessMonkey = None
        let mutable testFailMonkey = None

        for line in lines do

            match line.Split() |> List.ofArray with
            | "" :: "" :: "Starting" :: "items:" :: rest ->
                items <-
                    rest
                    |> List.map (fun s -> s.Replace(",", "") |> int |> bigint)
                    |> LinkedList<bigint>
                    |> Some
            | [ ""; ""; "Operation:"; "new"; "="; operand1Str; opStr; operand2Str ] ->
                let operand1 =
                    match operand1Str with
                    | "old" -> OldValue
                    | other -> other |> int |> Literal

                let operand2 =
                    match operand2Str with
                    | "old" -> OldValue
                    | other -> other |> int |> Literal

                update <-
                    match opStr with
                    | "+" -> Plus(operand1, operand2) |> Some
                    | "-" -> Minus(operand1, operand2) |> Some
                    | "*" -> Times(operand1, operand2) |> Some
                    | _ -> None
            | [ ""; ""; "Test:"; "divisible"; "by"; divisor ] -> testDivisor <- divisor |> int |> Some
            | [ ""; ""; ""; ""; "If"; "true:"; "throw"; "to"; "monkey"; monkey ] ->
                testSuccessMonkey <- monkey |> int |> Some
            | [ ""; ""; ""; ""; "If"; "false:"; "throw"; "to"; "monkey"; monkey ] ->
                testFailMonkey <- monkey |> int |> Some
            | _ -> ()

            match (items, update, testDivisor, testSuccessMonkey, testFailMonkey) with
            | (Some i, Some u, Some d, Some s, Some f) ->
                yield
                    { HeldItems = i
                      UpdateOp = u
                      TestDivisor = d
                      TestSuccessMonkey = s
                      TestFailMonkey = f }

                items <- None
                update <- None
                testDivisor <- None
                testSuccessMonkey <- None
                testFailMonkey <- None
            | _ -> ()

    }

let monkeyTurn (monkeys: Monkey[]) (index: int) (worryDecay: bigint -> bigint) =
    let currentMonkey = monkeys[index]

    let itemCount = currentMonkey.HeldItems.Count

    for item in currentMonkey.HeldItems do
        let newWorry1 = interpretUpdate currentMonkey.UpdateOp item
        let newWorry2 = worryDecay newWorry1

        let nextMonkey =
            if newWorry2 % (bigint currentMonkey.TestDivisor) = 0I then
                currentMonkey.TestSuccessMonkey
            else
                currentMonkey.TestFailMonkey

        monkeys[ nextMonkey ].HeldItems.AddLast(newWorry2) |> ignore

    monkeys[ index ].HeldItems.Clear()

    itemCount

let monkeyBusiness (monkeys: Monkey[]) (rounds: int) (worryDecay: bigint -> bigint) =
    let monkeyItemCount = Array.create monkeys.Length 0

    for roundNo = 1 to rounds do
        for index = 0 to monkeys.Length - 1 do
            monkeyItemCount[index] <- monkeyItemCount[index] + (monkeyTurn monkeys index worryDecay)

        printfn "\nAfter round %d:" roundNo

        for index = 0 to monkeys.Length - 1 do
            printfn "Monkey %d inspected items %d times" index monkeyItemCount[index]

    monkeyItemCount
    |> Array.sortDescending
    |> Array.map int64
    |> Array.take 2
    |> Array.reduce (fun a b -> a * b)

let gcd (a: bigint) (b: bigint) =
    let mutable aTmp = a
    let mutable bTmp = b

    while bTmp <> 0I do
        let tmp = bTmp
        bTmp <- aTmp % bTmp
        aTmp <- tmp

    aTmp

let lcm (a: bigint) (b: bigint) = (a * b) / (gcd a b)

let day11a () =
    let monkeys = readLines () |> parse |> Array.ofSeq
    monkeyBusiness monkeys 20 (fun n -> n / 3I)

let day11b () =
    let monkeys = readLines () |> parse |> Array.ofSeq

    let modulus =
        monkeys |> Array.map (fun m -> m.TestDivisor |> bigint) |> Array.reduce lcm

    monkeyBusiness monkeys 10000 (fun n -> n % modulus)

let args = Environment.GetCommandLineArgs()

let part =
    if args.Length = 3 then
        Some(Environment.GetCommandLineArgs()[2])
    else
        None

match part with
| Some "a" -> day11a () |> printfn "Monkey business: %d"
| Some "b" -> day11b () |> printfn "Monkey business: %d"
| _ -> printfn "Usage: %s <a|b>" (args[1])
