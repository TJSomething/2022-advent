#!/usr/bin/env -S dotnet fsi

open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

let rec readLines () =
    seq {
        let line = Console.ReadLine()

        if line <> null then
            if line <> "" then
                yield line

            yield! readLines ()
    }

type FSNode =
    | Dir of name: string * children: List<FSNode>
    | File of name: string * size: int

let readTree () =
    let rootChildren = List<FSNode>()
    let rootNode = Dir(name = "/", children = rootChildren)
    let dirChildrenStack = Stack<List<FSNode>>()
    let mutable isListing = false

    for line in readLines () do
        match (line[..4], line[5..]) with
        | ("$ cd ", "/") ->
            dirChildrenStack.Clear()
            dirChildrenStack.Push(rootChildren)

        | ("$ cd ", "..") -> dirChildrenStack.Pop() |> ignore

        | ("$ cd ", newDirName) ->
            let newChild =
                Seq.pick
                    (fun child ->
                        match child with
                        | Dir(childName, children) when childName = newDirName -> Some(children)
                        | _ -> None)
                    (dirChildrenStack.Peek())

            dirChildrenStack.Push(newChild)

        | ("$ ls", "") -> ()
        | _ ->
            let node =
                if line.StartsWith("dir") then
                    Dir(name = line[4..], children = List())
                else
                    let parts = line.Split(" ")
                    File(name = parts[1], size = (parts[0] |> int))

            dirChildrenStack.Peek().Add(node)

    rootNode


let getDirSizes (node: FSNode) =
    let rec helper ((acc: ((string * int) list) * int)) (node: FSNode) =
        let (dirs, currentSize) = acc

        match node with
        | Dir(name, children) ->
            let (dirs2, childrenSize) = Seq.fold helper (dirs, 0) children
            let dirs3 = (name, childrenSize) :: dirs2
            (dirs3, currentSize + childrenSize)
        | File(_, size) -> (dirs, currentSize + size)

    let (listing, _) = helper ([], 0) node
    listing

let day7a () =
    let tree = readTree ()

    let dirs =
        getDirSizes tree
        |> List.map (fun (_, size) -> size)
        |> List.filter (fun size -> size < 100000)
        |> List.sum

    printfn "Sum: %d" dirs

let day7b () =
    let tree = readTree ()

    let dirSizes = getDirSizes tree |> List.sortBy (fun (_, size) -> size)

    let totalSpace = 70_000_000
    let targetFreeSpace = 30_000_000
    let (_, usedSpace) = List.find (fun (name, _) -> name = "/") dirSizes
    let freeSpace = totalSpace - usedSpace
    let neededSpace = targetFreeSpace - freeSpace

    let (dirName, dirSize) =
        dirSizes |> List.find (fun (_, size) -> neededSpace <= size)

    printfn "Directory to remove: %s (%d)" dirName dirSize

let args = Environment.GetCommandLineArgs()

let part =
    if args.Length = 3 then
        Some(Environment.GetCommandLineArgs()[2])
    else
        None

if part = Some "a" then day7a ()
elif part = Some "b" then day7b ()
else printfn "Usage: %s <a|b>" (args[1])
