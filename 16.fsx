#!/usr/bin/env -S dotnet fsi

open System
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
    |> Seq.cache

type Valve =
    { Name: String
      FlowRate: int
      Neighbors: String Set }

let linePattern =
    Regex(@"Valve (.*) has flow rate=([0-9]+); tunnels{0,1} leads{0,1} to valves{0,1} (.+)", RegexOptions.Compiled)

let parse (line: string) =
    let lineGroups = (linePattern.Match line).Groups

    match lineGroups |> List.ofSeq with
    | [ _; name; flowRateStr; valvesStr ] ->
        let valves = valvesStr.Value.Split(", ")
        let flowRate = flowRateStr.Value |> int

        { Name = name.Value
          FlowRate = flowRate
          Neighbors = valves |> Set }
        |> Some
    | _ ->
        printfn "line did not match: %s" line
        None

let buildGraph (valves: Valve seq) = valves |> Array.ofSeq

type IntValve =
    { Index: int
      Name: String
      FlowRate: int
      Neighbors: (int * int) list }

type State =
    { Path: IntValve list
      Current: IntValve
      TimeElapsed: int
      PressureReleased: int
      OpenFlowRate: int
      OpenValves: int64
      OpenValveCount: int }

let stateHeuristic (maxTime: int) (state: State) =
    -(state.PressureReleased + state.OpenFlowRate * (maxTime * state.TimeElapsed))

let valveDistance (a: Valve) (b: Valve) (graph: Valve[]) =
    let nameToIndex =
        graph
        |> Seq.indexed
        |> Seq.map (fun (index, valve) -> (valve.Name, index))
        |> Map.ofSeq

    let mutable prevNodes = Set.singleton a.Name
    let mutable result = -1

    let nodeQueue = Queue()
    nodeQueue.Enqueue((a, 0))

    while nodeQueue.Count > 0 do
        let (node, dist) = nodeQueue.Dequeue()
        let edges = node.Neighbors

        if node <> b then
            for edge in edges do
                if not (Set.contains edge prevNodes) then
                    prevNodes <- Set.add edge prevNodes
                    nodeQueue.Enqueue((graph[nameToIndex[edge]], dist + 1))
        else
            nodeQueue.Clear()
            result <- dist

    result

let day16a () =
    let maxTime = 30

    let graph = readLines () |> Seq.map parse |> Seq.choose id |> buildGraph

    let valveFrontier = PriorityQueue<State, int>()

    let nameToIndex =
        graph
        |> Seq.indexed
        |> Seq.map (fun (index, valve) -> (valve.Name, index))
        |> Map.ofSeq

    let indexedGraph =
        graph
        |> Seq.indexed
        |> Seq.map (fun (index, valve) ->
            { Name = valve.Name
              Index = index
              FlowRate = valve.FlowRate
              Neighbors =
                graph
                |> Seq.indexed
                |> Seq.filter (fun (_, v) -> v.FlowRate > 0 && v.Name <> valve.Name)
                |> Seq.map (fun (index, otherValve) ->
                    let dist = valveDistance valve otherValve graph

                    (index, dist))
                |> List.ofSeq })
        |> Array.ofSeq

    let countOfUsefulValves =
        graph |> Seq.filter (fun valve -> valve.FlowRate > 0) |> Seq.length

    let start = indexedGraph[nameToIndex["AA"]]

    valveFrontier.Enqueue(
        { Path = []
          Current = start
          TimeElapsed = 0
          PressureReleased = 0
          OpenFlowRate = start.FlowRate
          OpenValves = 0
          OpenValveCount = 0 },
        0
    )

    let mutable bestPath = valveFrontier.Peek()

    while valveFrontier.Count > 0 do
        let state = valveFrontier.Dequeue()

        if bestPath.PressureReleased < state.PressureReleased then
            bestPath <- state
            printfn "New best:\n%O" bestPath

        let canTurnOnValve =
            state.Current.FlowRate > 0
            && (((1L <<< state.Current.Index) &&& state.OpenValves) = 0)

        if canTurnOnValve then
            let newTime = state.TimeElapsed + 1
            let newOpenValveCount = state.OpenValveCount + 1
            let newPressureReleased = state.PressureReleased + state.OpenFlowRate
            let newOpenFlowRate = state.OpenFlowRate + state.Current.FlowRate
            let newOpenValves = ((1L <<< state.Current.Index) ||| state.OpenValves)

            let nextState =
                { Path = state.Current :: state.Path
                  Current = state.Current
                  TimeElapsed = newTime
                  PressureReleased = newPressureReleased
                  OpenFlowRate = newOpenFlowRate
                  OpenValves = newOpenValves
                  OpenValveCount = newOpenValveCount }

            // Shortcut when we've opened the last valve
            if newTime < maxTime && newOpenValveCount = countOfUsefulValves then
                let skippedState =
                    { nextState with
                        TimeElapsed = maxTime
                        PressureReleased = newPressureReleased + newOpenFlowRate * (maxTime - newTime) }

                valveFrontier.Enqueue(skippedState, stateHeuristic maxTime nextState)
            elif newTime <= maxTime then
                valveFrontier.Enqueue(nextState, stateHeuristic maxTime nextState)
        else
            for (nextValveIndex, distance) in state.Current.Neighbors do
                let nextValve = indexedGraph[nextValveIndex]

                let newTime = state.TimeElapsed + distance

                // Since we converted the graph into a complete graph, we can skip
                // going to any closed valves
                let isClosed = (1L <<< nextValveIndex) &&& state.OpenValves = 0

                if newTime <= maxTime && isClosed then
                    let newPressureReleased = state.PressureReleased + state.OpenFlowRate * distance

                    let nextState =
                        { state with
                            Path = state.Current :: state.Path
                            Current = nextValve
                            TimeElapsed = newTime
                            PressureReleased = newPressureReleased }

                    valveFrontier.Enqueue(nextState, stateHeuristic maxTime nextState)


    bestPath

type Agent =
    { TargetValve: int; ValveDistance: int }

type MultiState =
    { Agents: Agent[]
      TimeElapsed: int
      PressureReleased: int
      OpenFlowRate: int
      OpenValves: int64
      OpenValveCount: int
      History: (int * (string * int)[]) list }


let multiStateHeuristic (maxTime: int) (state: MultiState) =
    -(state.PressureReleased + state.OpenFlowRate * (maxTime * state.TimeElapsed))

let addOpenValve (oldOpenValves: int64) (newValveIndex: int) =
    (1L <<< newValveIndex) ||| oldOpenValves

let isValveOpen (openValves: int64) (valveIndex: int) = (1L <<< valveIndex) &&& openValves <> 0

let day16b () =
    let maxTime = 26
    let agentCount = 2

    let graph = readLines () |> Seq.map parse |> Seq.choose id |> buildGraph

    let valveFrontier = PriorityQueue<MultiState, int>()

    let nameToIndex =
        graph
        |> Seq.indexed
        |> Seq.map (fun (index, valve) -> (valve.Name, index))
        |> Map.ofSeq

    let indexedGraph =
        graph
        |> Seq.indexed
        |> Seq.map (fun (index, valve) ->
            { Name = valve.Name
              Index = index
              FlowRate = valve.FlowRate
              Neighbors =
                graph
                |> Seq.indexed
                |> Seq.filter (fun (_, v) -> v.FlowRate > 0 && v.Name <> valve.Name)
                |> Seq.map (fun (index, otherValve) ->
                    let dist = valveDistance valve otherValve graph

                    (index, dist))
                |> List.ofSeq })
        |> Array.ofSeq

    let countOfUsefulValves =
        graph |> Seq.filter (fun valve -> valve.FlowRate > 0) |> Seq.length

    let start = indexedGraph[nameToIndex["AA"]]

    valveFrontier.Enqueue(
        { Agents =
            Array.init agentCount (fun _ ->
                { TargetValve = start.Index
                  ValveDistance = 0 })
          TimeElapsed = 0
          PressureReleased = 0
          OpenFlowRate = 0
          OpenValves = 0
          OpenValveCount = 0
          History = [] },
        0
    )

    let mutable bestPath = valveFrontier.Peek()

    while valveFrontier.Count > 0 do
        let state = valveFrontier.Dequeue()

        if bestPath.PressureReleased < state.PressureReleased then
            bestPath <- state
            printfn "New best:\n%O" bestPath

        // If all the valves are open, then skip to the end
        if state.TimeElapsed = maxTime then
            ()
        elif state.OpenValveCount = countOfUsefulValves then
            let newState =
                { state with
                    TimeElapsed = maxTime
                    PressureReleased = state.PressureReleased + state.OpenFlowRate * (maxTime - state.TimeElapsed) }

            valveFrontier.Enqueue(newState, multiStateHeuristic maxTime newState)
        else
            let mutable newTimeElapsed = state.TimeElapsed
            let mutable newPressureReleased = state.PressureReleased
            let mutable newOpenValves = state.OpenValves
            let mutable newOpenFlowRate = state.OpenFlowRate
            let mutable newOpenValveCount = state.OpenValveCount
            let mutable newAgents = [| state.Agents |]
            let mutable newHistory = []

            let nonDeterministicAgentUpdate (agentIndex: int) (nextVersionsOfAgent: Agent[]) =
                newAgents <-
                    newAgents
                    |> Array.collect (fun agentsState ->
                        nextVersionsOfAgent
                        |> Array.map (fun replacementAgent ->
                            agentsState
                            |> Array.mapi (fun i oldAgent -> if i = agentIndex then replacementAgent else oldAgent)))

            while newAgents.Length = 1 && newTimeElapsed < maxTime do
                newHistory <-
                    (newTimeElapsed,
                     newAgents[0]
                     |> Array.map (fun agent -> (indexedGraph[agent.TargetValve].Name, agent.ValveDistance)))
                    :: newHistory

                newTimeElapsed <- newTimeElapsed + 1
                newPressureReleased <- newPressureReleased + newOpenFlowRate

                for i = 0 to newAgents[0].Length - 1 do
                    if newAgents.Length = 0 then
                        ()
                    // Move all agents in transit one step for all candidate states
                    elif (newAgents[0][i]).ValveDistance > 0 then
                        nonDeterministicAgentUpdate
                            i
                            [| { newAgents[0][i] with ValveDistance = ((newAgents[0][i]).ValveDistance - 1) } |]
                    elif
                        ((indexedGraph[(newAgents[0][i]).TargetValve])).FlowRate > 0
                        && not (isValveOpen newOpenValves (newAgents[0][i]).TargetValve)
                    then
                        let valveToOpen = (newAgents[0][i]).TargetValve
                        newOpenValves <- addOpenValve newOpenValves valveToOpen
                        newOpenFlowRate <- newOpenFlowRate + (indexedGraph[valveToOpen]).FlowRate
                        newOpenValveCount <- newOpenValveCount + 1
                    // For all agents at valves that were open at the start of
                    // this step, split them into agents that are one step
                    // toward all valves that will available at the end of this
                    // step. Ensure that agents are going to unique valves.
                    else
                        let targetCandidates =
                            indexedGraph[(newAgents[0][i]).TargetValve].Neighbors
                            |> Seq.choose (fun (target, distance) ->
                                let isOpen = isValveOpen newOpenValves target

                                if not (isOpen) then
                                    Some
                                        { TargetValve = target
                                          ValveDistance = distance - 1 }
                                else
                                    None)
                            |> Array.ofSeq

                        if targetCandidates.Length > 0 then
                            nonDeterministicAgentUpdate i targetCandidates

                newAgents <-
                    newAgents
                    |> Array.filter (fun agentsState ->
                        let distinctTargets =
                            agentsState |> Seq.distinctBy (fun agent -> agent.TargetValve) |> Seq.length

                        distinctTargets = agentsState.Length)

            for agentsState in newAgents do
                let newState =
                    { Agents = agentsState
                      TimeElapsed = newTimeElapsed
                      PressureReleased = newPressureReleased
                      OpenFlowRate = newOpenFlowRate
                      OpenValves = newOpenValves
                      OpenValveCount = newOpenValveCount
                      History = List.concat [ newHistory; state.History ] }

                valveFrontier.Enqueue(newState, multiStateHeuristic maxTime newState)

    bestPath

let args = Environment.GetCommandLineArgs()

let part =
    if args.Length = 3 then
        Some(Environment.GetCommandLineArgs()[2])
    else
        None

match part with
| Some "a" -> day16a () |> printfn "Best:\n%O"
| Some "b" -> day16b () |> printfn "Best:\n%O"
| _ -> printfn "Usage: %s <a|b>" (args[1])
