module Day4

open System
open System.Text.RegularExpressions

type ActionType = Start=0 | Sleep=0 | Wake=1
type UserAction = { id: int option; minute: int; action: ActionType }
let parseLine line =
    let m = Regex.Match(line, @"^\[.*?(\d+)\] (Guard #(\d+)|falls|wakes)");
    let g = m.Groups
    {
        id = match g.[3].Value with | "" -> None | v -> Some (Int32.Parse v)
        minute = Int32.Parse g.[1].Value
        action =
            match g.[2].Value with
            | "falls" -> ActionType.Sleep
            | "wakes" -> ActionType.Wake
            | _ -> ActionType.Start
    }

type GuardRotation = { guard: int option; actions: UserAction list; }
type FoldState = { rotations: GuardRotation list; current: GuardRotation }

let emptyFoldState = { rotations = []; current = { guard = None; actions = [] }}

let extractGuardRotations (state: FoldState) (action: UserAction) =
    match action.id with
    | Some id -> {
        state with
            current = { guard = Some id; actions = [] } // start of a new guard
            rotations = // push previous guard onto list if available
                if Option.isSome state.current.guard
                then { state.current with actions = List.rev state.current.actions } :: state.rotations
                else state.rotations }
    | None -> {
        state with // prepend action onto current list
            current = { state.current with actions = action :: state.current.actions }}

let calcMinutesForGuard (rotationList: GuardRotation list) =
    let arr = Array.zeroCreate 60
    for { actions = actions } in rotationList do
        let mutable lastSleep = 0
        for m in actions do
            match m.action with
            | ActionType.Sleep -> do
                lastSleep <- m.minute
            | ActionType.Wake -> do
                for i in { lastSleep .. m.minute - 1 } do
                    arr.[i] <- arr.[i] + 1
            | _ -> ()
    arr


let calcGuardCost (guard: int option, rot: GuardRotation list) =
    let minutes = calcMinutesForGuard rot
    let totalMinutes = Array.sum minutes
    let (bestMinute, timeAsleep) = minutes |> Array.mapi (fun i a -> (i, a)) |> Array.maxBy (fun (i, a) -> a)
    (guard, totalMinutes, bestMinute, timeAsleep)

let day4 () =
    printfn "Day 4:"
    let vs = Util.readLines "day4.input" |> List.sort
    let actions = List.map parseLine vs

    let { rotations = groupedActions } = Seq.fold extractGuardRotations emptyFoldState actions

    let rotationsPerGuard = groupedActions |> List.groupBy (fun a -> a.guard)
    let guardCosts = rotationsPerGuard |> List.map calcGuardCost
    let (g0, _, b0, _) = guardCosts |> List.maxBy (fun (_, t, _, _) -> t)
    let (g1, _, b1, _) = guardCosts |> List.maxBy (fun (_, _, _, t) -> t)

    printfn "%A" (Option.get g0 * b0)
    printfn "%A" (Option.get g1 * b1)