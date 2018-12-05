module Day5

open System
open System.IO
open System.Collections.Generic

let samePolymer a b = Char.Equals(Char.ToLower a, Char.ToLower b)
let polymersWillReact a b = samePolymer a b && not (Char.Equals(a, b))

let isNotNull a = match a with | null -> false | _ -> true

let calcNumLeftInReaction text =
    // sometimes mutable algorithms are just easier, OK!?
    let ll = new LinkedList<char>(text)
    let mutable item = ll.First
    while isNotNull item && isNotNull item.Next do
        if polymersWillReact item.Value item.Next.Value then do
            let itemToRemove = item
            item <- if isNotNull item.Previous then item.Previous else item.Next.Next
            ll.Remove(itemToRemove.Next)
            ll.Remove(itemToRemove)
        else
            item <- item.Next
    ll.Count

let trialRemovingPolymer text c =
    let shorterText = text |> Seq.filter (fun a -> not (samePolymer a c)) |> String.Concat
    calcNumLeftInReaction shorterText

let getShortestWithRemoval (text: string) =
    let uniquePolymers = Set.ofSeq (text.ToLower())
    uniquePolymers |> Seq.map (fun p -> trialRemovingPolymer text p) |> Seq.min

let run () =
    printfn "Day 5:"
    let text = (File.ReadAllLines "day5.input").[0]
    
    let numLeft = calcNumLeftInReaction text
    let shortest = getShortestWithRemoval text

    printfn "%d" numLeft
    printfn "%d" shortest