// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Text.RegularExpressions;

let readLines fname = File.ReadLines(fname) |> List.ofSeq 

// ==== Day 1 ==== //
let rec getFirstCumulativeRepeat' vs acc idx seenFreq =
    match Set.contains acc seenFreq with
    | true -> acc
    | false ->
        let newAcc = acc + (Array.item idx vs)
        let newIdx = (idx + 1) % Array.length vs
        getFirstCumulativeRepeat' vs newAcc newIdx (Set.add acc seenFreq)

let getFirstCumulativeRepeat vs = getFirstCumulativeRepeat' vs 0 0 Set.empty
let day1 () =
    printfn "Day 1:"
    let vs = readLines "day1.input" |> List.map Int32.Parse
    printfn "%d" (List.sum vs)
    printfn "%d" (getFirstCumulativeRepeat (Array.ofList vs))

// ==== Day 2 ==== //
let numOfCharInString c s = Seq.length (Seq.filter (fun c' -> c' = c) s)
let hasRepeat n s = Seq.exists (fun c -> numOfCharInString c s = n) (Set.ofSeq s)
let numWithN n ss = Seq.filter (hasRepeat n) ss |> Seq.length

let hasOneLetterDifferent (a, b) =
    Seq.zip a b
    |> Seq.filter (fun (x, y) -> x <> y)
    |> Seq.length = 1

let commonLetters (a, b) =
    Seq.zip a b
    |> Seq.filter (fun (x, y) -> x = y)
    |> Seq.map (fun (x, _) -> x)

let day2 () =
    printfn "Day 2:"
    let vs = readLines "day2.input"
    let numWith2 = numWithN 2 vs
    let numWith3 = numWithN 3 vs
    printfn "%d * %d = %d" numWith2 numWith3 (numWith2 * numWith3)
    let product = seq { for a in vs do for b in vs do yield a, b }
    let result =
        Seq.find hasOneLetterDifferent product
        |> commonLetters
        |> String.Concat
    printfn "%s" result

// ==== Day 3 ==== //

type Region = { id: string; left: int; top: int; width: int; height: int }
let parseLine line =
    let m = Regex.Match(line, @"^#([^ ]+) @ (\d+),(\d+): (\d+)x(\d+)$")
    let g = m.Groups
    {
        id = g.[1].Value
        left = Int32.Parse g.[2].Value
        top = Int32.Parse g.[3].Value
        width = Int32.Parse g.[4].Value
        height = Int32.Parse g.[5].Value
    }

let iterRegion size { left=l; top=t; width=w; height=h } = seq {
    for i in { l .. l + w - 1 } do
        for j in { t .. t + h - 1 } do
            yield j * size + i
    }

let createFabric size regions =
    let fabric: int[] = Array.zeroCreate (size * size)
    for region in regions do
        iterRegion size region |> Seq.iter
            (fun i -> fabric.[i] <- fabric.[i] + 1)
    fabric

let countOverlapping fabric =
    Array.fold (fun count x -> count + (if x > 1 then 1 else 0)) 0 fabric

let regionWithOverlap size (fabric: int[]) region =
    iterRegion size region |> Seq.forall (fun i -> fabric.[i] = 1)

let day3 () =
    printfn "Day 3:"
    let size = 1000 
    let vs = readLines "day3.input" |> List.map parseLine
    let fabric = createFabric size vs
    let numOverlap = countOverlapping fabric
    printfn "%A" numOverlap
    let region = List.find (regionWithOverlap size fabric) vs
    printfn "#%s" region.id

[<EntryPoint>]
let main argv =
    printfn "Executing main"
    // day1 ()
    // day2 ()
    // day3 ()
    // Day4.run ()
    Day5.run ()
    0