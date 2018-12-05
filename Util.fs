module Util

open System.IO
let readLines fname = File.ReadLines(fname) |> List.ofSeq 