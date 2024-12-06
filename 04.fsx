open System
open System.IO
open System.Text.RegularExpressions

let input = File.ReadAllLines("./04.txt")

let rows =
    input
    |> Array.map _.ToCharArray()

let cols = Array.transpose rows

let stepDiag rowDelta colDelta startRow startCol =
        seq { for s in 0 .. Int32.MaxValue -> (startRow + s * rowDelta, startCol + s * colDelta) }

let leftDiag (grid: char array array) =
    let rowMax = Array.length grid - 1
    let colMax = Array.length grid.[0] - 1
    let stepUpRight = stepDiag -1 1
    [|
        for i in 0 .. rowMax + colMax ->
            if i < rowMax then
                stepUpRight i 0
            else
                stepUpRight rowMax (i - rowMax)
            |> Seq.takeWhile (fun (r, c) -> r >= 0 && c <= colMax)
            |> Array.ofSeq
            |> Array.map (fun (r, c) -> grid.[r].[c])
    |]

let rightDiag (grid: 'a array array) =
    let rowMax = Array.length grid - 1
    let colMax = Array.length grid.[0] - 1
    let stepDownRight = stepDiag 1 1
    [|
        for i in 0 .. rowMax + colMax ->
            let ci = colMax - i
            if ci >= 0 then
                stepDownRight 0 ci
                |> Seq.takeWhile (fun (r, c) -> r <= rowMax && c <= colMax)
            else
                stepDownRight (i - rowMax) 0
                |> Seq.takeWhile (fun (r, c) -> r <= rowMax && c <= colMax)
            |> Array.ofSeq
            |> Array.map (fun (r, c) -> grid.[r].[c])
    |]

let countXmas (grid: char array array) =
    grid
    |> Array.map (fun row -> String(row))
    |> Array.sumBy (fun str -> Regex.Count(str, "(?=(XMAS|SAMX))"))

let part1 =
    countXmas rows +
    countXmas cols +
    (rows |> leftDiag |> countXmas) +
    (rows |> rightDiag |> countXmas)
