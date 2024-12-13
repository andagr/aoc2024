open System
open System.IO
open System.Text.RegularExpressions

let input = File.ReadAllLines("./04.txt")

type Letter = { Row: int; Col: int; Value: char }

let rows =
    input
    |> Array.mapi (fun r line -> line.ToCharArray() |> Array.mapi (fun c v -> { Row = r; Col = c; Value = v }))


let cols = Array.transpose rows

let stepDiag rowDelta colDelta startRow startCol =
        seq { for s in 0 .. Int32.MaxValue -> (startRow + s * rowDelta, startCol + s * colDelta) }

let leftDiag (grid: Letter array array) =
    let rowMax = Array.length grid - 1
    let colMax = Array.length grid[0] - 1
    let stepUpRight = stepDiag -1 1
    [|
        for i in 0 .. rowMax + colMax ->
            if i < rowMax then
                stepUpRight i 0
            else
                stepUpRight rowMax (i - rowMax)
            |> Seq.takeWhile (fun (r, c) -> r >= 0 && c <= colMax)
            |> Array.ofSeq
            |> Array.map (fun (r, c) -> grid[r][c])
    |]

let rightDiag (grid: Letter array array) =
    let rowMax = Array.length grid - 1
    let colMax = Array.length grid[0] - 1
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
            |> Array.map (fun (r, c) -> grid[r][c])
    |]

let countXmas (grid: Letter array array) =
    grid
    |> Array.map (fun line -> String(line |> Array.map _.Value))
    |> Array.sumBy (fun str -> Regex.Count(str, "(?=(XMAS|SAMX))"))

let part1 =
    countXmas rows +
    countXmas cols +
    (rows |> leftDiag |> countXmas) +
    (rows |> rightDiag |> countXmas)

let findMasCenters (grid: Letter array) =
    if Array.length grid < 3 then
        [||]
    else
        [| for ci in 1 .. (Array.length grid - 2) do
               if (grid[ci - 1].Value = 'M' && grid[ci].Value = 'A' && grid[ci + 1].Value = 'S') ||
                  (grid[ci - 1].Value = 'S' && grid[ci].Value = 'A' && grid[ci + 1].Value = 'M') then
                    yield grid[ci] |]

let leftDiagMasCenters =
    rows
    |> leftDiag
    |> Array.map findMasCenters
    |> Array.concat

let rightDiagMasCenters =
    let rightDiagRows =
        rows
        |> rightDiag
    rightDiagRows
    |> Array.map findMasCenters
    |> Array.concat

let part2 =
    leftDiagMasCenters
    |> Array.filter (fun l -> rightDiagMasCenters |> Array.exists (fun r -> l.Row = r.Row && l.Col = r.Col))
    |> Array.length