open System
open System.IO
open System.Text.RegularExpressions

let input = File.ReadAllLines("./04.txt")

let rows =
    input
    |> Array.map _.ToCharArray()

let cols = Array.transpose rows

let skewLeft grid =
    [|for ri in 0 .. Array.length grid - 1 ->
          grid[ri]
          |> Array.insertManyAt (Array.length grid[ri]) (Array.create (Array.length grid[ri] - 1 - ri) ' ')
          |> Array.insertManyAt 0 (Array.create ri ' ')|]
let diagsTopRight =
    skewLeft rows
    |> Array.transpose

let skewRight grid =
    [|for ri in 0 .. Array.length grid - 1 ->
          grid[ri]
          |> Array.insertManyAt (Array.length grid[ri]) (Array.create ri ' ')
          |> Array.insertManyAt 0 (Array.create (Array.length grid[ri] - 1 - ri) ' ')|]
let diagsTopLeft =
    skewRight rows
    |> Array.transpose

let countXmas (grid: char array array) =
    grid
    |> Array.map (fun row -> String(row))
    |> Array.sumBy (fun str -> Regex.Count(str, "(?=(XMAS|SAMX))"))

let part1 =
    countXmas rows +
    countXmas cols +
    countXmas diagsTopRight +
    countXmas diagsTopLeft