open System
open System.IO
open Microsoft.FSharp.Core

let input = File.ReadAllLines("./08.txt") |> Array.map _.ToCharArray()

// let input =
//     """
//     ......#....#
//     ...#....0...
//     ....#0....#.
//     ..#....0....
//     ....0....#..
//     .#....A.....
//     ...#........
//     #......#....
//     ........A...
//     .........A..
//     ..........#.
//     ..........#.
//     """
//     |> _.Split('\n', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
//     |> Array.map _.ToCharArray()

(*
00 01 02 03
10 11 12 13
20 21 22 23
30 31 32 33
*)

type Antenna = {
    Frequency: char
    Row: int
    Col: int
}

let parse input =
    input
    |> Array.mapi (fun ri row ->
        row
        |> Array.mapi (fun ci col -> { Frequency = col; Row = ri; Col = ci })
        |> Array.filter (fun a -> a.Frequency <> '#' && a.Frequency <> '.'))
    |> Array.concat
    |> List.ofArray

let findAntennaSiblings antennas antenna =
    antennas |> List.filter (fun a' -> a'.Frequency = antenna.Frequency && a' <> antenna)

let findAntinodes boundRow boundCol antennas =
    let getAntinodeFor a b =
        let dRow, dCol = (b.Row - a.Row, b.Col - a.Col)
        (b.Row + dRow, b.Col + dCol)
    antennas
    |> List.collect (fun a -> (findAntennaSiblings antennas a) |> List.map (getAntinodeFor a))
    |> List.filter (fun (x, y) -> x >= 0 && x <= boundRow && y >= 0 && y <= boundCol)

let antennas = input |> parse
let antinodes =
    antennas
    |> (findAntinodes (Array.length input - 1) (Array.length input[0] - 1))
    |> List.distinct

[| for ri in 0 .. input.Length - 1 ->
    [| for ci in 0 .. input[0].Length ->
         antennas |> List.tryFind (fun a -> a.Row = ri && a.Col = ci) |> Option.map _.Frequency
         |> Option.orElse (antinodes |> List.tryFind (fun an -> an = (ri, ci)) |> Option.map (fun _ -> '#'))
         |> Option.defaultValue '.'
         |] |]
|> Array.map (fun row -> String(row))
|> (fun lines -> String.Join('\n', lines))
|> printfn "%A"

let part1 = antinodes |> List.length
