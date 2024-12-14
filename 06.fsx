open System
open System.IO

let input = File.ReadAllLines("./06.txt")

// let input =
//     """
//     ....#.....
//     .........#
//     ..........
//     ..#.......
//     .......#..
//     ..........
//     .#..^.....
//     ........#.
//     #.........
//     ......#...
//     """ |> _.Split("\n", StringSplitOptions.RemoveEmptyEntries)

let room = input |> Array.map _.ToCharArray()

type Direction =
    | Up
    | Down
    | Left
    | Right

type Guard = {
    Row: int
    Col: int
    Direction: Direction
}

let isGuard char = char = '^' || char = 'v' || char = '<' || char = '>'
let isObstacle char = char = '#'
let direction char =
    match char with
    | '^' -> Up
    | 'v' -> Down
    | '<' -> Left
    | '>' -> Right
    | _ -> failwith "Invalid direction"
let turn guard =
    match guard.Direction with
    | Up -> { guard with Direction = Right }
    | Right -> { guard with Direction = Down }
    | Down -> { guard with Direction = Left }
    | Left -> { guard with Direction = Up }
let peek guard =
    match guard.Direction with
    | Up -> (guard.Row - 1, guard.Col)
    | Right -> (guard.Row, guard.Col + 1)
    | Down -> (guard.Row + 1, guard.Col)
    | Left -> (guard.Row, guard.Col - 1)
let move guard =
    match guard.Direction with
    | Up -> { guard with Row = guard.Row - 1 }
    | Right -> { guard with Col = guard.Col + 1 }
    | Down -> { guard with Row = guard.Row + 1 }
    | Left -> { guard with Col = guard.Col - 1 }
let hasExited guard =
    let row, col = (guard.Row, guard.Col)
    row < 0 || row >= room.Length || col < 0 || col >= room.[row].Length

let guard =
    room
    |> Array.findIndex (fun row -> row |> Array.exists isGuard)
    |> fun guardRow ->
        let guardCol = room[guardRow] |> Array.findIndex isGuard
        {
            Row = guardRow
            Col = guardCol
            Direction = direction room.[guardRow].[guardCol]
        }

let obstacles =
    [
        for row in 0..room.Length - 1 do
            for col in 0..room.[row].Length - 1 do
                if isObstacle room[row].[col] then yield (row, col)
    ]
    |> Set.ofList

let rec steps guard acc =
    if hasExited guard then acc |> List.distinct |> List.length
    else
        let acc' = (guard.Row, guard.Col) :: acc
        let guard' =
            if obstacles |> Set.contains (peek guard) then guard |> turn |> move
            else move guard
        steps guard' acc'

steps guard []