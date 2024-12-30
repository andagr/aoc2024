fsi.PrintWidth <- 300

open System
open System.IO

module Array2D =
    let findIndex item a2d =
        let rec findIndex' y x =
            match Array2D.get a2d y x with
            | i when i = item -> y, x
            | _ ->
                let y', x' =
                    if x = (Array2D.length2 a2d - 1) then (y + 1, 0)
                    else (y, x + 1)
                findIndex' y' x'
        findIndex' 0 0

    let sum a2d =
        let rec sum' y x value =
            if y = (Array2D.length1 a2d) then value
            elif x = (Array2D.length2 a2d - 1) then sum' (y + 1) 0 (value + Array2D.get a2d y x)
            else sum' y (x + 1) (value + Array2D.get a2d y x)
        sum' 0 0 0

// let input =
//     """
//     ##########
//     #..O..O.O#
//     #......O.#
//     #.OO..O.O#
//     #..O@..O.#
//     #O#..O...#
//     #O..O..O.#
//     #.OO.O.OO#
//     #....O...#
//     ##########
//
//     <vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
//     vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
//     ><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
//     <<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
//     ^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
//     ^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
//     >^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
//     <><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
//     ^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
//     v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
//     """

let input = File.ReadAllText("./15.txt")

type RoomObj =
    | Wall
    | Empty
    | Box
    | Robot

type Instruction =
    | Up
    | Right
    | Down
    | Left

let parseRoomObj c =
    match c with
    | '#' -> Wall
    | '.' -> Empty
    | 'O' -> Box
    | '@' -> Robot
    | _ -> failwith $"Unknown room obj: {c}"

let parseInstruction c =
    match c with
    | '^' -> Some Up
    | '>' -> Some Right
    | 'v' -> Some Down
    | '<' -> Some Left
    | _ -> None

let parseRoom (inputRoom: string) =
    let roomGrid =
        inputRoom
        |> _.Split("\n", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
        |> Array.map _.ToCharArray()
    Array2D.init
        (Array.length roomGrid)
        (Array.length roomGrid[0])
        (fun y x -> parseRoomObj (roomGrid[y][x]))

let viewRoom room =
    room
    |> Array2D.map (fun roomObj ->
        match roomObj with
        | Wall -> '#'
        | Empty -> '.'
        | Box -> 'O'
        | Robot -> '@')

let parseInstructions (inputInstructions: string) =
    inputInstructions
    |> _.ToCharArray()
    |> List.ofArray
    |> List.choose parseInstruction

let room, instructions =
    input
    |> _.Split("\n\n", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
    |> fun parts -> parseRoom parts[0], parseInstructions parts[1]

let exec instruction room =
    let move (fy, fx) (ty, tx) room =
        let movedObj = Array2D.get room fy fx
        room
        |> Array2D.mapi (fun y x ro ->
            if y = fy && x = fx then Empty
            elif y = ty && x = tx then movedObj
            else ro)
    let next (fy, fx) instruction =
        match instruction with
        | Up -> (fy - 1, fx)
        | Right -> (fy, fx + 1)
        | Down -> (fy + 1, fx)
        | Left -> (fy, fx - 1)
    let rec tryMove (fy, fx) (ty, tx) room =
        let roomObj = Array2D.get room fy fx
        match roomObj with
        | Wall -> Error ()
        | Empty -> Ok room
        | Box
        | Robot ->
            tryMove (ty, tx) (next (ty, tx) instruction) room
            |> Result.map (move (fy, fx) (ty, tx))
    let ry, rx = room |> Array2D.findIndex Robot
    match tryMove (ry, rx) (next (ry, rx) instruction) room with
    | Error _ -> room
    | Ok movedRoom -> movedRoom

viewRoom room
let postRoom =
    instructions
    |> List.fold
        (fun room instruction -> exec instruction room)
        room
viewRoom postRoom
let part1 =
    postRoom
    |> Array2D.mapi (fun y x ro ->
        match ro with
        | Box -> y * 100 + x
        | _ -> 0)
    |> Array2D.sum