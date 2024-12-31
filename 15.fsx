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

let next (fy, fx) instruction =
    match instruction with
    | Up -> (fy - 1, fx)
    | Right -> (fy, fx + 1)
    | Down -> (fy + 1, fx)
    | Left -> (fy, fx - 1)

let exec instruction room =
    let move (fy, fx) (ty, tx) room =
        let movedObj = Array2D.get room fy fx
        room
        |> Array2D.mapi (fun y x ro ->
            if y = fy && x = fx then Empty
            elif y = ty && x = tx then movedObj
            else ro)
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

let postRoom =
    instructions
    |> List.fold
        (fun room instruction -> exec instruction room)
        room

let part1 =
    postRoom
    |> Array2D.mapi (fun y x ro ->
        match ro with
        | Box -> y * 100 + x
        | _ -> 0)
    |> Array2D.sum

type WideRoomObj =
    | WWall
    | WEmpty
    | WBoxLeft
    | WBoxRight
    | WRobot

let widen (room: RoomObj array2d) =
    Array2D.init
        (Array2D.length1 room)
        (Array2D.length2 room * 2)
        (fun y x ->
            match room[y, x/2] with
            | Wall -> WWall
            | Empty -> WEmpty
            | Box when x % 2 = 0 -> WBoxLeft
            | Box -> WBoxRight
            | Robot when x % 2 = 0 -> WRobot
            | Robot -> WEmpty)

let viewWideRoom wideRoom =
    wideRoom
    |> Array2D.map (fun wRoomObj ->
        match wRoomObj with
        | WWall -> '#'
        | WEmpty -> '.'
        | WBoxLeft -> '['
        | WBoxRight -> ']'
        | WRobot -> '@')

let execWide instruction (wideRoom: WideRoomObj array2d) =
    let wideMove (fy, fx) (ty, tx) (wideRoom: WideRoomObj array2d) =
        let movedObj = Array2D.get wideRoom fy fx
        wideRoom
        |> Array2D.mapi (fun y x ro ->
            if y = fy && x = fx then WEmpty
            elif y = ty && x = tx then movedObj
            else ro)
    let rec tryWideMove (fy, fx) (ty, tx) (implicit: bool) (wideRoom: WideRoomObj array2d) =
        let roomObj = Array2D.get wideRoom fy fx
        match roomObj with
        | WWall -> Error ()
        | WEmpty -> Ok wideRoom
        | WBoxLeft when instruction = Left || instruction = Right || implicit ->
            tryWideMove (ty, tx) (next (ty, tx) instruction) false wideRoom
            |> Result.map (wideMove (fy, fx) (ty, tx))
        | WBoxRight when instruction = Left || instruction = Right || implicit ->
            tryWideMove (ty, tx) (next (ty, tx) instruction) false wideRoom
            |> Result.map (wideMove (fy, fx) (ty, tx))
        | WRobot ->
            tryWideMove (ty, tx) (next (ty, tx) instruction) false wideRoom
            |> Result.map (wideMove (fy, fx) (ty, tx))
        | WBoxLeft ->
            tryWideMove (ty, tx) (next (ty, tx) instruction) false wideRoom
            |> Result.bind (fun lwr -> tryWideMove (fy, fx + 1) (next (fy, fx + 1) instruction) true lwr)
            |> Result.map (wideMove (fy, fx) (ty, tx))
        | WBoxRight ->
            tryWideMove (ty, tx) (next (ty, tx) instruction) false wideRoom
            |> Result.bind (fun rwr -> tryWideMove (fy, fx - 1) (next (fy, fx - 1) instruction) true rwr)
            |> Result.map (wideMove (fy, fx) (ty, tx))
    let ry, rx = wideRoom |> Array2D.findIndex WRobot
    match tryWideMove (ry, rx) (next (ry, rx) instruction) false wideRoom with
    | Error _ -> wideRoom
    | Ok movedRoom -> movedRoom


let wideRoom = widen room

let postWideRoom =
    instructions
    |> List.fold
        (fun wideRoom instruction -> execWide instruction wideRoom)
        wideRoom

let part2 =
    postWideRoom
    |> Array2D.mapi (fun y x ro ->
        match ro with
        | WBoxLeft -> y * 100 + x
        | _ -> 0)
    |> Array2D.sum
