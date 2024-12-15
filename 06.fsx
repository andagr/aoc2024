open System.IO

let input = File.ReadAllLines("./06.txt")

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

let isGuard char =
    match char with
    | '^' | 'v' | '<' | '>' -> true
    | _ -> false
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
let forward guard =
    match guard.Direction with
    | Up -> { guard with Row = guard.Row - 1 }
    | Right -> { guard with Col = guard.Col + 1 }
    | Down -> { guard with Row = guard.Row + 1 }
    | Left -> { guard with Col = guard.Col - 1 }
let hasExited guard =
    let row, col = (guard.Row, guard.Col)
    row < 0 || row >= room.Length || col < 0 || col >= room[row].Length

let guard =
    room
    |> Array.findIndex (fun row -> row |> Array.exists isGuard)
    |> fun guardRow ->
        let guardCol = room[guardRow] |> Array.findIndex isGuard
        {
            Row = guardRow
            Col = guardCol
            Direction = direction room[guardRow].[guardCol]
        }

let obstacles =
    [
        for row in 0..room.Length - 1 do
            for col in 0..room[row].Length - 1 do
                if isObstacle room[row].[col] then yield (row, col)
    ]
    |> Set.ofList

let rec move obstacles guard =
    if obstacles |> Set.contains (peek guard) then guard |> turn |> move obstacles
    else forward guard

let patrol guard obstacles =
    let rec patrol' guard obstacles steps =
        if hasExited guard then Ok steps
        else
            let guard' = move obstacles guard
            if steps |> Set.contains guard' then
                Error ()
            else
                let steps' = steps |> Set.add guard
                patrol' guard' obstacles steps'
    patrol' guard obstacles Set.empty

let part1 =
    patrol guard obstacles
    |> Result.map (Set.map (fun guard -> (guard.Row, guard.Col)))
    |> Result.map Set.count

let addObstacleInPath (obstacles: Set<int * int>) (startStep: Guard) (currentStep: Guard)=
    let nextStep = move obstacles currentStep
    if (nextStep.Row, nextStep.Col) = (startStep.Row, startStep.Col) || hasExited nextStep then None
    else Some (nextStep.Row, nextStep.Col)

let part2 =
    let initialPatrol = patrol guard obstacles |> Result.defaultValue Set.empty
    initialPatrol
    |> Seq.choose (addObstacleInPath obstacles guard)
    |> Seq.choose (fun newObstacle ->
        match patrol guard (obstacles |> Set.add newObstacle) with
        | Ok _ -> None
        | Error () -> Some newObstacle)
    |> Seq.distinct
    |> Seq.length
