open System.IO
open System.Text.RegularExpressions

let input = File.ReadLines("./14.txt")

type Robot = {
    P: int * int
    V: int * int
}

let width, height = 101, 103

let robots =
    input
    |> List.ofSeq
    |> List.map (fun i ->
        let pm = Regex.Match(i, "p=(\d+),(\d+)")
        let vm = Regex.Match(i, "v=(-?\d+),(-?\d+)")
        {
          P = (int pm.Groups[1].Value, int pm.Groups[2].Value)
          V = (int vm.Groups[1].Value, int vm.Groups[2].Value)
        })

let move robot =
    let x = (fst robot.P + fst robot.V) % width
    let x =
        if x < 0 then width + x
        else x
    let y = (snd robot.P + snd robot.V) % height
    let y =
        if y < 0 then height + y
        else y
    { robot with P = (x, y) }

let rec moveTimes times robot =
    if times <= 0 then robot
    else moveTimes (times - 1) (move robot)

let quadrant robot =
    match robot.P with
    | x, y when
        x >= 0 && x < width / 2 &&
        y >= 0 && y < height / 2 -> Some 1
    | x, y when
        x > width / 2 && x < width &&
        y >= 0 && y < height / 2 -> Some 2
    | x, y when
        x >= 0 && x < width / 2 &&
        y > height / 2 && y < height -> Some 3
    | x, y when
        x > width / 2 && x < width &&
        y > height / 2 && y < height -> Some 4
    |_ -> None


let part1 =
    robots
    |> List.map (moveTimes 100)
    |> List.groupBy quadrant
    |> List.choose (fun (q, rs) ->
        match q with
        | None -> None
        | Some _ -> Some (List.length rs))
    |> List.reduce (*)
