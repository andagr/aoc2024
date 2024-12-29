open System
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

let render (robots: Robot list) =
    let renderLine grid robot =
        let x, y = fst robot.P, snd robot.P
        Array2D.set grid y x '█'
        grid
    let grid = Array2D.create height width ' '
    robots
    |> List.fold renderLine grid

let renderMove robots =
    let rec renderMove' seconds robots frames =
        if seconds = 10001 then frames
        else
            let frame = render robots
            renderMove' (seconds + 1) (robots |> List.map move) (frame :: frames)
    renderMove' 0 robots []

let print (frames: char array2d list) =
    let printFrame (writer: StreamWriter) (frameIx: int) (frame: char array2d) =
        writer.WriteLine(frameIx)
        [ for i in 0 .. height - 1 -> frame[i, 0..] ]
        |> List.map (fun cs -> String.Join("", cs))
        |> List.iter writer.WriteLine
    use writer = File.CreateText("./robot_positions.txt")
    frames
    |> List.iteri (printFrame writer)

let part2 =
    renderMove robots
    |> List.rev
    |> print