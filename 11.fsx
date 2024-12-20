let input = "64554 35 906 6 6960985 5755 975820 0" |> _.Split(" ")

let stones = input |> Array.map int64 |> List.ofArray

let (|IsEvenDigits|_|) stone =
    let str = string stone
    let length = String.length str
    if length % 2 = 0 then Some [int64 (str.Substring(0, length / 2)); int64 (str.Substring(length / 2, length / 2))]
    else None

let blink times stones =
    let blinkOnce stone =
        match stone with
        | 0L -> [1L]
        | IsEvenDigits v ->  v
        | v -> [v * 2024L]
    let rec blink' times stones =
        if times = 0 then stones
        else stones |> List.collect blinkOnce |> blink' (times - 1)
    blink' times stones

let part1 = blink 25 stones |> List.length
