open System.IO
open System.Text.RegularExpressions

// let input = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
let input = File.ReadAllText("./03.txt")

Regex.Matches(input, "mul\((\d+),(\d+)\)")
|> List.ofSeq
|> List.map (fun m -> int m.Groups[1].Value * int m.Groups[2].Value)
|> List.sum