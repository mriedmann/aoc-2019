open System

// Helper
let check2 (f1: 'a -> bool) (f2: 'a -> bool) (x: 'a) =
  (f1 x) && (f2 x)

let digitize (i: int) =
  string i |> Seq.map (fun x -> (int x) - 48) |> Seq.toList

let getNumCount (f)=
  Seq.filter f >> Seq.length

// Part 1
let hasDoubleDigit (x: int list) =
  Seq.exists2 (=) x (x.[1..])

let isStrictlyIncreaing (x: int list) =
  not (Seq.exists2 (>) x (x.[1..]))

let isPossibleSolutionEasy(i: int) =
  digitize i |> check2 hasDoubleDigit isStrictlyIncreaing

// Part 2
let rec getSameDigitSegments (x: int list list) (m: int list)  =
  let segmentLength = m |> List.takeWhile (fun e -> e = (Seq.head m)) |> List.length
  let segment, rest = m |> List.splitAt segmentLength
  let segments = segment :: x
  if (Seq.isEmpty rest) then
    segments
  else
    getSameDigitSegments segments rest

let getDigitSegmentLengths (x: int list) =
  x |> getSameDigitSegments (Seq.toList Seq.empty) |> List.map Seq.length

let isPossibleSolution (f) (i: int) =
  digitize i |> check2 (getDigitSegmentLengths >> List.exists f) isStrictlyIncreaing


[<EntryPoint>]
let main argv =  
  let input = [387638..919123]
  input |> getNumCount isPossibleSolutionEasy |> printfn "Part1: %d  (old)"
  input |> getNumCount (isPossibleSolution (fun x -> x >= 2)) |> printfn "Part1: %d"
  input |> getNumCount (isPossibleSolution (fun x -> x = 2)) |> printfn "Part2: %d"
  0
