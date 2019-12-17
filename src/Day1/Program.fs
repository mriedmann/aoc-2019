open System
open System.IO

let readValues (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield (sr.ReadLine () |> int) 
}

let calcFuel (mass : int) =
  let x = float mass / 3.0
  int x - 2

let rec calcFuel2 (mass : int) = seq {
  let y = calcFuel mass
  match y with
  | i when i > 0 ->
    yield y
    yield! calcFuel2 y
  | _ ->
    ()
}

let calcFuel3 (mass : int) = 
  calcFuel2 mass |> Seq.sum
  

[<EntryPoint>]
let main argv =
    assert ((12 |> calcFuel) = 2)
    assert ((14 |> calcFuel) = 2)
    assert ((1969 |> calcFuel) = 654)
    assert ((100756 |> calcFuel) = 33583)
    let result1 = Seq.sumBy calcFuel (readValues "input.txt")
    assert (result1 = 3457281)
    result1 |> printfn "Result: %d"
    
    assert ((14 |> calcFuel3) = 2)
    assert ((1969 |> calcFuel3) = 966)
    assert ((100756 |> calcFuel3) = 50346)
    let result2 = Seq.sumBy calcFuel3 (readValues "input.txt")
    assert (result2 = 5183030)
    result2 |> printfn "Result2: %d"
    
    0 // return an integer exit code
