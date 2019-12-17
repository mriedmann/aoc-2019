open System.IO

type Opcode = 
  | Add=1 
  | Mult=2 
  | Halt=99 

type Command =
  | ValidCommand of Opcode * int * int * int
  | InvalidCommand of Opcode

type Scenario = { noun:int; verb:int; result:int }

let readValues (filePath:string) = seq {
  use sr = new StreamReader (filePath)
  while not sr.EndOfStream do
    for i in ((sr.ReadLine ()).Split ',') do
      yield (i |> int)
}
  
let getCommand (input:int array) (pointer:int) =
  let opcode:Opcode = enum input.[pointer]
  let get (pos:int) = Array.get input pos
  match opcode with
  | Opcode.Add | Opcode.Mult | Opcode.Halt -> 
    ValidCommand (opcode, get (pointer+1), get (pointer+2), get (pointer+3))
  | _ -> InvalidCommand opcode

let executeCalcCommand (input:int array) opcode xi yi di =
  let x = Array.get input xi
  let y = Array.get input yi
  let set = fun result -> Array.set input di result
  match opcode with
  | Opcode.Add -> set (x + y)
  | Opcode.Mult -> set (x * y)
  | _ -> failwith "Invalid Opcode"
  

let executeCommand (input:int array) (pointer:int) (cmd: Command) =
  match cmd with
  | ValidCommand (Opcode.Halt, _, _, _) ->
    true
  | ValidCommand (opcode, xi, yi, di) ->
    executeCalcCommand input opcode xi yi di
    false
  | _ -> 
    failwith "Invalid Command"

let nextPointerPosition (pointer:int) =
    pointer + 4

let rec executeProgram (input:int array) (pointer:int) =
  let halt = getCommand input pointer |> executeCommand input pointer
  
  if halt then
    ()
  else
    executeProgram input (nextPointerPosition pointer)

let runProgram (program: int array) (noun: int) (verb: int) =
  let input = Array.copy program
  Array.set input 1 noun
  Array.set input 2 verb
  executeProgram input 0
  {noun=noun; verb=verb; result=input.[0]}

[<EntryPoint>]
let main argv =
  let program = Seq.toArray (readValues "input.txt")

  [for x in 0..99 do
   for y in 0..99 do
   yield {noun=x; verb=y; result=0}]
  |> List.map (fun x -> runProgram program x.noun x.verb) 
  |> List.tryFind (fun x -> x.result = 19690720) 
  |> (fun x -> 
      match x with
      | Some(x) -> ((x.noun * 100 + x.verb), x.result)
      | None -> failwith "no solution found")
  |> fun i -> printfn "%A" i
  |> ignore

  0 // return an integer exit code
