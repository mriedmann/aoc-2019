// Learn more about F# at http://fsharp.org

open System
open System.Drawing

let calcIntersection (A:Point, B:Point, C:Point, D:Point) =
  let (Ax,Ay,Bx,By,Cx,Cy,Dx,Dy) =
     (A.X, A.Y, B.X, B.Y, C.X, C.Y, D.X, D.Y)
  let d = (Bx-Ax)*(Dy-Cy)-(By-Ay)*(Dx-Cx)

  if  d = 0 then
    // parallel lines ==> no intersection in euclidean plane
    None
  else
    let q = (Ay-Cy) * (Dx-Cx) - (Ax-Cx) * (Dy-Cy)
    let r = (float q) / (float d)
    let p = (Ay-Cy) * (Bx-Ax) - (Ax-Cx) * (By-Ay)
    let s = (float p) / (float d)

    if r < 0. or r > 1. or s < 0. or s > 1. then
      None // intersection is not within the line segments
    else
      Some(
        float (Ax+r*(float (Bx-Ax))),  // Px
        float (Ay+r*(By-Ay)))  // Py

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code

// s: 0,10 e: 0,10
// s: 5,10 e: 5,5