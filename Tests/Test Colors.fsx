#r @"D:\Git\FsEx\bin\Release\netstandard2.0\FsEx.dll"

open System
open FsEx

let rnd = Random()


let test (r, g, b) = 
    let c = Color(r, g, b)
    
    let h, s, l = Color.toHSL c
    
    let c' = Color.fromHSL(h, s, l)
    
    c'=c |> failIfFalse "bang"


test (0uy, 0uy, 0uy)
test (1uy, 1uy, 1uy)
test (255uy, 0uy, 0uy)
test (0uy, 255uy, 0uy)
test (0uy, 0uy, 255uy)

test (255uy, 255uy, 0uy)
test (255uy, 0uy, 255uy)
test (0uy, 255uy, 255uy)

test (255uy, 255uy, 255uy)
test (254uy, 254uy, 254uy)



for i=0 to 9999 do 
    let r = rnd.Next(255)|> byte    
    let g = rnd.Next(255)|> byte
    let b = rnd.Next(255)|> byte
    
    test(r, g, b)
    
    