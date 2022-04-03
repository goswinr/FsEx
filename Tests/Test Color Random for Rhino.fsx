#r @"D:\Git\FsEx\bin\Release\netstandard2.0\FsEx.dll"
#r @"C:\Program Files\Rhino 7\System\RhinoCommon.dll"
#r @"D:\Git\Rhino.Scripting\bin\Release\net48\Rhino.Scripting.dll"
#r @"D:\Git\Rhino.Scripting.Extension\bin\Release\net48\Rhino.Scripting.Extension.dll"


open System
open FsEx
open Rhino.Geometry
open Rhino.Scripting.Extension
open UtilMath
type rs = Rhino.Scripting


  
do
    rs.DisableRedraw() 
    
    let rnd = Random() 
    
    let mutable lastHue = 0.0
    let mutable lumUp = false
    
    let rec randomColorForRhino() = 
        lastHue <- lastHue + 0.6180334 // golden angle conjugate
        lastHue <- lastHue % 1.0 // loop it between 0.0 and 1.0
    
        let mutable s = rnd.NextDouble() 
        s <- s*s*s*s      //  0.0 - 1.0 increases the probalility that the number is small( )
        s <- s*0.7    //  0.0 - 0.7 make sure it is less than 0.6
        s <- 1.1 - s  //  1.2 - 0.5  
        s <- clamp01 s
        
        let mutable l = rnd.NextDouble() 
        l <- l*l     //  0.0 - 1.0 increases the probalility that the number is small( )
        l <- l*0.35* s   //  0.0 - 0.25 make sure it is less than 0.6
        l <- if lumUp then lumUp<-false;  0.5+l*1.1 else lumUp<-true ;  0.5-l //alternate luminace up or down,  mor on the bright side
        
        if l > 0.3 && lastHue > 0.10 && lastHue < 0.22 then  // exlude yellow
            randomColorForRhino() 
        else    
            Color.fromHSL(lastHue, s, l)
    
    let cubesize = 0.03
    let makeHslCube (h, s, l )= 
        let col = Color.fromHSL(h, s, l)
        let a = Point3d(h, s, l)
        let sc = 1.0
        let size = Vector3d(sc*cubesize,sc*cubesize,sc*cubesize )
        BoundingBox(a, a|> RhPnt.translate size)
        |> rs.Add 
        |>! rs.setColor col
        
            
    for i=0 to 2000 do  
        let col = randomColorForRhino() 
        //let col = Color.randomForRhino() 
        
        let li = rs.AddLayer($"rnd::{i}", col)
        let h, l, s = Color.HSLfromColor(col)
        rs.ObjectLayer(makeHslCube(h, l, s), li)
        