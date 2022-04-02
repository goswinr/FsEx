#r @"D:\Git\FsEx\bin\Release\netstandard2.0\FsEx.dll"

open System
open FsEx

type MyRecord =  
    {
    name:string
    number:int
    de:string 
    }
    
do
    
    
    
    let mk(s) = {
        name=s
        number=2
        de="hghjg"
        }
    
    let xs = [| 
        mk"""a-000000000000000000000001111111111
        111112222222222222233333333333333333333
        333444444444444444444444444455555550000
        0000000000000000001111111111111112222222
        22222223333333333333333333333344444444444
        44444444444444555555500000000000000000000
        001111111111111112222222222222233333333333
        333333333333444444444444444444444444455555
        5500000000000000000000001111111111111112222
        2222222222333333333333333333333334444444444
        4444444444444445555555000000000000000000000
        0111111111111111222222222222223333333333333
        33333333334444444444444444444444444555555555
        55555555555555555555555555666666666666666666666666666666"""
        mk "b-00000000000000000000000"
        
        mk "c-00000000000000000000000"
        mk "d-00000000000000000000000"
        mk "e-00000000000000000000000"
        mk "f-00000000000000000000000"
        mk "g-00000000000000000000000"
        mk "h-00000000000000000000000"
        mk "i-00000000000000000000000"
        mk "j-00000000000000000000000"
        |]
    
    let cs = [| 
        ["a"]
        ["b"] 
        ["c"]
        ["d"]
        ["e"]
        ["f"]
        ["g"]
        ["h"]
        ["i"]
        ["j"]
        |]    
    let xxs=[| 
        xs
        xs
        xs
        xs
        xs
        xs
        xs
        xs
        xs
        xs 
        |]    
        
    let pl xs = eprintf "Seq.Length = %d: " (Seq.length xs)   ;  xs 
        
    clearSeffLog() 
    let m = mk("Gos")
    cs |> Array.truncate 3  |> Seq.ofArray|> pl |> print
    cs |> Array.truncate 4  |> Seq.ofArray|> pl |> print
    cs |> Array.truncate 5  |> Seq.ofArray|> pl |> print
    cs |> Array.truncate 6  |> Seq.ofArray|> pl |> print
    cs |> Array.truncate 7  |> Seq.ofArray|> pl |> print
    cs |> Array.truncate 8  |> Seq.ofArray|> pl |> print
    cs |> Array.truncate 9  |> Seq.ofArray|> pl |> print
    cs |> Array.truncate 10 |> Seq.ofArray|> pl |> print
    
    
    xs |> Array.truncate 3  |> pl |> print
    xs |> Array.truncate 4  |> pl |> print
    xs |> Array.truncate 5  |> pl |> print
    xs |> Array.truncate 6  |> pl |> print
    xs |> Array.truncate 7  |> pl |> print
    xs |> Array.truncate 8  |> pl |> print
    xs |> Array.truncate 9  |> pl |> print
    xs |> Array.truncate 10 |> pl |> print
    