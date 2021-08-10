// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

// Various tests for the:
// Microsoft.FSharp.Collections.Array module

namespace FSharp.Core.UnitTests.Collections

open System
open FSharp.Core.UnitTests.LibraryTestFx
open Xunit
open FsEx

(*
[Test Strategy]
Make sure each method works on:
* Integer array (value type)
* String  array (reference type)
* Empty   array (0 elements)
* Null    array (null)
*)
    

type RarrWindowedTestInput<'t> =
    {
        InputArray : 't Rarr
        WindowSize : int
        ExpectedArray : 't Rarr Rarr
        Exception : Type option
    }

type TestRarrModule2() =

    [<Fact>]
    member this.Length() =
        // integer array  
        let resultInt = Rarr.length [|1..8 |].asRarr
        if resultInt <> 8 then Assert.Fail()
        
        // string array    
        let resultStr = Rarr.length [|"Lists"; "are";  "commonly" ; "list"  |].asRarr
        if resultStr <> 4 then Assert.Fail()
        
        // empty array     
        let resultEpt = Rarr.length [|  |].asRarr
        if resultEpt <> 0 then Assert.Fail()

        // null array
        // Rarr cant be null: let nullArr = null:string[]      
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.length  nullArr  |> ignore)  
        
        // null array, argument name showing up
        //try
        //    Rarr.length nullArr |> ignore
        //with 
        //| :? ArgumentNullException as e -> Assert.Equal("array", e.ParamName) |> ignore

        ()

    [<Fact>]
    member this.Indexed() =
        // integer array
        let resultInt = Rarr.indexed [|10..2..20 |].asRarr
        Assert.AreEqual([|(0,10);(1,12);(2,14);(3,16);(4,18);(5,20) |].asRarr, resultInt)

        // string array
        let funcStr (x:int) (y:string) =  x+ y.Length
        let resultStr = Rarr.indexed [| "Lists"; "Are"; "Commonly"; "List"  |].asRarr
        Assert.AreEqual([| (0,"Lists");(1,"Are");(2,"Commonly");(3,"List")  |].asRarr, resultStr)

        // empty array
        let emptyArr:int Rarr = [|  |].asRarr
        let resultEpt = Rarr.indexed emptyArr
        Assert.AreEqual([|  |].asRarr, resultEpt)

        // null array
        // Rarr cant be null: let nullArr = null:string[]
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.indexed nullArr |> ignore)

        ()

    [<Fact>]
    member this.Map() = 
        // integer array
        let funcInt x = 
                match x with
                | _ when x % 2 = 0 -> 10*x            
                | _ -> x
        let resultInt = Rarr.map funcInt [| 1..10  |].asRarr
        if resultInt <!> [|1;20;3;40;5;60;7;80;9;100 |].asRarr then Assert.Fail()
        
        // string array
        let funcStr (x:string) = x.ToLower()
        let resultStr = Rarr.map funcStr [|"Lists"; "Are";  "Commonly" ; "List"  |].asRarr
        if resultStr <!> [|"lists"; "are";  "commonly" ; "list"  |].asRarr then Assert.Fail()
        
        // empty array
        let resultEpt = Rarr.map funcInt [|  |].asRarr
        if resultEpt <!> [|  |].asRarr then Assert.Fail()

        // null array
        // Rarr cant be null: let nullArr = null:string[]      
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.map funcStr nullArr |> ignore)
        
        ()

    [<Fact>]
    member this.Map2() = 
        // integer array 
        let funcInt x y = x+y
        let resultInt = Rarr.map2 funcInt [|1..10 |].asRarr [|2..2..20 |].asRarr
        if resultInt <!> [|3;6;9;12;15;18;21;24;27;30 |].asRarr then Assert.Fail()
        
        // string array
        let funcStr (x:int) (y:string) =  x+ y.Length
        let resultStr = Rarr.map2 funcStr [|3;6;9;11 |].asRarr [|"Lists"; "Are";  "Commonly" ; "List"  |].asRarr
        if resultStr <!> [|8;9;17;15 |].asRarr then Assert.Fail()
        
        // empty array
        let emptyArr:int Rarr = [|  |].asRarr
        let resultEpt = Rarr.map2 funcInt emptyArr emptyArr
        if resultEpt <!> [|  |].asRarr then Assert.Fail()

        // null array
        // Rarr cant be null: let nullArr = null:int[]
        let validArray = [| 1  |].asRarr       
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.map2 funcInt nullArr validArray |> ignore)  
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.map2 funcInt validArray nullArr |> ignore)  
        
        // len1 <!> len2
        CheckThrowsArgumentException(fun () -> Rarr.map2 funcInt [|1..10 |].asRarr [|2..20 |].asRarr |> ignore)
        
        ()

    [<Fact>]
    member this.Map3() =
        // Integer array
        let funcInt a b c = (a + b) * c
        let resultInt = Rarr.map3 funcInt [| 1..8  |].asRarr [| 2..9  |].asRarr [| 3..10  |].asRarr
        if resultInt <!> [| 9; 20; 35; 54; 77; 104; 135; 170  |].asRarr then Assert.Fail()

        // First array is shorter
        CheckThrowsArgumentException (fun () -> Rarr.map3 funcInt [| 1..2  |].asRarr [| 2..9  |].asRarr [| 3..10  |].asRarr |> ignore)
        // Second array is shorter
        CheckThrowsArgumentException (fun () -> Rarr.map3 funcInt [| 1..8  |].asRarr [| 2..6  |].asRarr [| 3..10  |].asRarr |> ignore)
        // Third array is shorter
        CheckThrowsArgumentException (fun () -> Rarr.map3 funcInt [| 1..8  |].asRarr [| 2..9  |].asRarr [| 3..6  |].asRarr |> ignore)
        
        // String array
        let funcStr a b c = a + b + c
        let resultStr = Rarr.map3 funcStr [| "A";"B";"C";"D"  |].asRarr [| "a";"b";"c";"d"  |].asRarr [| "1";"2";"3";"4"  |].asRarr
        if resultStr <!> [| "Aa1";"Bb2";"Cc3";"Dd4"  |].asRarr then Assert.Fail()

        // Empty array
        let resultEmpty = Rarr.map3 funcStr [| |].asRarr [| |].asRarr [| |].asRarr
        if resultEmpty <!> [| |].asRarr then Assert.Fail()

        // Null array
        // Rarr cant be null: let nullArray = null : int[]
        let nonNullArray = [|1 |].asRarr
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.map3 funcInt nullArray nonNullArray nonNullArray |> ignore)
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.map3 funcInt nonNullArray nullArray nonNullArray |> ignore)
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.map3 funcInt nonNullArray nonNullArray nullArray |> ignore)

        ()

    [<Fact>]
    member this.MapFold() =
        // integer array
        let funcInt acc x = if x % 2 = 0 then 10*x, acc + 1 else x, acc
        let resultInt,resultIntAcc = Rarr.mapFold funcInt 100 [| 1..10  |].asRarr
        if resultInt <!> [| 1;20;3;40;5;60;7;80;9;100  |].asRarr then Assert.Fail()
        Assert.AreEqual(105, resultIntAcc)

        // string array
        let funcStr acc (x:string) = match x.Length with 0 -> "empty", acc | _ -> x.ToLower(), sprintf "%s%s" acc x
        let resultStr,resultStrAcc = Rarr.mapFold funcStr "" [| "";"BB";"C";""  |].asRarr
        if resultStr <!> [| "empty";"bb";"c";"empty"  |].asRarr then Assert.Fail()
        Assert.AreEqual("BBC", resultStrAcc)

        // empty array
        let resultEpt,resultEptAcc = Rarr.mapFold funcInt 100 [|  |].asRarr
        if resultEpt <!> [|  |].asRarr then Assert.Fail()
        Assert.AreEqual(100, resultEptAcc)

        // null array
        // Rarr cant be null: let nullArr = null:string[]
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.mapFold funcStr "" nullArr |> ignore)

        ()

    [<Fact>]
    member this.MapFoldBack() =
        // integer array
        let funcInt x acc = if acc < 105 then 10*x, acc + 2 else x, acc
        let resultInt,resultIntAcc = Rarr.mapFoldBack funcInt [| 1..10  |].asRarr 100
        if resultInt <!> [| 1;2;3;4;5;6;7;80;90;100  |].asRarr then Assert.Fail()
        Assert.AreEqual(106, resultIntAcc)

        // string array
        let funcStr (x:string) acc = match x.Length with 0 -> "empty", acc | _ -> x.ToLower(), sprintf "%s%s" acc x
        let resultStr,resultStrAcc = Rarr.mapFoldBack funcStr [| "";"BB";"C";""  |].asRarr ""
        if resultStr <!> [| "empty";"bb";"c";"empty"  |].asRarr then Assert.Fail()
        Assert.AreEqual("CBB", resultStrAcc)

        // empty array
        let resultEpt,resultEptAcc = Rarr.mapFoldBack funcInt [|  |].asRarr 100
        if resultEpt <!> [|  |].asRarr then Assert.Fail()
        Assert.AreEqual(100, resultEptAcc)

        // null array
        // Rarr cant be null: let nullArr = null:string[]
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.mapFoldBack funcStr nullArr "" |> ignore)

        ()

    [<Fact>]
    member this.Mapi() = 
        // integer array 
        let funcInt x y = x+y
        let resultInt = Rarr.mapi funcInt [|10..2..20 |].asRarr
        if resultInt <!> [|10;13;16;19;22;25 |].asRarr then Assert.Fail()
        
        // string array
        let funcStr (x:int) (y:string) =  x+ y.Length
        let resultStr = Rarr.mapi funcStr  [|"Lists"; "Are";  "Commonly" ; "List"  |].asRarr
        if resultStr <!> [|5;4;10;7 |].asRarr then Assert.Fail()
        
        // empty array
        let emptyArr:int Rarr = [|  |].asRarr
        let resultEpt = Rarr.mapi funcInt emptyArr 
        if resultEpt <!> [|  |].asRarr then Assert.Fail()

        // null array
        // Rarr cant be null: let nullArr = null:string[]      
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.mapi funcStr nullArr |> ignore)  
        
        ()

    [<Fact>]
    member this.mapi2() = 
        // integer array 
        let funcInt x y z = x+y+z
        let resultInt = Rarr.mapi2 funcInt [|1..10 |].asRarr [|2..2..20 |].asRarr
        if resultInt <!> [|3;7;11;15;19;23;27;31;35;39 |].asRarr then Assert.Fail()
        
        // string array
        let funcStr  z (x:int) (y:string)  =z + x+ y.Length 
        let resultStr = Rarr.mapi2 funcStr [|3;6;9;11 |].asRarr [|"Lists"; "Are";  "Commonly" ; "List"  |].asRarr
        if resultStr <!> [|8;10;19;18 |].asRarr then Assert.Fail()
        
        // empty array
        let emptyArr:int Rarr = [|  |].asRarr
        let resultEpt = Rarr.mapi2 funcInt emptyArr emptyArr
        if resultEpt <!> [|  |].asRarr then Assert.Fail()

        // null array
        // Rarr cant be null: let nullArr = null:int[] 
        let validArray = [| 1  |].asRarr      
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.mapi2 funcInt validArray  nullArr  |> ignore)  
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.mapi2 funcInt  nullArr validArray |> ignore)  
        
        // len1 <!> len2
        CheckThrowsArgumentException(fun () -> Rarr.mapi2 funcInt [|1..10 |].asRarr [|2..20 |].asRarr |> ignore)
        
        ()

    [<Fact>]
    member this.Max() = 
        // integer array 
        let resultInt = Rarr.max  [|2..2..20 |].asRarr
        if resultInt <> 20 then Assert.Fail()
        
        // string array
        let resultStr = Rarr.max [|"t"; "ahe"; "Lists"; "Are";  "Commonly" ; "List";"a"  |].asRarr
        if resultStr <> "t" then Assert.Fail()
        
        // empty array -- argumentexception   
        
        // null array
        // Rarr cant be null: let nullArr = null:string[]      
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.max   nullArr  |> ignore)  
        
        // len = 0
        CheckThrowsArgumentException(fun() -> Rarr.max  [| |].asRarr |> ignore)
        
        ()

    [<Fact>]
    member this.MaxBy()= 
        // integer array 
        let funcInt x = x%8
        let resultInt = Rarr.maxBy funcInt [|2..2..20 |].asRarr
        if resultInt <> 6 then Assert.Fail()
        
        // string array
        let funcStr (x:string) = x.Length 
        let resultStr = Rarr.maxBy funcStr  [|"Lists"; "Are";  "Commonly" ; "List" |].asRarr
        if resultStr <> "Commonly" then Assert.Fail()    
        
        // empty array -- argumentexception    

        // null array
        // Rarr cant be null: let nullArr = null:string[]      
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.maxBy funcStr   nullArr  |> ignore)  
        
        // len = 0
        CheckThrowsArgumentException(fun() -> Rarr.maxBy funcInt (Rarr.empty<int>) |> ignore)
        
        ()

    [<Fact>]
    member this.Min() =
        // integer array 
        let resultInt = Rarr.min  [|3;7;8;9;4;1;1;2 |].asRarr
        if resultInt <> 1 then Assert.Fail()
        
        // string array
        let resultStr = Rarr.min [|"a"; "Lists";  "Commonly" ; "List"   |].asRarr 
        if resultStr <> "Commonly" then Assert.Fail()
        
        // empty array -- argumentexception   
        
        // null array
        // Rarr cant be null: let nullArr = null:string[]      
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.min   nullArr  |> ignore)  
        
        // len = 0
        CheckThrowsArgumentException(fun () -> Rarr.min  [| |].asRarr |> ignore)
        
        () 

    [<Fact>]
    member this.MinBy()= 
        // integer array 
        let funcInt x = x%8
        let resultInt = Rarr.minBy funcInt [|3;7;9;4;8;1;1;2 |].asRarr
        if resultInt <> 8 then Assert.Fail()
        
        // string array
        let funcStr (x:string) = x.Length 
        let resultStr = Rarr.minBy funcStr  [|"Lists"; "Are";  "Commonly" ; "List" |].asRarr
        if resultStr <> "Are" then Assert.Fail()    
        
        // empty array -- argumentexception    

        // null array
        // Rarr cant be null: let nullArr = null:string[]      
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.minBy funcStr   nullArr  |> ignore)  
        
        // len = 0
        CheckThrowsArgumentException(fun () -> Rarr.minBy funcInt (Rarr.empty<int>) |> ignore)
        
        ()
        

    [<Fact>]
    member this.Of_List() =
        // integer array  
        let resultInt = Rarr.ofList [1..10]
        if resultInt <!> [|1..10 |].asRarr then Assert.Fail()
        
        // string array    
        let resultStr = Rarr.ofList ["Lists"; "are";  "commonly" ; "list" ]
        if resultStr <!> [| "Lists"; "are";  "commonly" ; "list"  |].asRarr then Assert.Fail()
        
        // empty array     
        let resultEpt = Rarr.ofList []
        if resultEpt <!> [| |].asRarr then Assert.Fail()

        // null array
        
        ()

    [<Fact>]
    member this.Of_Seq() =
        // integer array  
        let resultInt = Rarr.ofSeq {1..10}
        if resultInt <!> [|1..10 |].asRarr then Assert.Fail()
        
        // string array    
        let resultStr = Rarr.ofSeq (seq {for x in 'a'..'f' -> x.ToString()})
        if resultStr <!> [| "a";"b";"c";"d";"e";"f"  |].asRarr then Assert.Fail()
        
        // empty array     
        let resultEpt = Rarr.ofSeq []
        if resultEpt <!> [|  |].asRarr then Assert.Fail()

        // null array
        
        ()

    [<Fact>]
    member this.Partition() =
        // integer array  
        let resultInt,ri = Rarr.partition (fun x -> x%3 = 0) [|1..10 |].asRarr
        if resultInt <!> [|3;6;9 |].asRarr then Assert.Fail()
        if ri <!>  [|1;2;4;5;7;8;10 |].asRarr then Assert.Fail()
        
        // string array    
        let resultStr,rs = Rarr.partition (fun (x:string) -> x.Length >4) [|"Lists"; "are";  "commonly" ; "list"  |].asRarr
        if resultStr<!> [|"Lists";"commonly" |].asRarr then Assert.Fail()
        if rs <!> [|"are"; "list" |].asRarr then Assert.Fail()
        
        // empty array     
        let resultEpt,re = Rarr.partition (fun x -> x%3 = 0) [| |].asRarr
        if resultEpt <!> [| |].asRarr then Assert.Fail()
        if re <!> [| |].asRarr then Assert.Fail()

        // null array
        // Rarr cant be null: let nullArr = null:string[]      
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.partition (fun (x:string) -> x.Length >4)  nullArr  |> ignore)  
        
        ()

    [<Fact>]
    member this.Permute() =
        // integer array  
        let resultInt = Rarr.permute (fun i -> (i+1) % 4) [|1;2;3;4 |].asRarr
        if resultInt <!> [|4;1;2;3 |].asRarr then Assert.Fail()
        
        // string array    
        let resultStr = Rarr.permute (fun i -> (i+1) % 4) [|"Lists"; "are";  "commonly" ; "list"  |].asRarr
        if resultStr <!> [|"list";"Lists"; "are";  "commonly"  |].asRarr then Assert.Fail()
        
        // empty array     
        let resultEpt = Rarr.permute (fun i -> (i+1) % 4) [| |].asRarr
        if resultEpt <!> [| |].asRarr then Assert.Fail()
    
        // null array
        // Rarr cant be null: let nullArr = null:string[]      
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.permute (fun i -> (i+1) % 4)  nullArr  |> ignore)   
        
        ()

    [<Fact>]
    member this.Reduce() =
        // integer array  
        let resultInt = Rarr.reduce (fun x y -> x/y) [|5*4*3*2; 4;3;2;1 |].asRarr
        if resultInt <> 5 then Assert.Fail()
        
        // string array    
        let resultStr = Rarr.reduce (fun (x:string) (y:string) -> x.Remove(0,y.Length)) [|"ABCDE";"A"; "B";  "C" ; "D"  |].asRarr
        if resultStr <> "E" then  Assert.Fail()
        
        // empty array 
        CheckThrowsArgumentException (fun () -> Rarr.reduce (fun x y -> x/y)  [| |].asRarr |> ignore)

        // null array
        // Rarr cant be null: let nullArr = null:string[]      
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.reduce (fun (x:string) (y:string) -> x.Remove(0,y.Length))  nullArr  |> ignore)   
        
        ()

        
    [<Fact>]
    member this.ReduceBack() =
        // integer array  
        let resultInt = Rarr.reduceBack (fun x y -> x/y) [|5*4*3*2; 4;3;2;1 |].asRarr
        if resultInt <> 30 then Assert.Fail()
        
        // string array    
        let resultStr = Rarr.reduceBack (fun (x:string) (y:string) -> x.Remove(0,y.Length)) [|"ABCDE";"A"; "B";  "C" ; "D"  |].asRarr
        if resultStr <> "ABCDE" then  Assert.Fail()
        
        // string array
        let funcStr x y = x+y 
        let resultStr2 = Rarr.reduceBack funcStr [|"A"; "B";  "C" ; "D"  |].asRarr 
        if resultStr2 <> "ABCD" then  Assert.Fail()

        // empty array 
        CheckThrowsArgumentException (fun () -> Rarr.reduceBack (fun x y -> x/y) [| |].asRarr |> ignore)

        // null array
        // Rarr cant be null: let nullArr = null:string[]      
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.reduceBack (fun (x:string) (y:string) -> x.Remove(0,y.Length))  nullArr  |> ignore)   
        
        ()
    

    [<Fact>]
    member this.Rev() =
        // integer array  
        let resultInt = Rarr.rev  [|1..10 |].asRarr
        if resultInt <!> [|10;9;8;7;6;5;4;3;2;1 |].asRarr then Assert.Fail()
        
        // string array    
        let resultStr = Rarr.rev  [|"Lists"; "are";  "commonly" ; "list"  |].asRarr
        if resultStr <!> [|"list"; "commonly"; "are"; "Lists"  |].asRarr then Assert.Fail()
        
        // empty array     
        let resultEpt = Rarr.rev  [| |].asRarr
        if resultEpt <!> [| |].asRarr then Assert.Fail()

        // null array
        // Rarr cant be null: let nullArr = null:string[]      
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.rev  nullArr  |> ignore) 
        ()

    [<Fact>] 
    member this.Scan() =
        // integer array
        let funcInt x y = x+y
        let resultInt = Rarr.scan funcInt 9 [| 1..10  |].asRarr
        if resultInt <!> [|9;10;12;15;19;24;30;37;45;54;64 |].asRarr then Assert.Fail()
        
        // string array
        let funcStr x y = x+y        
        let resultStr = Rarr.scan funcStr "x" [|"A"; "B";  "C" ; "D"  |].asRarr
        if resultStr <!> [|"x";"xA";"xAB";"xABC";"xABCD" |].asRarr then Assert.Fail()
        


        // empty array
        let resultEpt = Rarr.scan funcInt 5 [|  |].asRarr
        if resultEpt <!> [|5 |].asRarr then Assert.Fail()

        // null array
        // Rarr cant be null: let nullArr = null:string[]      
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.scan funcStr "begin"  nullArr  |> ignore)  
        
        ()   
    
    [<Fact>]
    member this.ScanBack() =
        // integer array 
        let funcInt x y = x+y
        let resultInt = Rarr.scanBack funcInt [| 1..10  |].asRarr 9
        if resultInt <!> [|64;63;61;58;54;49;43;36;28;19;9 |].asRarr then Assert.Fail()
        
        // string array
        let funcStr x y = x+y        
        let resultStr = Rarr.scanBack funcStr [|"A"; "B";  "C" ; "D"  |].asRarr "X" 
        if resultStr <!> [|"ABCDX";"BCDX";"CDX";"DX";"X" |].asRarr then Assert.Fail()


        
        // empty array
        let resultEpt = Rarr.scanBack funcInt [|  |].asRarr 5 
        if resultEpt <!> [|5 |].asRarr then Assert.Fail()

        // null array
        // Rarr cant be null: let nullArr = null:string[]      
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.scanBack funcStr nullArr "begin"  |> ignore) 
        
        ()

    [<Fact>]
    member this.Skip() =
        // integer array
        let resultInt = Rarr.skip 2 [|1..10 |].asRarr
        if resultInt <!> [|3..10 |].asRarr then Assert.Fail()
        
        let resultInt2 = Rarr.skip 0 [|1..10 |].asRarr
        if resultInt2 <!> [|1..10 |].asRarr then Assert.Fail()
        
        CheckThrowsArgumentException (fun () ->  Rarr.skip -5 [|1..10 |].asRarr |> ignore)
        //if resultInt3 <!> [|1..10 |].asRarr then Assert.Fail()

        // string List
        let resultStr = Rarr.skip 2 [|"str1";"str2";"str3";"str4" |].asRarr
        if resultStr <!> [|"str3";"str4" |].asRarr then Assert.Fail()

        // empty List
        let resultEpt = Rarr.skip 0 [| |].asRarr
        if resultEpt <!> [| |].asRarr then Assert.Fail()

        // exceptions
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.skip 0 (null:string[]) |> ignore)
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.skip -3 (null:string[]) |> ignore)
        CheckThrowsArgumentException (fun () -> Rarr.skip 1 [| |].asRarr |> ignore)
        CheckThrowsArgumentException (fun () -> Rarr.skip 4 [|1; 2; 3 |].asRarr |> ignore)

    [<Fact>]
    member this.SkipWhile() =
        // integer array
        let funcInt x = (x < 4)
        let intArr = [|1..10 |].asRarr
        let resultInt = Rarr.skipWhile funcInt intArr
        if resultInt <!> [|4..10 |].asRarr then Assert.Fail()

        // string array
        let funcStr (s:string) = s.Length < 8
        let strArr = [| "Lists"; "are";  "commonly" ; "list"  |].asRarr
        let resultStr = Rarr.skipWhile funcStr strArr
        if resultStr <!> [| "commonly" ; "list"  |].asRarr then Assert.Fail()

        // empty array
        let resultEmpt = Rarr.skipWhile (fun _ -> failwith "unexpected error") [|  |].asRarr
        if resultEmpt <!> [|  |].asRarr then Assert.Fail()

        // null array
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.skipWhile (fun _ -> failwith "unexpected error") null |> ignore)

        // skip all
        let resultAll = Rarr.skipWhile (fun _ -> true) intArr
        if resultAll <!> [|  |].asRarr then Assert.Fail()

        // skip none
        let resultNone = Rarr.skipWhile (fun _ -> false) intArr
        if resultNone <!> intArr then Assert.Fail()

        ()

    [<Fact>]
    member this.Set() =
        // integer array  
        let intArr = [|10;9;8;7 |].asRarr
        Rarr.set intArr  3 600
        if intArr <!> [|10;9;8;600 |].asRarr then Assert.Fail()  
        
        // string array
        let strArr = [|"Lists"; "are";  "commonly" ; "list"  |].asRarr    
        Rarr.set strArr 2 "always"
        if strArr <!> [|"Lists"; "are";  "always" ; "list"  |].asRarr     then Assert.Fail()
        
        // empty array -- outofbundaryexception
        
        // null array
        // Rarr cant be null: let nullArr = null:string[]      
        // Rarr cant be null: CheckThrowsNullRefException (fun () -> Rarr.set nullArr 0 "null"   |> ignore)
        
        ()    

    [<Fact>]
    member this.sortInPlaceWith() =
        // integer array  
        let intArr = [|3;5;7;2;4;8 |].asRarr
        Rarr.sortInPlaceWith compare intArr  
        if intArr <!> [|2;3;4;5;7;8 |].asRarr then Assert.Fail()  

        // Sort backwards
        let intArr = [|3;5;7;2;4;8 |].asRarr
        Rarr.sortInPlaceWith (fun a b -> -1 * compare a b) intArr  
        if intArr <!> [|8;7;5;4;3;2 |].asRarr then Assert.Fail()  
        
        // string array
        let strArr = [|"Lists"; "are"; "a"; "commonly"; "used"; "data"; "structure" |].asRarr    
        Rarr.sortInPlaceWith compare strArr 
        if strArr <!> [| "Lists"; "a"; "are"; "commonly"; "data"; "structure"; "used" |].asRarr     then Assert.Fail()
        
        // empty array
        let emptyArr:int Rarr = [|  |].asRarr
        Rarr.sortInPlaceWith compare emptyArr
        if emptyArr <!> [| |].asRarr then Assert.Fail()
        
        // null array
        // Rarr cant be null: let nullArr = null:string[]      
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.sortInPlaceWith compare nullArr  |> ignore)  
        
        // len = 2  
        let len2Arr = [|8;3 |].asRarr      
        Rarr.sortInPlaceWith compare len2Arr
        Assert.AreEqual([|3;8 |].asRarr, len2Arr)
        
        // Equal elements
        let eights = [|8; 8;8 |].asRarr      
        Rarr.sortInPlaceWith compare eights
        Assert.AreEqual([|8;8;8 |].asRarr, eights)
        
        ()   
        

    [<Fact>]
    member this.sortInPlaceBy() =
        // integer array  
        let intArr = [|3;5;7;2;4;8 |].asRarr
        Rarr.sortInPlaceBy int intArr  
        if intArr <!> [|2;3;4;5;7;8 |].asRarr then Assert.Fail()  
        
        // string array
        let strArr = [|"Lists"; "are"; "a"; "commonly"; "used"; "data"; "structure" |].asRarr    
        Rarr.sortInPlaceBy (fun (x:string) -> x.Length)  strArr 
        // note: Rarr.sortInPlaceBy is not stable, so we allow 2 results.
        if strArr <!> [| "a"; "are";"data"; "used";"Lists"; "commonly";"structure" |].asRarr && strArr <!> [| "a"; "are"; "used"; "data"; "Lists"; "commonly";"structure" |].asRarr    then Assert.Fail()
        
        // empty array
        let emptyArr:int Rarr = [|  |].asRarr
        Rarr.sortInPlaceBy int emptyArr
        if emptyArr <!> [| |].asRarr then Assert.Fail()
        
        // null array
        // Rarr cant be null: let nullArr = null:string[]      
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.sortInPlaceBy (fun (x:string) -> x.Length) nullArr |> ignore)  
        
        // len = 2  
        let len2Arr = [|8;3 |].asRarr      
        Rarr.sortInPlaceBy int len2Arr
        if len2Arr <!> [|3;8 |].asRarr then Assert.Fail()  
        Assert.AreEqual([|3;8 |].asRarr,len2Arr)  
        
        () 
        
    [<Fact>]
    member this.SortDescending() =
        // integer array  
        let intArr = [|3;5;7;2;4;8 |].asRarr
        let resultInt = Rarr.sortDescending intArr  
        Assert.AreEqual([|8;7;5;4;3;2 |].asRarr, resultInt)
        
        // string Array
        let strArr = [|"Z";"a";"d"; ""; "Y"; null; "c";"b";"X" |].asRarr   
        let resultStr = Rarr.sortDescending strArr         
        Assert.AreEqual([|"d"; "c"; "b"; "a"; "Z"; "Y"; "X"; ""; null |].asRarr, resultStr)
        
        // empty array
        let emptyArr:int Rarr = [|  |].asRarr
        let resultEmpty = Rarr.sortDescending emptyArr
        if resultEmpty <!> [| |].asRarr then Assert.Fail()
        
        // tuple array
        let tupArr = [|(2,"a");(1,"d");(1,"b");(1,"a");(2,"x");(2,"b");(1,"x") |].asRarr   
        let resultTup = Rarr.sortDescending tupArr         
        Assert.AreEqual([|(2,"x");(2,"b");(2,"a");(1,"x");(1,"d");(1,"b");(1,"a") |].asRarr, resultTup)

        // date array
        let dateArr = [|DateTime(2014,12,31);DateTime(2014,1,1);DateTime(2015,1,1);DateTime(2013,12,31);DateTime(2014,1,1) |].asRarr   
        let resultDate = Rarr.sortDescending dateArr         
        
        if [|DateTime(2014,12,31);DateTime(2014,1,1);DateTime(2015,1,1);DateTime(2013,12,31);DateTime(2014,1,1) |].asRarr <!> dateArr then  Assert.Fail()
        if [|DateTime(2015,1,1);DateTime(2014,12,31);DateTime(2014,1,1);DateTime(2014,1,1);DateTime(2013,12,31) |].asRarr <!> resultDate then  Assert.Fail()


        //Assert.AreEqual([|DateTime(2014,12,31);DateTime(2014,1,1);DateTime(2015,1,1);DateTime(2013,12,31);DateTime(2014,1,1) |].asRarr, dateArr)
        //Assert.AreEqual([|DateTime(2015,1,1);DateTime(2014,12,31);DateTime(2014,1,1);DateTime(2014,1,1);DateTime(2013,12,31) |].asRarr, resultDate)

        // float array
        let minFloat,maxFloat,epsilon = System.Double.MinValue,System.Double.MaxValue,System.Double.Epsilon
        let floatArr = [| 0.0; 0.5; 2.0; 1.5; 1.0; minFloat; maxFloat; epsilon; -epsilon  |].asRarr
        let resultFloat = Rarr.sortDescending floatArr
        Assert.AreEqual([| maxFloat; 2.0; 1.5; 1.0; 0.5; epsilon; 0.0; -epsilon; minFloat;  |].asRarr, resultFloat)

        () 
        
    [<Fact>]
    member this.SortByDescending() =
        // integer array  
        let intArr = [|3;5;7;2;4;8 |].asRarr
        let resultInt = Rarr.sortByDescending int intArr           
        Assert.AreEqual([|3;5;7;2;4;8 |].asRarr, intArr)
        Assert.AreEqual([|8;7;5;4;3;2 |].asRarr, resultInt)
                
        // string array
        let strArr = [|".."; ""; "..."; "."; "...." |].asRarr    
        let resultStr = Rarr.sortByDescending (fun (x:string) -> x.Length)  strArr 
        Assert.AreEqual([|".."; ""; "..."; "."; "...." |].asRarr, strArr)
        Assert.AreEqual([|"....";"...";"..";"."; "" |].asRarr, resultStr)
        
        // empty array
        let emptyArr:int Rarr = [|  |].asRarr
        let resultEmpty = Rarr.sortByDescending int emptyArr        
        if resultEmpty <!> [| |].asRarr then Assert.Fail()    
        
        // tuple array
        let tupArr = [|(2,"a");(1,"d");(1,"b");(2,"x") |].asRarr
        let sndTup = Rarr.sortByDescending snd tupArr         
        Assert.AreEqual( [|(2,"a");(1,"d");(1,"b");(2,"x") |].asRarr , tupArr)
        Assert.AreEqual( [|(2,"x");(1,"d");(1,"b");(2,"a") |].asRarr , sndTup)
        
        // date array
        let dateArr = [|DateTime(2013,12,31);DateTime(2014,2,1);DateTime(2015,1,1);DateTime(2014,3,1) |].asRarr
        let resultDate = Rarr.sortByDescending (fun (d:DateTime) -> d.Month) dateArr         
        if [|DateTime(2013,12,31);DateTime(2014,2,1);DateTime(2015,1,1);DateTime(2014,3,1) |].asRarr <!> dateArr then Assert.Fail()
        if [|DateTime(2013,12,31);DateTime(2014,3,1);DateTime(2014,2,1);DateTime(2015,1,1) |].asRarr <!> resultDate then Assert.Fail()

        // float array
        let minFloat,maxFloat,epsilon = System.Double.MinValue,System.Double.MaxValue,System.Double.Epsilon
        let floatArr = [| 0.0; 0.5; 2.0; 1.5; 1.0; minFloat; maxFloat; epsilon; -epsilon  |].asRarr
        let resultFloat = Rarr.sortByDescending id floatArr
        Assert.AreEqual([| maxFloat; 2.0; 1.5; 1.0; 0.5; epsilon; 0.0; -epsilon; minFloat;  |].asRarr, resultFloat)

        ()  
         
    [<Fact>]
    member this.Sub() =
        // integer array  
        let resultInt = Rarr.sub [|1..8 |].asRarr 3 3
        if resultInt <!> [|4;5;6 |].asRarr then Assert.Fail()
        
        // string array    
        let resultStr = Rarr.sub [|"Lists"; "are";  "commonly" ; "list"  |].asRarr 1 2
        if resultStr <!> [|"are";  "commonly"  |].asRarr then Assert.Fail()
        
        // empty array     
        let resultEpt = Rarr.sub [|  |].asRarr 0 0
        if resultEpt <!> [| |].asRarr then Assert.Fail()

        // null array
        // Rarr cant be null: let nullArr = null:string[]      
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.sub nullArr 1 1 |> ignore)  
        
        // bounds
        CheckThrowsArgumentException (fun () -> Rarr.sub resultInt -1 2 |> ignore)
        CheckThrowsArgumentException (fun () -> Rarr.sub resultInt 1 -2 |> ignore)
        CheckThrowsArgumentException (fun () -> Rarr.sub resultInt 1 20 |> ignore)
        
        ()

    [<Fact>]
    member this.Sum() =
        // empty integer array 
        let resultEptInt = Rarr.sum ([| |].asRarr:int Rarr) 
        if resultEptInt <> 0 then Assert.Fail()    
        
        // empty float32 array
        let emptyFloatArray = Rarr.empty<System.Single> 
        let resultEptFloat = Rarr.sum emptyFloatArray 
        if resultEptFloat <> 0.0f then Assert.Fail()
        
        // empty double array
        let emptyDoubleArray = Rarr.empty<System.Double> 
        let resultDouEmp = Rarr.sum emptyDoubleArray 
        if resultDouEmp <> 0.0 then Assert.Fail()
        
        // empty decimal array
        let emptyDecimalArray = Rarr.empty<System.Decimal> 
        let resultDecEmp = Rarr.sum emptyDecimalArray 
        if resultDecEmp <> 0M then Assert.Fail()

        // integer array  
        let resultInt = Rarr.sum [|1..10 |].asRarr 
        if resultInt <> 55 then Assert.Fail()  
        
        // float32 array
        let floatArray: float32 Rarr = [| 1.1f; 1.1f; 1.1f  |].asRarr
        let resultFloat = Rarr.sum floatArray
        if resultFloat < 3.3f - 0.001f || resultFloat > 3.3f + 0.001f then
            Assert.Fail()
        
        // double array
        let doubleArray: System.Double Rarr = [| 1.0; 8.0  |].asRarr
        let resultDouble = Rarr.sum doubleArray
        if resultDouble <> 9.0 then Assert.Fail()
        
        // decimal array
        let decimalArray: decimal Rarr = [| 0M; 19M; 19.03M  |].asRarr
        let resultDecimal = Rarr.sum decimalArray
        if resultDecimal <> 38.03M then Assert.Fail()      
 
        // null array
        // Rarr cant be null: let nullArr = null:double[]    
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.sum  nullArr  |> ignore) 
        ()

    [<Fact>]
    member this.SumBy() =
        // empty integer array         
        let resultEptInt = Rarr.sumBy int ([| |].asRarr:int Rarr) 
        if resultEptInt <> 0 then Assert.Fail()    
        
        // empty float32 array
        let emptyFloatArray = Rarr.empty<System.Single> 
        let resultEptFloat = Rarr.sumBy float32 emptyFloatArray 
        if resultEptFloat <> 0.0f then Assert.Fail()
        
        // empty double array
        let emptyDoubleArray = Rarr.empty<System.Double> 
        let resultDouEmp = Rarr.sumBy float emptyDoubleArray 
        if resultDouEmp <> 0.0 then Assert.Fail()
        
        // empty decimal array
        let emptyDecimalArray = Rarr.empty<System.Decimal> 
        let resultDecEmp = Rarr.sumBy decimal emptyDecimalArray 
        if resultDecEmp <> 0M then Assert.Fail()

        // integer array  
        let resultInt = Rarr.sumBy int [|1..10 |].asRarr 
        if resultInt <> 55 then Assert.Fail()  
        
        // float32 array
        let floatArray: string Rarr = [| "1.2";"3.5";"6.7"  |].asRarr
        let resultFloat = Rarr.sumBy float32 floatArray
        if abs (resultFloat - 11.4f) > 0.00000001f then Assert.Fail()
        
        // double array
        let doubleArray: System.Double Rarr = [| 1.0;8.0  |].asRarr
        let resultDouble = Rarr.sumBy float doubleArray
        if resultDouble <> 9.0 then Assert.Fail()
        
        // decimal array
        let decimalArray: decimal Rarr = [| 0M;19M;19.03M  |].asRarr
        let resultDecimal = Rarr.sumBy decimal decimalArray
        if resultDecimal <> 38.03M then Assert.Fail()      
        
        // null array
        // Rarr cant be null: let nullArr = null:double[]    
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.sumBy float32  nullArr  |> ignore) 
        ()

    [<Fact>]
    member this.Tl() =
        // integer array  
        let resultInt = Rarr.tail [|1..10 |].asRarr        
        Assert.AreEqual([|2..10 |].asRarr, resultInt)
        
        // string array    
        let resultStr = Rarr.tail [| "a"; "b"; "c"; "d"  |].asRarr        
        Assert.AreEqual([| "b";  "c" ; "d"  |].asRarr, resultStr)
        
        // 1-element array    
        let resultStr2 = Rarr.tail [| "a"  |].asRarr        
        Assert.AreEqual([|  |].asRarr, resultStr2)

        CheckThrowsArgumentException(fun () -> Rarr.tail [| |].asRarr |> ignore)

        // Rarr cant be null: CheckThrowsArgumentNullException(fun () -> Rarr.tail null |> ignore)
        ()

    [<Fact>]
    member this.To_List() =
        // integer array  
        let resultInt = Rarr.toList [|1..10 |].asRarr
        if resultInt <> [1..10] then Assert.Fail()
        
        // string array    
        let resultStr = Rarr.toList [|"Lists"; "are";  "commonly" ; "list"  |].asRarr
        if resultStr <> ["Lists"; "are";  "commonly" ; "list"] then Assert.Fail()
        
        // empty array     
        let resultEpt = Rarr.toList  [| |].asRarr
        if resultEpt <> [] then Assert.Fail()

        // null array
        // Rarr cant be null: let nullArr = null:string[]      
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.toList   nullArr  |> ignore)  
        
        ()    
        
    [<Fact>]
    member this.To_Seq() =
        // integer array  
        let resultInt = [|1..10 |].asRarr |> Rarr.toSeq  |> Rarr.ofSeq
        if resultInt <!> [|1..10 |].asRarr then Assert.Fail()
        
        // string array    
        let resultStr = [|"Lists"; "are";  "commonly" ; "list"  |].asRarr |> Rarr.toSeq |> Rarr.ofSeq
        if resultStr <!> [|"Lists"; "are";  "commonly" ; "list"  |].asRarr then Assert.Fail()
        
        // empty array     
        let resultEpt =[| |].asRarr |> Rarr.toSeq  |> Rarr.ofSeq
        if resultEpt <!> [| |].asRarr  then Assert.Fail()

        // null array
        // Rarr cant be null: let nullArr = null:string[]  
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> nullArr  |> Rarr.toSeq   |> ignore)  
        
        ()   

    [<Fact>]
    member this.Transpose() =
        // integer array
        Assert.AreEqual([|[|1;4 |].asRarr; [|2;5 |].asRarr; [|3;6 |].asRarr |].asRarr, Rarr.transpose (rarr{ [|1..3 |].asRarr; [|4..6 |].asRarr} ))
        Assert.AreEqual([|[|1;4 |].asRarr; [|2;5 |].asRarr; [|3;6 |].asRarr |].asRarr, Rarr.transpose (rarr{[|1..3 |].asRarr; [|4..6 |].asRarr} ))
        Assert.AreEqual([|[|1 |].asRarr; [|2 |].asRarr; [|3 |].asRarr |].asRarr, Rarr.transpose [|[|1..3 |].asRarr |].asRarr)
        Assert.AreEqual([|[|1..2 |].asRarr |].asRarr, Rarr.transpose [|[|1 |].asRarr; [|2 |].asRarr |].asRarr)

        // string array
        Assert.AreEqual([|[|"a";"d" |].asRarr; [|"b";"e" |].asRarr; [|"c";"f" |].asRarr |].asRarr, Rarr.transpose (rarr{[|"a";"b";"c" |].asRarr; [|"d";"e";"f" |].asRarr}))

        // empty array
        Assert.AreEqual([|  |].asRarr, Rarr.transpose [|  |].asRarr)

        // array of empty arrays - m x 0 array transposes to 0 x m (i.e. empty)
        Assert.AreEqual([|  |].asRarr, Rarr.transpose [| [| |].asRarr  |].asRarr)
        Assert.AreEqual([|  |].asRarr, Rarr.transpose [| [| |].asRarr; [| |].asRarr  |].asRarr)

        // null array
        // Rarr cant be null: let nullArr = null: string[][]
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> nullArr |> Rarr.transpose |> ignore)

        // jagged arrays
        CheckThrowsArgumentException (fun () -> Rarr.transpose [| [|1; 2 |].asRarr; [|3 |].asRarr  |].asRarr |> ignore)
        CheckThrowsArgumentException (fun () -> Rarr.transpose [| [|1 |].asRarr; [|2; 3 |].asRarr  |].asRarr |> ignore)

    [<Fact>]
    member this.Truncate() =
        // integer array
        Assert.AreEqual([|1..3 |].asRarr, Rarr.truncate 3 [|1..5 |].asRarr)
        Assert.AreEqual([|1..5 |].asRarr, Rarr.truncate 10 [|1..5 |].asRarr)
        Assert.AreEqual([|  |].asRarr, Rarr.truncate 0 [|1..5 |].asRarr)

        // string array
        Assert.AreEqual([|"str1";"str2" |].asRarr, Rarr.truncate 2 [|"str1";"str2";"str3" |].asRarr)

        // empty array
        Assert.AreEqual([|  |].asRarr, Rarr.truncate 0 [|  |].asRarr)
        Assert.AreEqual([|  |].asRarr, Rarr.truncate 1 [|  |].asRarr)

        // null array
        // Rarr cant be null: CheckThrowsArgumentNullException(fun() -> Rarr.truncate 1 null |> ignore)

        // negative count
        Assert.AreEqual([|  |].asRarr, Rarr.truncate -1 [|1..5 |].asRarr)
        Assert.AreEqual([|  |].asRarr, Rarr.truncate System.Int32.MinValue [|1..5 |].asRarr)

        ()

    [<Fact>]
    member this.TryFind() =
        // integer array  
        let resultInt = [|1..10 |].asRarr |> Rarr.tryFind (fun x -> x%7 = 0)  
        if resultInt <> Some 7 then Assert.Fail()
        
        // string array    
        let resultStr = [|"Lists"; "are";  "commonly" ; "list"  |].asRarr |> Rarr.tryFind (fun (x:string) -> x.Length > 4)
        if resultStr <> Some "Lists" then Assert.Fail()
        
        // empty array     
        let resultEpt =[| |].asRarr |> Rarr.tryFind  (fun x -> x%7 = 0)  
        if resultEpt <> None  then Assert.Fail()

        // null array
        // Rarr cant be null: let nullArr = null:string[]      
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.tryFind (fun (x:string) -> x.Length > 4)  nullArr  |> ignore)  
        
        ()
        
    [<Fact>]
    member this.TryFindBack() =
        // integer array
        let funcInt x = x%5 = 0
        Assert.AreEqual(Some 20, [| 1..20  |].asRarr |> Rarr.tryFindBack funcInt)
        Assert.AreEqual(Some 15, [| 1..19  |].asRarr |> Rarr.tryFindBack funcInt)
        Assert.AreEqual(Some 5, [| 5..9  |].asRarr |> Rarr.tryFindBack funcInt)

        // string array
        let resultStr = [|"Lists"; "are";  "commonly" ; "list"  |].asRarr |> Rarr.tryFindBack (fun (x:string) -> x.Length > 4)
        Assert.AreEqual(Some "commonly", resultStr)

        // empty array
        Assert.AreEqual(None, [|  |].asRarr |> Rarr.tryFindBack (fun _ -> failwith "error"))

        // not found
        Assert.AreEqual(None, [| 1..20  |].asRarr |> Rarr.tryFindBack (fun _ -> false))

        // null array
        // Rarr cant be null: let nullArr = null:string[]
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.tryFindBack (fun _ -> failwith "error") nullArr |> ignore)

        ()

    [<Fact>]
    member this.TryFindIndex() =
        // integer array  
        let resultInt = [|1..10 |].asRarr |> Rarr.tryFindIndex (fun x -> x%7 = 0)  
        if resultInt <> Some 6 then Assert.Fail()
        
        // string array    
        let resultStr = [|"Lists"; "are";  "commonly" ; "list"  |].asRarr |> Rarr.tryFindIndex (fun (x:string) -> x.Length > 4)
        if resultStr <> Some 0 then Assert.Fail()
        
        // empty array     
        let resultEpt =[| |].asRarr |> Rarr.tryFindIndex  (fun x -> x % 7 = 0)  
        if resultEpt <> None  then Assert.Fail()

        // null array
        // Rarr cant be null: let nullArr = null:string[]      
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.tryFindIndex (fun (x:string) -> x.Length > 4)  nullArr  |> ignore)  
        
        ()

    [<Fact>]
    member this.TryFindIndexBack() =
        // integer array
        let funcInt x = x%5 = 0
        Assert.AreEqual(Some 19, [| 1..20  |].asRarr |> Rarr.tryFindIndexBack funcInt)
        Assert.AreEqual(Some 14, [| 1..19  |].asRarr |> Rarr.tryFindIndexBack funcInt)
        Assert.AreEqual(Some 0, [| 5..9  |].asRarr |> Rarr.tryFindIndexBack funcInt)

        // string array
        let resultStr = [|"Lists"; "are";  "commonly" ; "list"  |].asRarr |> Rarr.tryFindIndexBack (fun (x:string) -> x.Length > 4)
        Assert.AreEqual(Some 2, resultStr)

        // empty array
        Assert.AreEqual(None, [|  |].asRarr |> Rarr.tryFindIndexBack (fun _ -> true))

        // not found
        Assert.AreEqual(None, [| 1..20  |].asRarr |> Rarr.tryFindIndexBack (fun _ -> false))

        // null array
        // Rarr cant be null: let nullArr = null:string[]
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.tryFindIndexBack (fun (x:string) -> x.Length > 4) nullArr |> ignore)

        ()

    [<Fact>]
    member this.Unfold() =
        // integer Seq
        let resultInt = Rarr.unfold (fun x -> if x < 20 then Some (x+1,x*2) else None) 1
        Assert.AreEqual([|2;3;5;9;17 |].asRarr, resultInt)

        // string Seq
        let resultStr = Rarr.unfold (fun (x:string) -> if x.Contains("unfold") then Some("a","b") else None) "unfold"
        Assert.AreEqual([|"a" |].asRarr, resultStr)

        // empty seq
        let resultEpt = Rarr.unfold (fun _ -> None) 1
        Assert.AreEqual([|  |].asRarr, resultEpt)

        ()

    [<Fact>]
    member this.Unzip() =
        // integer array  
        let resultInt =  Rarr.unzip [|(1,2);(2,4);(3,6) |].asRarr 
        if resultInt <!!>  ([|1..3 |].asRarr, [|2..2..6 |].asRarr) then Assert.Fail()
        
        // string array    
        let resultStr = Rarr.unzip [|("A","a");("B","b");("C","c");("D","d") |].asRarr
        let str = resultStr.ToString()
        if resultStr <!!> ([|"A"; "B";  "C" ; "D"  |].asRarr,[|"a";"b";"c";"d" |].asRarr) then Assert.Fail()
        
        // empty array     
        let resultEpt = Rarr.unzip  [| |].asRarr
        if resultEpt <!!> ([| |].asRarr,[| |].asRarr)  then Assert.Fail()

        // null array
        
        ()

    [<Fact>]
    member this.Unzip3() =
        // integer array  
        let resultInt =  Rarr.unzip3 [|(1,2,3);(2,4,6);(3,6,9) |].asRarr
        if resultInt <!!!> ([|1;2;3 |].asRarr, [|2;4;6 |].asRarr, [|3;6;9 |].asRarr) then Assert.Fail()
        
        // string array    
        let resultStr = Rarr.unzip3 [|("A","1","a");("B","2","b");("C","3","c");("D","4","d") |].asRarr
        if resultStr <!!!> ([|"A"; "B";  "C" ; "D"  |].asRarr, [|"1";"2";"3";"4" |].asRarr, [|"a"; "b"; "c"; "d" |].asRarr) then Assert.Fail()
        
        // empty array     
        let resultEpt = Rarr.unzip3  [| |].asRarr
        if resultEpt <!!!>  ([| |].asRarr, [| |].asRarr, [| |].asRarr) then Assert.Fail()

        // null array
        
        ()

    [<Fact>]
    member this.Windowed() =
        let testWindowed config =
            try
                config.InputArray
                |> Rarr.windowed config.WindowSize
                |> (fun actual -> Assert.True(config.ExpectedArray =+= actual))
            with
            | _ when Option.isNone config.Exception -> Assert.Fail()
            | e when e.GetType() = (Option.get config.Exception) -> ()
            | _ -> Assert.Fail()

        {
          InputArray = [|1..10 |].asRarr
          WindowSize = 1
          ExpectedArray =  [| for i in 1..10 do yield [| i  |].asRarr  |].asRarr
          Exception = None
        } |> testWindowed
        {
          InputArray = [|1..10 |].asRarr
          WindowSize = 5
          ExpectedArray =  [| for i in 1..6 do yield [| i; i+1; i+2; i+3; i+4  |].asRarr  |].asRarr
          Exception = None
        } |> testWindowed
        {
          InputArray = [|1..10 |].asRarr
          WindowSize = 10
          ExpectedArray =  [| yield [| 1 .. 10  |].asRarr  |].asRarr
          Exception = None
        } |> testWindowed
        {
          InputArray = [|1..10 |].asRarr
          WindowSize = 25
          ExpectedArray = [|  |].asRarr
          Exception = None
        } |> testWindowed
        {
          InputArray = [|"str1";"str2";"str3";"str4" |].asRarr
          WindowSize = 2
          ExpectedArray =  [| [|"str1";"str2" |].asRarr; [|"str2";"str3" |].asRarr; [|"str3";"str4" |].asRarr  |].asRarr
          Exception = None
        } |> testWindowed
        {
          InputArray = [|  |].asRarr
          WindowSize = 2
          ExpectedArray = [|  |].asRarr
          Exception = None
        } |> testWindowed
        //{
        //  InputArray = null
        //  WindowSize = 2
        //  ExpectedArray = [|  |].asRarr
        //  Exception = Some typeof<ArgumentNullException>
        //} |> testWindowed
        {
          InputArray = [|1..10 |].asRarr
          WindowSize = 0
          ExpectedArray =  [|  |].asRarr
          Exception = Some typeof<ArgumentException>
        } |> testWindowed

        // expectedArrays indexed by arraySize,windowSize
        let expectedArrays = Array2D.zeroCreate 6 6
        expectedArrays.[1,1] <- [| [|1 |].asRarr  |].asRarr
        expectedArrays.[2,1] <- [| [|1 |].asRarr; [|2 |].asRarr  |].asRarr
        expectedArrays.[2,2] <- [| [|1; 2 |].asRarr  |].asRarr
        expectedArrays.[3,1] <- [| [|1 |].asRarr; [|2 |].asRarr; [|3 |].asRarr  |].asRarr
        expectedArrays.[3,2] <- [| [|1; 2 |].asRarr; [|2; 3 |].asRarr  |].asRarr
        expectedArrays.[3,3] <- [| [|1; 2; 3 |].asRarr  |].asRarr
        expectedArrays.[4,1] <- [| [|1 |].asRarr; [|2 |].asRarr; [|3 |].asRarr; [|4 |].asRarr  |].asRarr
        expectedArrays.[4,2] <- [| [|1; 2 |].asRarr; [|2; 3 |].asRarr; [|3; 4 |].asRarr  |].asRarr
        expectedArrays.[4,3] <- [| [|1; 2; 3 |].asRarr; [|2; 3; 4 |].asRarr  |].asRarr
        expectedArrays.[4,4] <- [| [|1; 2; 3; 4 |].asRarr  |].asRarr
        expectedArrays.[5,1] <- [| [|1 |].asRarr; [|2 |].asRarr; [|3 |].asRarr; [|4 |].asRarr; [|5 |].asRarr  |].asRarr
        expectedArrays.[5,2] <- [| [|1; 2 |].asRarr; [|2; 3 |].asRarr; [|3; 4 |].asRarr; [|4; 5 |].asRarr  |].asRarr
        expectedArrays.[5,3] <- [| [|1; 2; 3 |].asRarr; [|2; 3; 4 |].asRarr; [|3; 4; 5 |].asRarr  |].asRarr
        expectedArrays.[5,4] <- [| [|1; 2; 3; 4 |].asRarr; [|2; 3; 4; 5 |].asRarr  |].asRarr
        expectedArrays.[5,5] <- [| [|1; 2; 3; 4; 5 |].asRarr  |].asRarr

        for arraySize = 0 to 5 do
            for windowSize = -1 to 5 do
                if windowSize <= 0 then
                    CheckThrowsArgumentException (fun () -> Rarr.windowed windowSize [|1..arraySize |].asRarr |> ignore)
                elif arraySize < windowSize then
                    Assert.True(([| |].asRarr) =+= ( Rarr.windowed windowSize [|1..arraySize |].asRarr))
                else
                    Assert.True(( expectedArrays.[arraySize, windowSize]) =+= (Rarr.windowed windowSize [|1..arraySize |].asRarr))

        ()

    [<Fact>]
    member this.Zero_Create() =
        (*
        // Check for bogus input
        CheckThrowsArgumentException(fun () -> Rarr.zeroCreate -1 |> ignore)
        
        // integer array  
        let resultInt =  Rarr.zeroCreate 8 
        if resultInt <> [|0;0;0;0;0;0;0;0 |].asRarr then Assert.Fail()
        
        // string array    
        let resultStr = Rarr.zeroCreate 3 
        if resultStr <> [|null;null;null |].asRarr then Assert.Fail()
        
        // empty array     
        let resultEpt = Rarr.zeroCreate  0
        if resultEpt <> [| |].asRarr  then Assert.Fail()
        *)
        
        ()

    [<Fact>]
    member this.BadCreateArguments() =
        // negative number
        CheckThrowsArgumentException (fun () -> Rarr.create -1 0 |> ignore)

    [<Fact>]
    member this.Zip() =
        // integer array  
        let resultInt =  Rarr.zip [|1..3 |].asRarr [|2..2..6 |].asRarr 
        if resultInt <!> [|(1,2);(2,4);(3,6) |].asRarr then Assert.Fail()
        
        // string array    
        let resultStr = Rarr.zip [|"A"; "B";  "C" ; "D"  |].asRarr [|"a";"b";"c";"d" |].asRarr
        if resultStr <!> [|("A","a");("B","b");("C","c");("D","d") |].asRarr then Assert.Fail()
        
        // empty array     
        let resultEpt = Rarr.zip  [| |].asRarr [| |].asRarr
        if resultEpt <!> [| |].asRarr  then Assert.Fail()

        // null array
        // Rarr cant be null: let nullArr = null:string[]      
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.zip nullArr   nullArr  |> ignore)  
        
        // len1 <!> len2
        CheckThrowsArgumentException(fun () -> Rarr.zip [|1..10 |].asRarr [|2..20 |].asRarr |> ignore)
        
        ()

    [<Fact>]
    member this.Zip3() =
        // integer array  
        let resultInt =  Rarr.zip3 [|1..3 |].asRarr [|2..2..6 |].asRarr [|3;6;9 |].asRarr
        if resultInt <!> [|(1,2,3);(2,4,6);(3,6,9) |].asRarr then Assert.Fail()
        
        // string array    
        let resultStr = Rarr.zip3 [|"A"; "B";  "C" ; "D"  |].asRarr  [|"1";"2";"3";"4" |].asRarr  [|"a"; "b"; "c"; "d" |].asRarr
        let str = resultStr.ToString()
        if resultStr <!> [|("A","1","a");("B","2","b");("C","3","c");("D","4","d") |].asRarr then Assert.Fail()
        
        // empty array     
        let resultEpt = Rarr.zip3  [| |].asRarr [| |].asRarr [| |].asRarr
        if resultEpt <!> [| |].asRarr  then Assert.Fail()

        // null array
        // Rarr cant be null: let nullArr = null:string[]      
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.zip3 nullArr  nullArr  nullArr  |> ignore)  
        
        // len1 <> len2
        CheckThrowsArgumentException(fun () -> Rarr.zip3 [|1..10 |].asRarr [|2..20 |].asRarr [|1..10 |].asRarr |> ignore)
        // len1 <> len3
        CheckThrowsArgumentException(fun () -> Rarr.zip3 [|1..10 |].asRarr [|1..10 |].asRarr [|2..20 |].asRarr |> ignore)
        
        ()

    [<Fact>]
    member this.Item() =
        // integer array
        let resultInt = Rarr.item 3 [|1..8 |].asRarr
        Assert.AreEqual(4, resultInt)

        // string array
        let resultStr = Rarr.item 2 [|"Arrays"; "are"; "commonly"; "array"  |].asRarr
        Assert.AreEqual("commonly", resultStr)

        // empty array
        CheckThrowsIndexOutRangException(fun () -> Rarr.item 0 ([|  |].asRarr : decimal Rarr) |> ignore)

        // null array
        // Rarr cant be null: let nullArr = null:string[]
        // Rarr cant be null: CheckThrowsNullRefException (fun () -> Rarr.item 0 nullArr |> ignore)

        // Negative index
        for i = -1 downto -10 do
           CheckThrowsIndexOutRangException (fun () -> Rarr.item i [|1..8 |].asRarr |> ignore)

        // Out of range
        for i = 11 to 20 do
           CheckThrowsIndexOutRangException (fun () -> Rarr.item i [|1..8 |].asRarr |> ignore)

    [<Fact>]
    member this.tryItem() =
        // integer array
        let intArr = [| 3;4;7;8;10  |].asRarr
        let resultInt = Rarr.tryItem 3 intArr
        Assert.AreEqual(Some(8), resultInt)

        // string array
        let strArr = [| "Lists"; "are"; "commonly"; "list"  |].asRarr
        let resultStr = Rarr.tryItem 1 strArr
        Assert.AreEqual(Some("are"), resultStr)

        // empty array
        let emptyArr:int Rarr = [|  |].asRarr
        let resultEmpty = Rarr.tryItem 1 emptyArr
        Assert.AreEqual(None, resultEmpty)

        // null array
        // Rarr cant be null: let nullArr = null:string[]
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.tryItem 0 nullArr |> ignore)

        // Negative index
        let resultNegativeIndex = Rarr.tryItem -1 [| 3;1;6;2  |].asRarr
        Assert.AreEqual(None, resultNegativeIndex)

        // Index greater than length
        let resultIndexGreater = Rarr.tryItem 14 [| 3;1;6;2  |].asRarr
        Assert.AreEqual(None, resultIndexGreater)
    


    [<Fact>]
    member this.RemoveAt() =
        // integer list
        Assert.AreEqual([|2; 3; 4; 5|].asRarr , (Rarr.removeAt 0 [|1..5|].asRarr ))
        Assert.AreEqual([|1; 2; 4; 5|].asRarr , (Rarr.removeAt 2 [|1..5|].asRarr ))
        Assert.AreEqual([|1; 2; 3; 4|].asRarr , (Rarr.removeAt 4 [|1..5|].asRarr ))

        //string list
        Assert.AreEqual([|"2"; "3"; "4"; "5"|].asRarr , (Rarr.removeAt 0 [|"1"; "2"; "3"; "4"; "5"|].asRarr ))
        Assert.AreEqual([|"1"; "2"; "4"; "5"|].asRarr , (Rarr.removeAt 2 [|"1"; "2"; "3"; "4"; "5"|].asRarr ))
        Assert.AreEqual([|"1"; "2"; "3"; "4"|].asRarr , (Rarr.removeAt 4 [|"1"; "2"; "3"; "4"; "5"|].asRarr ))

        // empty list & out of bounds
        CheckThrowsArgumentException (fun () -> Rarr.removeAt 0 [||].asRarr  |> ignore)
        CheckThrowsArgumentException (fun () -> Rarr.removeAt -1 [|1|].asRarr  |> ignore)
        CheckThrowsArgumentException (fun () -> Rarr.removeAt 2 [|1|].asRarr  |> ignore)

    [<Fact>]
    member this.RemoveManyAt() =
        // integer list
        Assert.AreEqual([|3; 4; 5|].asRarr , (Rarr.removeManyAt 0 2 [|1..5|].asRarr ))
        Assert.AreEqual([|1; 2; 5|].asRarr , (Rarr.removeManyAt 2 2 [|1..5|].asRarr ))
        Assert.AreEqual([|1; 2; 3|].asRarr , (Rarr.removeManyAt 3 2 [|1..5|].asRarr ))

        //string list
        Assert.AreEqual([|"3"; "4"; "5"|].asRarr , (Rarr.removeManyAt 0 2 [|"1"; "2"; "3"; "4"; "5"|].asRarr ))
        Assert.AreEqual([|"1"; "2"; "5"|].asRarr , (Rarr.removeManyAt 2 2 [|"1"; "2"; "3"; "4"; "5"|].asRarr ))
        Assert.AreEqual([|"1"; "2"; "3"|].asRarr , (Rarr.removeManyAt 3 2 [|"1"; "2"; "3"; "4"; "5"|].asRarr ))

        // empty list & out of bounds
        CheckThrowsArgumentException (fun () -> Rarr.removeManyAt 0 2 [||].asRarr  |> ignore)
        CheckThrowsArgumentException (fun () -> Rarr.removeManyAt -1 2 [|1|].asRarr  |> ignore)
        CheckThrowsArgumentException (fun () -> Rarr.removeManyAt 2 2 [|1|].asRarr  |> ignore)

    [<Fact>]
    member this.UpdateAt() =
        // integer list
        Assert.AreEqual([|0; 2; 3; 4; 5|].asRarr , (Rarr.updateAt 0 0 [|1..5|].asRarr ))
        Assert.AreEqual([|1; 2; 0; 4; 5|].asRarr , (Rarr.updateAt 2 0 [|1..5|].asRarr ))
        Assert.AreEqual([|1; 2; 3; 4; 0|].asRarr , (Rarr.updateAt 4 0 [|1..5|].asRarr ))

        //string list
        Assert.AreEqual([|"0"; "2"; "3"; "4"; "5"|].asRarr , (Rarr.updateAt 0 "0" [|"1"; "2"; "3"; "4"; "5"|].asRarr ))
        Assert.AreEqual([|"1"; "2"; "0"; "4"; "5"|].asRarr , (Rarr.updateAt 2 "0" [|"1"; "2"; "3"; "4"; "5"|].asRarr ))
        Assert.AreEqual([|"1"; "2"; "3"; "4"; "0"|].asRarr , (Rarr.updateAt 4 "0" [|"1"; "2"; "3"; "4"; "5"|].asRarr ))

        // empty list & out of bounds
        CheckThrowsArgumentException (fun () -> Rarr.updateAt 0 0 [||].asRarr  |> ignore)
        CheckThrowsArgumentException (fun () -> Rarr.updateAt -1 0 [|1|].asRarr  |> ignore)
        CheckThrowsArgumentException (fun () -> Rarr.updateAt 2 0 [|1|].asRarr  |> ignore)

    [<Fact>]
    member this.InsertAt() =
        // integer list
        Assert.AreEqual([|0; 1; 2; 3; 4; 5|].asRarr , (Rarr.insertAt 0 0 [|1..5|].asRarr ))
        Assert.AreEqual([|1; 2; 0; 3; 4; 5|].asRarr , (Rarr.insertAt 2 0 [|1..5|].asRarr ))
        Assert.AreEqual([|1; 2; 3; 4; 0; 5|].asRarr , (Rarr.insertAt 4 0 [|1..5|].asRarr ))

        //string list
        Assert.AreEqual([|"0"; "1"; "2"; "3"; "4"; "5"|].asRarr , (Rarr.insertAt 0 "0" [|"1"; "2"; "3"; "4"; "5"|].asRarr ))
        Assert.AreEqual([|"1"; "2"; "0"; "3"; "4"; "5"|].asRarr , (Rarr.insertAt 2 "0" [|"1"; "2"; "3"; "4"; "5"|].asRarr ))
        Assert.AreEqual([|"1"; "2"; "3"; "4"; "0"; "5"|].asRarr , (Rarr.insertAt 4 "0" [|"1"; "2"; "3"; "4"; "5"|].asRarr ))

        // empty list & out of bounds
        Assert.AreEqual([0], Rarr.insertAt 0 0 [||].asRarr )
        CheckThrowsArgumentException (fun () -> Rarr.insertAt -1 0 [|1|].asRarr  |> ignore)
        CheckThrowsArgumentException (fun () -> Rarr.insertAt 2 0 [|1|].asRarr  |> ignore)

    [<Fact>]
    member this.InsertManyAt() =
        // integer list
        Assert.AreEqual([|0; 0; 1; 2; 3; 4; 5|].asRarr , (Rarr.insertManyAt 0 [0; 0] [|1..5|].asRarr ))
        Assert.AreEqual([|1; 2; 0; 0; 3; 4; 5|].asRarr , (Rarr.insertManyAt 2 [0; 0] [|1..5|].asRarr ))
        Assert.AreEqual([|1; 2; 3; 4; 0; 0; 5|].asRarr , (Rarr.insertManyAt 4 [0; 0] [|1..5|].asRarr ))

        //string list
        Assert.AreEqual([|"0"; "0"; "1"; "2"; "3"; "4"; "5"|].asRarr , (Rarr.insertManyAt 0 ["0"; "0"] [|"1"; "2"; "3"; "4"; "5"|].asRarr ))
        Assert.AreEqual([|"1"; "2"; "0"; "0"; "3"; "4"; "5"|].asRarr , (Rarr.insertManyAt 2 ["0"; "0"] [|"1"; "2"; "3"; "4"; "5"|].asRarr ))
        Assert.AreEqual([|"1"; "2"; "3"; "4"; "0"; "0"; "5"|].asRarr , (Rarr.insertManyAt 4 ["0"; "0"] [|"1"; "2"; "3"; "4"; "5"|].asRarr ))

        // empty list & out of bounds
        Assert.AreEqual([0; 0], Rarr.insertManyAt 0 [0; 0] [||].asRarr )
        CheckThrowsArgumentException (fun () -> Rarr.insertManyAt -1 [0; 0] [|1|].asRarr  |> ignore)
        CheckThrowsArgumentException (fun () -> Rarr.insertManyAt 2 [0; 0] [|1|].asRarr  |> ignore) 