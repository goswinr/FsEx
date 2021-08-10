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


[<AutoOpen>]
module ExtensionOnArray = 
    type ``[]``<'T> with
        member a.asRarr = Rarr(a)

    //let inline (==) a b = Rarr.equals a b
    //let inline (=+=) a b = Rarr.equals2 a b
    //let inline (=++=) (a:Rarr<'T>*Rarr<'T>) (b:Rarr<'T>*Rarr<'T>) = Rarr.equals (fst a) (fst b) && Rarr.equals (snd a) (snd b)
    //let inline (<!!>) (a:Rarr<'T>*Rarr<'T>) (b:Rarr<'T>*Rarr<'T>) = not <| Rarr.equals (fst a) (fst b) && not <| Rarr.equals (snd a) (snd b)
    //let inline (<!!!>) (a:Rarr<'T>*Rarr<'T>*Rarr<'T>) (b:Rarr<'T>*Rarr<'T>*Rarr<'T>) = not <| Rarr.equals (t1 a) (t1 b) && not <| Rarr.equals (t2 a) (t2 b) && not <| Rarr.equals (t3 a) (t3 b)
    //let inline (<!>) a b = not <| Rarr.equals a b

    let inline (==) a b =  a = b
    let inline (=+=) a b =  a = b
    let inline (=++=) (a:Rarr<'T>*Rarr<'T>) (b:Rarr<'T>*Rarr<'T>) = a=b
    let inline (<!!>) (a:Rarr<'T>*Rarr<'T>) (b:Rarr<'T>*Rarr<'T>) = a<>b
    let inline (<!!!>) (a:Rarr<'T>*Rarr<'T>*Rarr<'T>) (b:Rarr<'T>*Rarr<'T>*Rarr<'T>) = a<>b
    let inline (<!>) a b = a<>b



    let eqi (rarr1: Rarr<'K*Rarr<'T>>) (rarr2: Rarr<'K*Rarr<'T>>) =
        if rarr1.Count <> rarr2.Count then false         
        else
            let rec eq i =
                if i < rarr1.Count then 
                    let i1,r1 = rarr1.[i]
                    let i2,r2 = rarr2.[i]
                    if Rarr.equals r1 r2 && i1=i2 then eq (i+1)
                    else false
                else
                    true
            eq 0
    
    let inline (<*>) a b = not <| eqi a b
    //let inline (=*=) a b = eqi a b

type TestRarrModule() =

    [<Fact>]
    member this.Empty() =
        let emptyArray = Rarr.empty
        if Rarr.length emptyArray <> 0 then Assert.Fail()    
        
        let c : int Rarr   = Rarr.empty<int>
        Assert.True( (c == [|   |].asRarr) )
        
        let d : string Rarr = Rarr.empty<string>
        Assert.True( (d == [|   |].asRarr) )
        ()


    [<Fact>]
    member this.AllPairs() =
        // integer array
        let resultInt =  Rarr.allPairs [|1..3  |].asRarr [|2..2..6  |].asRarr
        if resultInt <!> [|(1,2);(1,4);(1,6)
                           (2,2);(2,4);(2,6)
                           (3,2);(3,4);(3,6)  |].asRarr then Assert.Fail()

        // string array
        let resultStr = Rarr.allPairs [|"A"; "B"; "C" ; "D"   |].asRarr [|"a";"b";"c";"d"  |].asRarr
        if resultStr <!> [|("A","a");("A","b");("A","c");("A","d")
                           ("B","a");("B","b");("B","c");("B","d")
                           ("C","a");("C","b");("C","c");("C","d")
                           ("D","a");("D","b");("D","c");("D","d")  |].asRarr then Assert.Fail()

        // empty array
        if Rarr.allPairs [| |].asRarr     [| |].asRarr <!> [| |].asRarr  then Assert.Fail()
        if Rarr.allPairs [|1..3 |].asRarr [| |].asRarr <!> [| |].asRarr  then Assert.Fail()
        if Rarr.allPairs [| |].asRarr [|1..3 |].asRarr <!> [| |].asRarr  then Assert.Fail()

        // null array
        // Rarr cant be null: let nullArr = null:string Rarr
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.allPairs nullArr nullArr  |> ignore)
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.allPairs [| |].asRarr    nullArr  |> ignore)
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.allPairs nullArr [| |].asRarr     |> ignore)

        ()

    [<Fact>]
    member this.Append() =
        // integer array
        let intArray = Rarr.append [| 1; 2  |].asRarr [| 3; 4  |].asRarr
        Assert.True( (intArray == [| 1; 2; 3; 4  |].asRarr) )

        // string array
        let strArray = Rarr.append [| "a"; "b"  |].asRarr [| "C"; "D"  |].asRarr
        Assert.True( (strArray == [| "a"; "b"; "C"; "D"  |].asRarr) )

        // empty array
        let emptyArray : int Rarr  = [|    |].asRarr
        let singleArray : int Rarr = [| 1  |].asRarr
        
        let appEmptySingle = Rarr.append emptyArray singleArray
        let appSingleEmpty = Rarr.append singleArray emptyArray
        
        Assert.True( (appEmptySingle == [| 1  |].asRarr) )
        Assert.True( (appSingleEmpty == [| 1  |].asRarr) )
      
        // null array
        // Rarr cant be null: let nullArray = null:int Rarr
        let validArray = [| 1  |].asRarr
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.append validArray nullArray |> ignore)    
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.append nullArray validArray |> ignore)   

        ()

    [<Fact>]
    member this.Average() =   
      
        // empty float32 array
        let emptyFloatArray = Rarr.empty<float32> 
        CheckThrowsArgumentException(fun () -> Rarr.average emptyFloatArray |> ignore)
        
        // empty double array
        let emptyDoubleArray = Rarr.empty<System.Double> 
        CheckThrowsArgumentException(fun () -> Rarr.average emptyDoubleArray |> ignore)
        
        // empty decimal array
        let emptyDecimalArray = Rarr.empty<System.Decimal> 
        CheckThrowsArgumentException (fun () -> Rarr.average emptyDecimalArray |>ignore )

        // float32 array
        let floatArray: float32 Rarr = [| 1.2f; 3.5f; 6.7f  |].asRarr
        let averageOfFloat = Rarr.average floatArray
        if averageOfFloat <> 3.8000000000000003f then Assert.Fail()
        
        // double array
        let doubleArray: System.Double Rarr = [| 1.0;8.0  |].asRarr
        let averageOfDouble = Rarr.average doubleArray
        if averageOfDouble <> 4.5 then Assert.Fail()
        
        // decimal array
        let decimalArray: decimal Rarr = [| 0M; 19M; 19.03M  |].asRarr
        let averageOfDecimal = Rarr.average decimalArray
        if averageOfDecimal <> 12.676666666666666666666666667M then Assert.Fail()      
        
        // null array
        // Rarr cant be null: let nullArr = null : double Rarr    
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.average nullArr |> ignore) 

        ()

    [<Fact>]
    member this.AverageBy() =

        // empty double array
        let emptyDouArray = Rarr.empty<System.Double>
        CheckThrowsArgumentException(fun () -> Rarr.averageBy (fun x -> x + 6.7) emptyDouArray |> ignore)

        // empty float32 array
        let emptyFloat32Array: float32 Rarr = [| |].asRarr
        CheckThrowsArgumentException(fun () -> Rarr.averageBy (fun x -> x + 9.8f) emptyFloat32Array |> ignore)

        // empty decimal array
        let emptyDecimalArray = Rarr.empty<System.Decimal>
        CheckThrowsArgumentException(fun () -> Rarr.averageBy (fun x -> x + 9.8M) emptyDecimalArray |> ignore)

        // float32 array
        let floatArray: float32 Rarr = [| 1.5f; 2.5f; 3.5f; 4.5f  |].asRarr // using values that behave nicely with IEEE floats
        let averageOfFloat = Rarr.averageBy (fun x -> x + 1.0f) floatArray
        Assert.AreEqual(4.0f, averageOfFloat)

        // double array
        let doubleArray: System.Double Rarr = [| 1.0; 8.0  |].asRarr // using values that behave nicely with IEEE doubles
        let averageOfDouble = Rarr.averageBy (fun x -> x + 1.0) doubleArray
        Assert.AreEqual(5.5, averageOfDouble)

        // decimal array
        let decimalArray: decimal Rarr = [| 0M;19M;19.03M  |].asRarr
        let averageOfDecimal = Rarr.averageBy (fun x -> x + 9.8M) decimalArray
        Assert.AreEqual(22.476666666666666666666666667M, averageOfDecimal)

        // null array
        // Rarr cant be null: let nullArr : double Rarr = null
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.averageBy (fun x -> x + 6.7) nullArr |> ignore)

        ()

    [<Fact>]
    member this.ChunkBySize() =

        // int Seq
        Assert.True([| [|1..4 |].asRarr; [|5..8 |].asRarr  |].asRarr =+= Rarr.chunkBySize 4 [|1..8 |].asRarr)
        Assert.True([| [|1..4 |].asRarr; [|5..8 |].asRarr; [|9..10 |].asRarr  |].asRarr =+= Rarr.chunkBySize 4 [|1..10 |].asRarr)
        Assert.True([| [|1 |].asRarr; [|2 |].asRarr; [|3 |].asRarr; [|4 |].asRarr  |].asRarr =+= Rarr.chunkBySize 1 [|1..4 |].asRarr)
        Assert.True([| [|1..3 |].asRarr; [|4 |].asRarr  |].asRarr =+= Rarr.chunkBySize 3 [|1..4 |].asRarr)
        Assert.True([| [|1..5 |].asRarr; [|6..10 |].asRarr; [|11..12 |].asRarr  |].asRarr =+= Rarr.chunkBySize 5 [|1..12 |].asRarr)

        // string Seq
        Assert.True([| [|"a"; "b" |].asRarr; [|"c";"d" |].asRarr; [|"e" |].asRarr  |].asRarr =+= Rarr.chunkBySize 2 [|"a";"b";"c";"d";"e" |].asRarr)

        // empty Seq
        Assert.True([| |].asRarr =+= Rarr.chunkBySize 3 [| |].asRarr)

        // null Seq
        // Rarr cant be null: let nullArr:_ Rarr = null
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.chunkBySize 3 nullArr |> ignore)

        // invalidArg
        CheckThrowsArgumentException (fun () -> Rarr.chunkBySize 0 [|1..10 |].asRarr |> ignore)
        CheckThrowsArgumentException (fun () -> Rarr.chunkBySize -1 [|1..10 |].asRarr |> ignore)

        ()

    [<Fact>]
    member this.SplitInto() =

        // int array
        Assert.True([| [|1..4 |].asRarr; [|5..7 |].asRarr; [|8..10 |].asRarr  |].asRarr =+= Rarr.splitInto 3 [|1..10 |].asRarr)
        Assert.True([| [|1..4 |].asRarr; [|5..8 |].asRarr; [|9..11 |].asRarr  |].asRarr =+= Rarr.splitInto 3 [|1..11 |].asRarr)
        Assert.True([| [|1..4 |].asRarr; [|5..8 |].asRarr; [|9..12 |].asRarr  |].asRarr =+= Rarr.splitInto 3 [|1..12 |].asRarr)

        Assert.True([| [|1..2 |].asRarr; [|3 |].asRarr; [|4 |].asRarr; [|5 |].asRarr  |].asRarr =+= Rarr.splitInto 4 [|1..5 |].asRarr)
        Assert.True([| [|1 |].asRarr; [|2 |].asRarr; [|3 |].asRarr; [|4 |].asRarr  |].asRarr =+= Rarr.splitInto 20 [|1..4 |].asRarr)

        // string array
        Assert.True([| [|"a"; "b" |].asRarr; [|"c";"d" |].asRarr; [|"e" |].asRarr  |].asRarr =+= Rarr.splitInto 3 [|"a";"b";"c";"d";"e" |].asRarr)

        // empty array
        Assert.True([|  |].asRarr =+= Rarr.splitInto 3 [|  |].asRarr)

        // null array
        // Rarr cant be null: let nullArr:_ Rarr = null
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.splitInto 3 nullArr |> ignore)

        // invalidArg
        CheckThrowsArgumentException (fun () -> Rarr.splitInto 0 [|1..10 |].asRarr |> ignore)
        CheckThrowsArgumentException (fun () -> Rarr.splitInto -1 [|1..10 |].asRarr |> ignore)

        ()

    [<Fact>]
    member this.distinct() =
        // distinct should work on empty array
        Assert.AreEqual([| |].asRarr, Rarr.distinct [| |].asRarr)

        // distinct not should work on null
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.distinct null |> ignore)

        // distinct should filter out simple duplicates
        Assert.AreEqual([|1 |].asRarr, Rarr.distinct [|1 |].asRarr)
        Assert.AreEqual([|1 |].asRarr, Rarr.distinct [|1; 1 |].asRarr)
        Assert.AreEqual([|1; 2; 3 |].asRarr, Rarr.distinct [|1; 2; 3; 1 |].asRarr)
        Assert.AreEqual([|[1;2]; [1;3] |].asRarr, Rarr.distinct [|[1;2]; [1;3]; [1;2]; [1;3] |].asRarr)
        Assert.AreEqual([|[1;1]; [1;2]; [1;3]; [1;4] |].asRarr, Rarr.distinct [|[1;1]; [1;2]; [1;3]; [1;4] |].asRarr)
        Assert.AreEqual([|[1;1]; [1;4] |].asRarr, Rarr.distinct [|[1;1]; [1;1]; [1;1]; [1;4] |].asRarr)

        Assert.AreEqual([|null |].asRarr, Rarr.distinct [|null |].asRarr)
        let list = new System.Collections.Generic.List<int>()
        Assert.AreEqual([|null, list |].asRarr, Rarr.distinct [|null, list |].asRarr)
        
    [<Fact>]
    member this.distinctBy() =
        // distinctBy should work on empty array
        Assert.AreEqual([| |].asRarr, Rarr.distinctBy (fun _ -> failwith "should not be executed") [| |].asRarr)

        // distinctBy should not work on null
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.distinctBy (fun _ -> failwith "should not be executed") null |> ignore)

        // distinctBy should filter out simple duplicates
        Assert.AreEqual([|1 |].asRarr, Rarr.distinctBy id [|1 |].asRarr)
        Assert.AreEqual([|1 |].asRarr, Rarr.distinctBy id [|1; 1 |].asRarr)
        Assert.AreEqual([|1; 2; 3 |].asRarr, Rarr.distinctBy id [|1; 2; 3; 1 |].asRarr)

        // distinctBy should use the given projection to filter out simple duplicates
        Assert.AreEqual([|1 |].asRarr, Rarr.distinctBy (fun x -> x / x) [|1; 2 |].asRarr)
        Assert.AreEqual([|1; 2 |].asRarr, Rarr.distinctBy (fun x -> if x < 3 then x else 1) [|1; 2; 3; 4 |].asRarr) 
        Assert.AreEqual([| [1;2]; [1;3] |].asRarr, Rarr.distinctBy (fun x -> List.sum x) [| [1;2]; [1;3]; [2;1] |].asRarr)

        Assert.AreEqual([|null |].asRarr, Rarr.distinctBy id [|null |].asRarr)
        let list = new System.Collections.Generic.List<int>()
        Assert.AreEqual([|null, list |].asRarr, Rarr.distinctBy id [|null, list |].asRarr)

    [<Fact>]
    member this.Except() =
        // integer array
        let intArr1 = [| yield! {1..100}
                         yield! {1..100}  |].asRarr
        let intArr2 = [| 1 .. 10  |].asRarr
        let expectedIntArr = [| 11 .. 100  |].asRarr

        Assert.AreEqual(expectedIntArr, Rarr.except intArr2 intArr1)

        // string array
        let strArr1 = [| "a"; "b"; "c"; "d"; "a"  |].asRarr
        let strArr2 = [| "b"; "c"  |].asRarr
        let expectedStrArr = [| "a"; "d"  |].asRarr

        Assert.AreEqual(expectedStrArr, Rarr.except strArr2 strArr1)

        // empty array
        let emptyIntArr = [|  |].asRarr
        Assert.AreEqual([|1..100 |].asRarr, Rarr.except emptyIntArr intArr1)
        Assert.AreEqual(emptyIntArr, Rarr.except intArr1 emptyIntArr)
        Assert.AreEqual(emptyIntArr, Rarr.except emptyIntArr emptyIntArr)
        Assert.AreEqual(emptyIntArr, Rarr.except intArr1 intArr1)

        // null array
        // Rarr cant be null: let nullArr : int  Rarr = null
        // Rarr cant be null: CheckThrowsArgumentNullException(fun () -> Rarr.except nullArr emptyIntArr |> ignore)
        // Rarr cant be null: CheckThrowsArgumentNullException(fun () -> Rarr.except emptyIntArr nullArr |> ignore)
        // Rarr cant be null: CheckThrowsArgumentNullException(fun () -> Rarr.except nullArr nullArr |> ignore)

        ()

    [<Fact>]
    member this.Take() =
        Assert.AreEqual([| |].asRarr, Rarr.take 0 [| |].asRarr)
        Assert.AreEqual([| |].asRarr, Rarr.take 0 [|"str1";"str2";"str3";"str4" |].asRarr)
        Assert.AreEqual([|1;2;4 |].asRarr, Rarr.take 3 [|1;2;4;5;7 |].asRarr)
        Assert.AreEqual([|"str1";"str2" |].asRarr, Rarr.take 2 [|"str1";"str2";"str3";"str4" |].asRarr)
        Assert.AreEqual( [|"str1";"str2";"str3";"str4" |].asRarr, Rarr.take 4 [|"str1";"str2";"str3";"str4" |].asRarr)

        CheckThrowsArgumentException (fun () -> Rarr.take 1 [| |].asRarr |> ignore)
        CheckThrowsArgumentException (fun () -> Rarr.take -1 [|0;1 |].asRarr |> ignore)
        CheckThrowsArgumentException (fun () -> Rarr.take 5 [|"str1";"str2";"str3";"str4" |].asRarr |> ignore)
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.take 5 null |> ignore)
        
    [<Fact>]
    member this.takeWhile() =
        Assert.AreEqual([| |].asRarr, Rarr.takeWhile (fun x -> failwith "should not be used") [| |].asRarr)
        Assert.AreEqual([|1;2;4;5 |].asRarr, Rarr.takeWhile (fun x -> x < 6) [|1;2;4;5;6;7 |].asRarr)
        Assert.AreEqual([|"a"; "ab"; "abc" |].asRarr, Rarr.takeWhile (fun (x:string) -> x.Length < 4) [|"a"; "ab"; "abc"; "abcd"; "abcde" |].asRarr)
        Assert.AreEqual([|"a"; "ab"; "abc"; "abcd"; "abcde" |].asRarr, Rarr.takeWhile (fun _ -> true) [|"a"; "ab"; "abc"; "abcd"; "abcde" |].asRarr)
        Assert.AreEqual([| |].asRarr, Rarr.takeWhile (fun _ -> false) [|"a"; "ab"; "abc"; "abcd"; "abcde" |].asRarr)
        Assert.AreEqual([| |].asRarr, Rarr.takeWhile (fun _ -> false) [|"a" |].asRarr)
        Assert.AreEqual([|"a" |].asRarr, Rarr.takeWhile (fun _ -> true) [|"a" |].asRarr)
        Assert.AreEqual([|"a" |].asRarr, Rarr.takeWhile (fun x -> x <> "ab") [|"a"; "ab"; "abc"; "abcd"; "abcde" |].asRarr)

        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.takeWhile (fun _ -> failwith "should not be used") null |> ignore) 

    [<Fact>]
    member this.splitAt() =        
        Assert.AreEqual([| |].asRarr, Rarr.splitAt 0 [| |].asRarr |> fst)  
        Assert.AreEqual([| |].asRarr, Rarr.splitAt 0 [| |].asRarr |> snd)

        Assert.AreEqual([|1..4 |].asRarr, Rarr.splitAt 4 [|1..10 |].asRarr |> fst)       
        Assert.AreEqual([|5..10 |].asRarr, Rarr.splitAt 4 [|1..10 |].asRarr |> snd)      

        Assert.AreEqual([| |].asRarr, Rarr.splitAt 0 [|1..2 |].asRarr |> fst)
        Assert.AreEqual([|1..2 |].asRarr, Rarr.splitAt 0 [|1..2 |].asRarr |> snd)

        Assert.AreEqual([|1 |].asRarr, Rarr.splitAt 1 [|1..2 |].asRarr |> fst)
        Assert.AreEqual([|2 |].asRarr, Rarr.splitAt 1 [|1..2 |].asRarr |> snd)

        Assert.AreEqual([|1..2 |].asRarr, Rarr.splitAt 2 [|1..2 |].asRarr |> fst)
        Assert.AreEqual([| |].asRarr, Rarr.splitAt 2 [|1..2 |].asRarr |> snd)

        Assert.AreEqual([|"a" |].asRarr, Rarr.splitAt 1 [|"a";"b";"c" |].asRarr |> fst)
        Assert.AreEqual([|"b";"c" |].asRarr, Rarr.splitAt 1 [|"a";"b";"c" |].asRarr |> snd)

        // split should fail if index exceeds bounds
        CheckThrowsArgumentException (fun () -> Rarr.splitAt 1 [| |].asRarr |> ignore)
        CheckThrowsArgumentException (fun () -> Rarr.splitAt -1 [|0;1 |].asRarr |> ignore)
        CheckThrowsArgumentException (fun () -> Rarr.splitAt 5 [|"str1";"str2";"str3";"str4" |].asRarr |> ignore)
        
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.splitAt 0 null |> ignore)
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.splitAt 1 null |> ignore)

    [<Fact>]
    member this.replicate() =
        // replicate should create multiple copies of the given value
        Assert.AreEqual([| |].asRarr, Rarr.replicate 0 null)
        Assert.AreEqual([| |].asRarr, Rarr.replicate 0 1)
        Assert.AreEqual([|null |].asRarr, Rarr.replicate 1 null)
        Assert.AreEqual([|"1";"1" |].asRarr, Rarr.replicate 2 "1")

        CheckThrowsArgumentException (fun () ->  Rarr.replicate -1 null |> ignore)
        
    [<Fact>]
    member this.Blit() = 
        // int array   
        let intSrc = [| 1..10  |].asRarr
        let intDes:int Rarr = Rarr.create 10 Unchecked.defaultof<_> 
        Rarr.blit intSrc 0 intDes 0 5
        
        if intDes.[4] <> 5 then Assert.Fail()
        if intDes.[5] <> 0 then Assert.Fail()
        
        // string array
        let strSrc = [| "a";"b";"c";"d";"e";"j" |].asRarr
        let strDes = Rarr.create 10 "w"
        Rarr.blit strSrc 1 strDes 2 3
        if strDes.[3] <> "c" || Rarr.get strDes 4 = "w" then Assert.Fail()
     
        // null array
        // Rarr cant be null: let nullArr = null:string Rarr
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.blit nullArr 1 strDes 2 3 |> ignore) 

        // bounds check
        CheckThrowsArgumentException (fun () -> Rarr.blit intSrc -1 intDes 1 3 |> ignore)
        CheckThrowsArgumentException (fun () -> Rarr.blit intSrc 1 intDes -1 3 |> ignore)
        CheckThrowsArgumentException (fun () -> Rarr.blit intSrc 1 intDes 1 -3 |> ignore)
        CheckThrowsArgumentException (fun () -> Rarr.blit intSrc 1 intDes 1 300 |> ignore)
        CheckThrowsArgumentException (fun () -> Rarr.blit intSrc 1 intDes 5 8 |> ignore)
        
        ()

    [<Fact>]
    member this.BlitExtend() = 
        // int array   
        let input = [| 10..19  |].asRarr
        let insert1  = [| 0..19 |].asRarr
        let insert2 = [| 0..11 |].asRarr
        let expected  = [| 0..19 |].asRarr
        Rarr.blitExtend input 0 insert1 10 10
        Assert.AreEqual(expected,expected)
        
        Rarr.blitExtend input 0 insert1 10 10
        Assert.AreEqual(expected,expected)
        // TODO add more
        
        ()

      
    member private this.ChooseTester chooseInt chooseString = 
        // int array
        let intSrc:int  Rarr = [| 1..100  |].asRarr    
        let funcInt x = if (x%5=0) then Some x else None       
        let intChoosed : int Rarr = chooseInt funcInt intSrc
        if intChoosed.[1] <> 10 then Assert.Fail()
        
        // string array
        let stringSrc: string  Rarr = "Lists are a commonly used data structure. They are not mutable, i.e., you can't delete an element of a list  instead you create a new list with the element deleted. List values often share storage under the hood, i.e., a list value only allocate more memory when you actually execute construction operations.".Split([|' ' |], System.StringSplitOptions.RemoveEmptyEntries).asRarr
        let funcString x = match x with
                           | "list"-> Some x
                           | "List" -> Some x
                           | _ -> None
        let strChoosed : string Rarr  = chooseString funcString stringSrc   
        if strChoosed.[1].ToLower() <> "list" then Assert.Fail()
        
        // empty array
        let emptySrc :int Rarr = [|  |].asRarr
        let emptyChoosed = chooseInt funcInt emptySrc
        Assert.True( (emptyChoosed == [|  |].asRarr) )

        // null array
        // Rarr cant be null: let nullArr = null:int Rarr    
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> chooseInt funcInt nullArr |> ignore) 
        
        () 
      
    [<Fact>]
    member this.Choose() = 
        this.ChooseTester Rarr.choose Rarr.choose

    [<Fact>]
    member this.``Parallel.Choose`` () = 
        this.ChooseTester Rarr.Parallel.choose Rarr.Parallel.choose

    member private this.CollectTester collectInt collectString =
    
        // int array - checking ordering
        let intSrc  = [| 1..3  |].asRarr
        //let func : int-> #seq<int> = fun i -> [| 1..i  |].asRarr
        let func  = fun i -> [| 1..i  |].asRarr
        let result : int Rarr = collectInt func intSrc
        Assert.AreEqual ([| 1; 1; 2; 1; 2; 3  |].asRarr, result)
        
        // string array
        let stringSrc = [| "foo"; "bar"  |].asRarr
        let func = fun s -> [| s  |].asRarr
        let result : string Rarr = collectString func stringSrc
        Assert.AreEqual(stringSrc, result)
        
        // empty array
        let emptyArray : string  Rarr = [|  |].asRarr
        let result = collectString func emptyArray
        Assert.AreEqual(emptyArray,result)
        
        // null array
        // Rarr cant be null: let nullArr = null:int Rarr
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> collectInt func nullArr |> ignore)
        
        ()

    [<Fact>]
    member this.Collect () =
        ()
        this.CollectTester Rarr.collect Rarr.collect
        
    [<Fact>]
    member this.CollectWithSideEffects () =
        let stamp = ref 0
        let f x = 
            stamp := !stamp + 1 
            [| x  |].asRarr
        
        Rarr.collect f [|  |].asRarr |> ignore
        Assert.AreEqual(0, !stamp)
        
        stamp := 0
        Rarr.collect f [|1;2;3 |].asRarr |> ignore
        Assert.AreEqual(3,!stamp)
        
    [<Fact>]
    member this.``Parallel.Collect`` () =
        this.CollectTester Rarr.Parallel.collect Rarr.Parallel.collect

    [<Fact>]
    member this.compareWith() =
        // compareWith should work on empty arrays
        Assert.AreEqual(0, Rarr.compareWith (fun _ -> failwith "should not be executed")  [| |].asRarr [| |].asRarr)
        Assert.AreEqual(-1, Rarr.compareWith (fun _ -> failwith "should not be executed") [| |].asRarr [|1 |].asRarr)
        Assert.AreEqual(1, Rarr.compareWith (fun _ -> failwith "should not be executed")  [|"1" |].asRarr [| |].asRarr)

        // compareWith should not work on null arrays          
        // Rarr cant be null: CheckThrowsArgumentNullException(fun () -> Rarr.compareWith (fun _ -> failwith "should not be executed") null [| |].asRarr |> ignore)
        // Rarr cant be null: CheckThrowsArgumentNullException(fun () -> Rarr.compareWith (fun _ -> failwith "should not be executed") [| |].asRarr null |> ignore)
    
        // compareWith should work on longer arrays
        Assert.AreEqual(-1, Rarr.compareWith compare [|"1";"2" |].asRarr [|"1";"3" |].asRarr)
        Assert.AreEqual(1, Rarr.compareWith compare [|1;2;43 |].asRarr [|1;2;1 |].asRarr)
        Assert.AreEqual(1, Rarr.compareWith compare [|1;2;3;4 |].asRarr [|1;2;3 |].asRarr)
        Assert.AreEqual(0, Rarr.compareWith compare [|1;2;3;4 |].asRarr [|1;2;3;4 |].asRarr)
        Assert.AreEqual(-1, Rarr.compareWith compare [|1;2;3 |].asRarr [|1;2;3;4 |].asRarr)
        Assert.AreEqual(1, Rarr.compareWith compare [|1;2;3 |].asRarr [|1;2;2;4 |].asRarr)
        Assert.AreEqual(-1, Rarr.compareWith compare [|1;2;2 |].asRarr [|1;2;3;4 |].asRarr)

        // compareWith should use the comparer
        Assert.AreEqual(0, Rarr.compareWith (fun x y -> 0) [|"1";"2" |].asRarr [|"1";"3" |].asRarr)
        Assert.AreEqual(1, Rarr.compareWith (fun x y -> 1) [|"1";"2" |].asRarr [|"1";"3" |].asRarr)
        Assert.AreEqual(-1, Rarr.compareWith (fun x y -> -1) [|"1";"2" |].asRarr [|"1";"3" |].asRarr)
        
    [<Fact>]
    member this.Concat() =
        // integer array
        let seqInt = 
            seq { for i in 1..10 do                
                    yield [|i; i*10 |].asRarr }
                    
        let conIntArr = Rarr.concat seqInt
        if Rarr.length conIntArr <> 20 then Assert.Fail()
        
        // string array
        let strSeq = 
            seq { for a in 'a'..'c' do
                    for b in 'a'..'c' do
                        yield [|a.ToString();b.ToString()  |].asRarr}
     
        let conStrArr = Rarr.concat strSeq
        if Rarr.length conStrArr <> 18 then Assert.Fail()
        
        // Empty array
        let emptyArrays = [| [|  |].asRarr; [| 0  |].asRarr; [| 1  |].asRarr; [|  |].asRarr; [|  |].asRarr  |].asRarr
        let result2 = Rarr.concat emptyArrays
        Assert.True(result2.[0] = 0 && result2.[1] = 1)
        if result2.[0] <> 0 && result2.[1] <> 1 then Assert.Fail()    

        // null array
        // Rarr cant be null: let nullArray = null:int Rarr
        // Rarr cant be null: let nullArrays = Rarr.create 2 nullArray
        // Rarr cant be null: CheckThrowsNullRefException (fun () -> Rarr.concat nullArrays |> ignore) 
                
        () 

    [<Fact>]
    member this.countBy() =
        // countBy should work on empty array
        Assert.AreEqual(0, Rarr.countBy (fun _ -> failwith "should not be executed") [| |].asRarr |> Rarr.length)

        // countBy should not work on null
        // Rarr cant be null: CheckThrowsArgumentNullException(fun () -> Rarr.countBy (fun _ -> failwith "should not be executed") null |> ignore)

        // countBy should count by the given key function
        Assert.AreEqual([| 5,1; 2,2; 3,2  |].asRarr, Rarr.countBy id [|5;2;2;3;3 |].asRarr)
        Assert.AreEqual([| 3,3; 2,2; 1,3  |].asRarr, Rarr.countBy (fun x -> if x < 3 then x else 3) [|5;2;1;2;3;3;1;1 |].asRarr)

    [<Fact>]
    member this.Copy() =
        // int array
        let intSrc:int  Rarr = [| 3;5;7  |].asRarr    
        let intCopyed = Rarr.copy  intSrc
        if intCopyed <!> [| 3;5;7  |].asRarr then Assert.Fail()
        
        // string array
        let stringSrc: string  Rarr = [|"Lists"; "are";  "commonly"   |].asRarr
        
        let strCopyed = Rarr.copy  stringSrc   
        if strCopyed <!> [|"Lists"; "are";  "commonly"   |].asRarr then Assert.Fail()
        
        // empty array
        let emptySrc :int Rarr = [|  |].asRarr
        let emptyCopyed = Rarr.copy emptySrc
        if emptyCopyed <!> [|  |].asRarr then Assert.Fail()

        // null array
        // Rarr cant be null: let nullArr = null:int Rarr    
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.copy nullArr |> ignore) 
        
        ()

    [<Fact>]
    member this.Create() =
        // int array
        let intArr = Rarr.create 3 8    
        if intArr <!> [| 8; 8; 8 |].asRarr then Assert.Fail()
        
        // string array
        let strArr = Rarr.create 3 "good"
        Assert.True( (strArr == [|"good"; "good";  "good" |].asRarr) )
        
        // empty array
        let emptyArr = Rarr.create 0 "empty"    
        if emptyArr <!> [|  |].asRarr then Assert.Fail()

        // array with null elements
        // Rarr cant be null: let nullStr = null:string  
        // Rarr cant be null: let nullArr = Rarr.create 3 nullStr
        // Rarr cant be null:Assert.True( (nullArr = [|null; null; null |].asRarr) )
        
        ()

    
    [<Fact>]
    member this.TryHead() =
        // integer array
        let resultInt = Rarr.tryHead  [|2..2..20 |].asRarr        
        Assert.AreEqual(2, resultInt.Value)
        
        // string array
        let resultStr = Rarr.tryHead  [|"a";"b";"c";"d" |].asRarr         
        Assert.AreEqual("a", resultStr.Value)

        // empty array   
        let resultNone = Rarr.tryHead [| |].asRarr
        Assert.AreEqual(None, resultNone)

        // null array
        // Rarr cant be null: let nullArr = null:string Rarr      
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.tryHead nullArr |> ignore) 
        ()
        
    [<Fact>]
    member this.Exists() =
        // integer array
        let intArr = [| 2;4;6;8  |].asRarr
        let funcInt x = if (x%2 = 0) then true else false
        let resultInt = Rarr.exists funcInt intArr
        if resultInt <> true then Assert.Fail()
        
        // string array
        let strArr = [|"Lists"; "are";  "commonly"  |].asRarr
        let funcStr (x:string) = if (x.Length >15) then true else false
        let resultStr = Rarr.exists funcStr strArr
        if resultStr <> false then Assert.Fail()
        
        // empty array
        let emptyArr:int Rarr = [|  |].asRarr
        let resultEpt = Rarr.exists funcInt emptyArr
        if resultEpt <> false then Assert.Fail()

        // null array
        // Rarr cant be null: let nullArr = null:string Rarr      
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.exists funcStr nullArr |> ignore) 
        
        ()
        
    [<Fact>]
    member this.Exists2() =
        // integer array
        let intFir = [| 2;4;6;8  |].asRarr
        let intSec = [| 1;2;3;4  |].asRarr
        let funcInt x y = if (x%y = 0) then true else false
        let resultInt = Rarr.exists2 funcInt intFir intSec
        if resultInt <> true then Assert.Fail()
        
        // string array
        let strFir = [|"Lists"; "are";  "commonly"  |].asRarr
        let strSec = [|"good"; "good";  "good"   |].asRarr
        let funcStr (x:string) (y:string) = if (x = y) then true else false
        let resultStr = Rarr.exists2 funcStr strFir strSec
        if resultStr <> false then Assert.Fail()
        
        // empty array
        let eptFir:int Rarr = [|  |].asRarr
        let eptSec:int Rarr = [|  |].asRarr
        let resultEpt = Rarr.exists2 funcInt eptFir eptSec
        if resultEpt <> false then Assert.Fail()

        // null array
        // Rarr cant be null: let nullFir = null:string Rarr 
        let validArray = [| "a"  |].asRarr      
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.exists2 funcStr nullFir validArray |> ignore)  
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.exists2 funcStr validArray nullFir |> ignore) 
        
        // len1 <> len2
        CheckThrowsArgumentException(fun () -> Rarr.exists2 funcInt [|1..10 |].asRarr [|2..20 |].asRarr |> ignore)
        
        ()

    [<Fact>]
    member this.Fill() =
        // integer array
        let intArr = [|1..5 |].asRarr
        Rarr.fill intArr 0 3 21
        if intArr <!> [|21;21;21;4;5 |].asRarr then Assert.Fail()
        
        // string array
        let strArr = [|"Lists"; "are"; "a"; "commonly"; "data";"structor"  |].asRarr
        Rarr.fill strArr 1 5 "a"
        
        if strArr <!> [|"Lists"; "a"; "a"; "a"; "a";"a"  |].asRarr then Assert.Fail()
        
        // empty array
        let emptyArr:int Rarr = [|  |].asRarr
        Rarr.fill emptyArr 0 0 8
        if emptyArr <!> [|  |].asRarr then Assert.Fail()

        // null array
        // Rarr cant be null: let nullArr = null:string Rarr 
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.fill nullArr 0 1 "good" |> ignore)
        
        // start < 0
        CheckThrowsArgumentException(fun () -> Rarr.fill intArr -1 3 21)
        
        // len < 0        
        CheckThrowsArgumentException(fun () -> Rarr.fill intArr 1 -2 21)
        
         
        ()

    [<Fact>] 
    member this.Filter() =
        // integer array
        let intArr = [| 1..20  |].asRarr
        let funcInt x = if (x%5 = 0) then true else false
        let resultInt = Rarr.filter funcInt intArr
        if resultInt <!> [|5;10;15;20 |].asRarr then Assert.Fail()
        
        // string array
        let strArr = [|"Lists"; "are"; "a"; "commonly"; "data";"structor"  |].asRarr
        let funcStr (x:string) = if (x.Length > 4) then true else false
        let resultStr = Rarr.filter funcStr strArr
        if resultStr <!> [|"Lists";  "commonly"; "structor"  |].asRarr then Assert.Fail()
        
        // empty array
        let emptyArr:int Rarr = [|  |].asRarr
        let resultEpt = Rarr.filter funcInt emptyArr
        if resultEpt <!> [|  |].asRarr then Assert.Fail()

        // null array
        // Rarr cant be null: let nullArr = null:string Rarr 
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () ->  Rarr.filter funcStr nullArr |> ignore) 
        
        ()
        
    [<Fact>]
    member this.Filter2 () =
        // The Rarr.filter algorithm uses a bitmask as a temporary storage mechanism
        // for which elements to filter. This introduces some possible error conditions
        // around how the filter is filled and subsequently used, so filter test
        // does a pretty exhaustive test suite.
        // It works by first generating arrays which consist of sequences of unique
        // positive and negative numbers, as per arguments, it then filters for the
        // positive values, and then compares the results against the original array.

        (*
        let makeTestArray size posLength negLength startWithPos startFromEnd =
            let array = Array.zeroCreate size

            let mutable sign  = if startWithPos then 1         else -1
            let mutable count = if startWithPos then posLength else negLength
            for i = 1 to size do
                let idx = if startFromEnd then size-i else i-1
                array.[idx] <- (idx+1) * sign
                count <- count - 1
                if count <= 0 then
                    sign <- sign * -1
                    count <- if sign > 0 then posLength else negLength

            array

        let checkFilter filter (array:array<_>) =
            let filtered = array |> filter (fun n -> n > 0)

            let mutable idx = 0
            for item in filtered do
                while array.[idx] < item do
                    idx <- idx + 1
                if item <> array.[idx] then
                    Assert.Fail ()
            idx <- idx + 1
            while idx < array.Length do
                if array.[idx] > 0 then
                    Assert.Fail ()
                idx <- idx + 1

        let checkCombinations filter maxSize =
            for size = 0 to maxSize do
                for posLength = 1 to size do
                    for negLength = 1 to size do
                        for startWithPos in [true; false] do
                            for startFromEnd in [true; false] do
                                let testArray = makeTestArray size posLength negLength startWithPos startFromEnd
                                checkFilter filter testArray

        // this could probably be a bit smaller, but needs to at least be > 64 to test chunk copying
        // of data, and > 96 gives a safer feel, so settle on a nice decimal rounding of one hundred
        // to appease those with digits.
        let suitableTestMaxLength = 100 

        checkCombinations Rarr.filter suitableTestMaxLength
        *)
        ()


    [<Fact>]
    member this.Where() =
        // integer array
        let intArr = [| 1..20  |].asRarr
        let funcInt x = if (x%5 = 0) then true else false
        let resultInt = Rarr.where funcInt intArr
        if resultInt <!> [|5;10;15;20 |].asRarr then Assert.Fail()
        
        // string array
        let strArr = [|"Lists"; "are"; "a"; "commonly"; "data";"structor"  |].asRarr
        let funcStr (x:string) = if (x.Length > 4) then true else false
        let resultStr = Rarr.where funcStr strArr
        if resultStr <!> [|"Lists";  "commonly"; "structor"  |].asRarr then Assert.Fail()
        
        // empty array
        let emptyArr:int Rarr = [|  |].asRarr
        let resultEpt = Rarr.where funcInt emptyArr
        if resultEpt <!> [|  |].asRarr then Assert.Fail()

        // null array
        // Rarr cant be null: let nullArr = null:string Rarr 
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () ->  Rarr.where funcStr nullArr |> ignore) 
        
        ()   

    [<Fact>]
    member this.``where should work like filter``() =
        Assert.AreEqual([| |].asRarr, Rarr.where (fun x -> x % 2 = 0) [| |].asRarr)
        Assert.AreEqual([|0;2;4;6;8 |].asRarr, Rarr.where (fun x -> x % 2 = 0) [|0..9 |].asRarr)
        Assert.AreEqual([|"a";"b";"c" |].asRarr, Rarr.where (fun _ -> true) [|"a";"b";"c" |].asRarr)

    [<Fact>]
    member this.Find() =
        // integer array
        let intArr = [| 1..20  |].asRarr
        let funcInt x = if (x%5 = 0) then true else false
        let resultInt = Rarr.find funcInt intArr
        if resultInt <> 5 then Assert.Fail()
        
        // string array
        let strArr = [|"Lists"; "are"; "a"; "commonly"; "data";"structor"  |].asRarr
        let funcStr (x:string) = if (x.Length >7) then true else false
        let resultStr = Rarr.find funcStr strArr
        if resultStr <> "commonly" then Assert.Fail()
        
        // empty array
        let emptyArr:int Rarr = [|  |].asRarr 
        CheckThrowsKeyNotFoundException (fun () -> Rarr.find (fun _ -> true) emptyArr |> ignore)

        // not found
        CheckThrowsKeyNotFoundException (fun () -> Rarr.find (fun _ -> false) intArr |> ignore)

        // null array
        // Rarr cant be null: let nullArr = null:string Rarr 
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.find funcStr nullArr |> ignore) 
        
        () 

    [<Fact>]
    member this.FindBack() =
        // integer array
        let funcInt x = if (x%5 = 0) then true else false
        Assert.AreEqual(20, Rarr.findBack funcInt [| 1..20  |].asRarr)
        Assert.AreEqual(15, Rarr.findBack funcInt [| 1..19  |].asRarr)
        Assert.AreEqual(5, Rarr.findBack funcInt [| 5..9  |].asRarr)

        // string array
        let strArr = [|"Lists"; "are"; "a"; "commonly"; "data";"structor"  |].asRarr
        let funcStr (x:string) = x.Length > 7
        let resultStr = Rarr.findBack funcStr strArr
        Assert.AreEqual("structor", resultStr)

        // empty array
        CheckThrowsKeyNotFoundException (fun () -> Rarr.findBack (fun _ -> true) [|  |].asRarr |> ignore)

        // not found
        CheckThrowsKeyNotFoundException (fun () -> Rarr.findBack (fun _ -> false) [| 1..20  |].asRarr |> ignore)

        // null array
        // Rarr cant be null: let nullArr = null:string Rarr
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.findBack funcStr nullArr |> ignore)

        ()

    [<Fact>]
    member this.FindIndex() =
        // integer array
        let intArr = [| 1..20  |].asRarr
        let funcInt x = if (x%5 = 0) then true else false
        let resultInt = Rarr.findIndex funcInt intArr
        if resultInt <> 4 then Assert.Fail()
        
        // string array
        let strArr = [|"Lists"; "are"; "a"; "commonly"; "data";"structor"  |].asRarr
        let funcStr (x:string) = if (x.Length >7) then true else false
        let resultStr = Rarr.findIndex funcStr strArr
        if resultStr <> 3 then Assert.Fail()
        
        // empty array
        let emptyArr:int Rarr = [|  |].asRarr  
        CheckThrowsKeyNotFoundException(fun() -> Rarr.findIndex (fun _ -> true) emptyArr |> ignore)
        
        // not found
        CheckThrowsKeyNotFoundException(fun() -> Rarr.findIndex (fun _ -> false) intArr |> ignore)

        // null array
        // Rarr cant be null: let nullArr = null:string Rarr  
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.findIndex funcStr nullArr |> ignore) 
        
        () 
        
    [<Fact>]
    member this.FindIndexBack() =
        // integer array
        let funcInt x = if (x%5 = 0) then true else false
        Assert.AreEqual(19, Rarr.findIndexBack funcInt [| 1..20  |].asRarr)
        Assert.AreEqual(14, Rarr.findIndexBack funcInt [| 1..19  |].asRarr)
        Assert.AreEqual(0, Rarr.findIndexBack funcInt [| 5..9  |].asRarr)

        // string array
        let strArr = [|"Lists"; "are"; "a"; "commonly"; "data";"structor"  |].asRarr
        let funcStr (x:string) = if (x.Length >7) then true else false
        let resultStr = Rarr.findIndexBack funcStr strArr
        Assert.AreEqual(5, resultStr)

        // empty array
        CheckThrowsKeyNotFoundException(fun() -> Rarr.findIndexBack (fun _ -> true) [|  |].asRarr |> ignore)

        // not found
        CheckThrowsKeyNotFoundException(fun() -> Rarr.findIndexBack (fun _ -> false) [| 1..20  |].asRarr |> ignore)

        // null array
        // Rarr cant be null: let nullArr = null:string Rarr
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.findIndexBack funcStr nullArr |> ignore)

        ()

    [<Fact>]
    member this.Pick() =
        // integers
        let intArr = [| 1..10  |].asRarr
        let matchFunc n =
            if n = 3 then Some(n.ToString())
            else None
        let resultInt = Rarr.pick matchFunc intArr
        Assert.AreEqual("3", resultInt)
        
        // make it not found
        CheckThrowsKeyNotFoundException (fun () -> Rarr.pick (fun n -> None) intArr |> ignore)

    [<Fact>]
    member this.last() =
        // last should fail on empty array
        CheckThrowsArgumentException(fun () -> Rarr.last [| |].asRarr |> ignore)

        // last should fail on null
        // Rarr cant be null: CheckThrowsArgumentNullException(fun () -> Rarr.last null |> ignore)

        // last should return the last element from arrays
        Assert.AreEqual(1, Rarr.last [|1 |].asRarr)
        Assert.AreEqual("2", Rarr.last [|"1"; "3"; "2" |].asRarr)
        Assert.AreEqual(["4"], Rarr.last [| ["1"; "3"]; []; ["4"]  |].asRarr)
    
    [<Fact>]
    member this.TryLast() =
        // integers array
        let IntSeq = [| 1..9  |].asRarr
        let intResult = Rarr.tryLast IntSeq
        Assert.AreEqual(9, intResult.Value)
                 
        // string array
        let strResult = Rarr.tryLast [|"first"; "second";  "third" |].asRarr
        Assert.AreEqual("third", strResult.Value)
         
        // Empty array
        let emptyResult = Rarr.tryLast Rarr.empty
        Assert.True(emptyResult.IsNone)
      
        // null array
        // Rarr cant be null: let nullArr = null:string Rarr  
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () ->Rarr.tryLast nullArr |> ignore) 
        () 

    [<Fact>]
    member this.ToSeq() =
        let intArr = [| 1..10  |].asRarr
        let seq = Rarr.toSeq intArr
        let sum = Seq.sum seq
        Assert.AreEqual(55, sum)
        
    [<Fact>]
    member this.TryPick() =
        // integer array
        let intArr = [| 1..10  |].asRarr    
        let funcInt x = 
                match x with
                | _ when x % 3 = 0 -> Some (x.ToString())            
                | _ -> None
        let resultInt = Rarr.tryPick funcInt intArr
        if resultInt <> Some "3" then Assert.Fail()
        
        // string array
        let strArr = [|"Lists"; "are";  "commonly" ; "list"  |].asRarr
        let funcStr x = 
                match x with
                | "good" -> Some (x.ToString())            
                | _ -> None
        let resultStr = Rarr.tryPick funcStr strArr
        if resultStr <> None then Assert.Fail()
        
        // empty array
        let emptyArr:int Rarr = [|  |].asRarr
        let resultEpt = Rarr.tryPick funcInt emptyArr
        if resultEpt <> None then Assert.Fail()

        // null array
        // Rarr cant be null: let nullArr = null:string Rarr  
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.tryPick funcStr nullArr |> ignore)  
        
        ()

    [<Fact>]
    member this.Fold() =
        // integer array
        let intArr = [| 1..5  |].asRarr    
        let funcInt x y = x+"+"+y.ToString()
        let resultInt = Rarr.fold funcInt "x" intArr
        if resultInt <> "x+1+2+3+4+5" then Assert.Fail()
        
        // string array
        let strArr = [|"A"; "B";  "C" ; "D"  |].asRarr
        let funcStr x y = x+y
            
        let resultStr = Rarr.fold funcStr "X" strArr
        if resultStr <> "XABCD" then Assert.Fail()
        
        // empty array
        let emptyArr : int Rarr = [|  |].asRarr
        let resultEpt = Rarr.fold funcInt "x" emptyArr
        if resultEpt <> "x" then Assert.Fail()

        // null array
        // Rarr cant be null: let nullArr = null : string Rarr 
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.fold funcStr "begin" nullArr |> ignore)  
        
        ()

    [<Fact>]
    member this.Fold2() =
        // integer array  
        let funcInt x y z = x + y.ToString() + z.ToString()
        let resultInt = Rarr.fold2 funcInt "x" [| 1;3;5  |].asRarr  [|2;4;6 |].asRarr
        if resultInt <> "x123456" then Assert.Fail()
        
        // string array
        let funcStr x y z= x + y + z        
        let resultStr = Rarr.fold2 funcStr "X" [|"A"; "B";  "C" ; "D"  |].asRarr [|"H"; "I";  "J" ; "K"  |].asRarr
        if resultStr <> "XAHBICJDK" then Assert.Fail()
        
        // empty array
        let emptyArr:int Rarr = [|  |].asRarr
        let resultEpt = Rarr.fold2 funcInt "x" emptyArr emptyArr
        if resultEpt <> "x" then Assert.Fail()

        // null array
        // Rarr cant be null: let nullArr = null:string Rarr
        let validArray = [| "a"  |].asRarr
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.fold2 funcStr "begin" validArray nullArr |> ignore)  
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.fold2 funcStr "begin" nullArr validArray |> ignore)  
        
        // len1 <> len2
        CheckThrowsArgumentException(fun () -> Rarr.fold2 funcInt "x" [| 1;3;5  |].asRarr  [|2;4;6;8 |].asRarr |> ignore)
                
        ()

    [<Fact>]
    member this.FoldBack() =
        // integer array
        let intArr = [| 1..5  |].asRarr    
        let funcInt x y = x.ToString()+y
        let resultInt = Rarr.foldBack funcInt intArr "x"
        if resultInt <> "12345x" then Assert.Fail()
        
        // string array
        let strArr = [|"A"; "B";  "C" ; "D"  |].asRarr
        let funcStr x y = x+y
            
        let resultStr = Rarr.foldBack funcStr strArr "X" 
        if resultStr <> "ABCDX" then Assert.Fail()
        
        // empty array
        let emptyArr:int Rarr = [|  |].asRarr
        let resultEpt = Rarr.foldBack funcInt emptyArr "x" 
        if resultEpt <> "x" then Assert.Fail()

        // null array
        // Rarr cant be null: let nullArr = null:string Rarr      
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.foldBack funcStr nullArr "begin" |> ignore)  
        
        ()

    [<Fact>]
    member this.FoldBack2() =
        // integer array  
        let funcInt x y z = x.ToString() + y.ToString() + z
        let resultInt = Rarr.foldBack2 funcInt  [| 1;3;5  |].asRarr  [|2;4;6 |].asRarr "x"
        if resultInt <> "123456x" then Assert.Fail()
        
        // string array
        let funcStr x y z= x + y + z        
        let resultStr = Rarr.foldBack2 funcStr [|"A"; "B";  "C" ; "D"  |].asRarr [|"H"; "I";  "J" ; "K"  |].asRarr "X"
        if resultStr <> "AHBICJDKX" then Assert.Fail()
        
        // empty array
        let emptyArr:int Rarr = [|  |].asRarr
        let resultEpt = Rarr.foldBack2 funcInt emptyArr emptyArr "x"
        if resultEpt <> "x" then Assert.Fail()

        // null array
        // Rarr cant be null: let nullArr = null : string Rarr 
        let validArray = [| "a"  |].asRarr 
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.foldBack2 funcStr nullArr validArray "begin" |> ignore)  
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.foldBack2 funcStr validArray nullArr "begin" |> ignore)  
        
        // len1 <> len2
        CheckThrowsArgumentException(fun () -> Rarr.foldBack2 funcInt [|1..10 |].asRarr [|2..20 |].asRarr "x" |> ignore)
        
        ()

    [<Fact>]
    member this.ForAll() =
        // integer array
        let resultInt = Rarr.forall (fun x -> x > 2) [| 3..2..10  |].asRarr
        if resultInt <> true then Assert.Fail()
        
        // string array
        let resultStr = Rarr.forall (fun (x:string) -> x.Contains("a")) [|"Lists"; "are";  "commonly" ; "list"  |].asRarr
        if resultStr <> false then Assert.Fail()
        
        // empty array 
        let resultEpt = Rarr.forall (fun (x:string) -> x.Contains("a")) [| |].asRarr 
        if resultEpt <> true then Assert.Fail()

        // null array
        // Rarr cant be null: let nullArr = null:string Rarr 
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.forall (fun x -> true) nullArr |> ignore)  
        
        ()
        
    [<Fact>]
    member this.ForAll2() =
        // integer array
        let resultInt = Rarr.forall2 (fun x y -> x < y) [| 1..10  |].asRarr [|2..2..20 |].asRarr
        if resultInt <> true then Assert.Fail()
        
        // string array
        let resultStr = Rarr.forall2 (fun (x:string) (y:string) -> x.Length < y.Length) [|"Lists"; "are";  "commonly" ; "list"  |].asRarr [|"Listslong"; "arelong";  "commonlylong" ; "listlong"  |].asRarr
        if resultStr <> true then Assert.Fail()
        
        // empty array 
        let resultEpt = Rarr.forall2 (fun x y -> x>y) [| |].asRarr [| |].asRarr
        if resultEpt <> true then Assert.Fail()

        // null array
        // Rarr cant be null: let nullArr = null:string Rarr
        let validArray = [| "a"  |].asRarr 
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.forall2 (fun x y-> true) nullArr validArray |> ignore)  
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.forall2 (fun x y-> true) validArray nullArr |> ignore)  
        
        // len1 <> len2
        CheckThrowsArgumentException(fun () -> Rarr.forall2 (fun x y -> x < y) [|1..10 |].asRarr [|2..20 |].asRarr |> ignore)
        
        ()
        
    [<Fact>]
    member this.Get() =
        // integer array
        let intArr = [| 3;4;7;8;10  |].asRarr    
        let resultInt = Rarr.get intArr 3
        if resultInt <> 8 then Assert.Fail()
        
        // string array
        let strArr = [|"Lists"; "are";  "commonly" ; "list"  |].asRarr
        
        let resultStr = Rarr.get strArr 2
        if resultStr <> "commonly" then Assert.Fail()
        
        // empty array
        let emptyArr:int Rarr = [|  |].asRarr
        CheckThrowsIndexOutRangException (fun () -> Rarr.get emptyArr -1 |> ignore)

        // null array
        // Rarr cant be null: let nullArr = null:string Rarr 
        // Rarr cant be null: CheckThrowsNullRefException (fun () -> Rarr.get nullArr 0 |> ignore)  
        
        ()

    [<Fact>]
    member this.``exactlyOne should return the element from singleton arrays``() =
        Assert.AreEqual(1, Rarr.exactlyOne [|1 |].asRarr)
        Assert.AreEqual("2", Rarr.exactlyOne [|"2" |].asRarr)
        ()

    [<Fact>]
    member this.``exactlyOne should fail on empty array``() =
        CheckThrowsArgumentException(fun () -> Rarr.exactlyOne [| |].asRarr |> ignore)

    [<Fact>]
    member this.``exactlyOne should fail on null array``() =
        // Rarr cant be null: CheckThrowsArgumentNullException(fun () -> Rarr.exactlyOne null |> ignore)
        ()

    [<Fact>]
    member this.``exactlyOne should fail on arrays with more than one element``() =
        CheckThrowsArgumentException(fun () -> Rarr.exactlyOne [|"1"; "2" |].asRarr |> ignore)

    [<Fact>]
    member this.``tryExactlyOne should return the element from singleton arrays``() =
        Assert.AreEqual(Some 1, Rarr.tryExactlyOne [|1 |].asRarr)
        Assert.AreEqual(Some "2", Rarr.tryExactlyOne [|"2" |].asRarr)
        ()

    [<Fact>]
    member this.``tryExactlyOne should return None on empty array``() =
        Assert.AreEqual(None, Rarr.tryExactlyOne [| |].asRarr)

    [<Fact>]
    member this.``tryExactlyOne should return None for arrays with more than one element``() =
        Assert.AreEqual(None, Rarr.tryExactlyOne [|"1"; "2" |].asRarr)

    [<Fact>]
    member this.``tryExactlyOne should fail on null array``() =
        // Rarr cant be null: CheckThrowsArgumentNullException(fun () -> Rarr.tryExactlyOne null |> ignore)
        ()

    [<Fact>]
    member this.GroupBy() =
        let funcInt x = x%5
             
        let IntArray = [| 0 .. 9  |].asRarr
                    
        let group_byInt = Rarr.groupBy funcInt IntArray
        
        let expectedIntArray = 
            [| for i in 0..4 -> i, [|i; i+5 |].asRarr  |].asRarr

        if group_byInt <*> expectedIntArray then Assert.Fail()
             
        // string array
        let funcStr (x:string) = x.Length
        let strArray = [|"l1ngth7"; "length 8";  "l2ngth7" ; "length  9" |].asRarr
        
        let group_byStr = Rarr.groupBy funcStr strArray
        let expectedStrArray = 
            [|
                7, [|"l1ngth7"; "l2ngth7" |].asRarr
                8, [|"length 8" |].asRarr
                9, [|"length  9" |].asRarr
             |].asRarr
       
        if group_byStr <*> expectedStrArray then Assert.Fail()

        // Empty array
        let emptyArray = [| |].asRarr
        let group_byEmpty = Rarr.groupBy funcInt emptyArray
        let expectedEmptyArray = [| |].asRarr

        if emptyArray <!> expectedEmptyArray then Assert.Fail()

        // Rarr cant be null: CheckThrowsArgumentNullException(fun () -> Rarr.groupBy funcInt (null : int array) |> ignore)
        ()

    member private this.InitTester initInt initString = 
        // integer array
        let resultInt : int Rarr = initInt 3 (fun x -> x + 3) 
        if resultInt <!> [|3;4;5 |].asRarr then Assert.Fail()
        
        // string array
        let funStr (x:int) = 
            match x with
            | 0 -> "Lists"
            | 1 -> "are"
            | 2 -> "commonly"
            | _ -> "end"    
        let resultStr = initString 3 funStr
        if resultStr <!> [|"Lists"; "are";  "commonly"   |].asRarr then Assert.Fail()
        
        // empty array  
        let resultEpt = initInt 0 (fun x -> x+1)
        if resultEpt <!> [|  |].asRarr then Assert.Fail()
        
        ()

    [<Fact>]
    member this.Hd() =
        // integer array
        let resultInt = Rarr.head [|2..2..20 |].asRarr
        Assert.AreEqual(2, resultInt)
        
        // string array
        let resultStr = Rarr.head [|"a";"b";"c";"d" |].asRarr 
        Assert.AreEqual("a", resultStr)
            
        CheckThrowsArgumentException(fun () -> Rarr.head [| |].asRarr |> ignore)        
        // Rarr cant be null: CheckThrowsArgumentNullException(fun () -> Rarr.head null |> ignore)
        ()    

    [<Fact>]
    member this.Init() = 
        this.InitTester Rarr.init Rarr.init
        
    [<Fact>]
    member this.InitWithSideEffects () =
        let stamp = ref 0
        let f i = 
            stamp := !stamp + 1; 
            i 
        Rarr.init 0 f |> ignore
        Assert.AreEqual (0, !stamp)
        
        stamp := 0
        Rarr.init 10 f |> ignore
        Assert.AreEqual (10, !stamp)
        
    [<Fact>]
    member this.``Parallel.Init``() = 
        this.InitTester Rarr.Parallel.init Rarr.Parallel.init

    [<Fact>]
    member this.IsEmpty() =
        // integer array
        let intArr = [| 3;4;7;8;10  |].asRarr    
        let resultInt = Rarr.isEmpty intArr 
        if resultInt <> false then Assert.Fail()
        
        // string array
        let strArr = [|"Lists"; "are";  "commonly" ; "list"  |].asRarr    
        let resultStr = Rarr.isEmpty strArr 
        if resultStr <> false then Assert.Fail()
        
        // empty array    
        let emptyArr:int Rarr = [|  |].asRarr
        let resultEpt = Rarr.isEmpty emptyArr 
        if resultEpt <> true then Assert.Fail()

        // null array
        // Rarr cant be null: let nullArr = null:string Rarr 
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.isEmpty nullArr |> ignore)  
        
        ()

    [<Fact>]
    member this.Iter() =
        // integer array
        let intArr = [| 1..10  |].asRarr  
        let resultInt = ref 0    
        let funInt (x:int) =   
            resultInt := !resultInt + x              
            () 
        Rarr.iter funInt intArr 
        if !resultInt <> 55 then Assert.Fail()    
        
        // string array
        let strArr = [|"Lists"; "are";  "commonly" ; "list"  |].asRarr
        let resultStr = ref ""
        let funStr (x : string) =
            resultStr := (!resultStr) + x   
            ()
        Rarr.iter funStr strArr  
        if !resultStr <> "Listsarecommonlylist" then Assert.Fail()   
        
        // empty array    
        let emptyArr : int Rarr = [|  |].asRarr
        let resultEpt = ref 0
        Rarr.iter funInt emptyArr 
        if !resultEpt <> 0 then Assert.Fail()    

        // null array
        // Rarr cant be null: let nullArr = null : string Rarr  
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.iter funStr nullArr |> ignore)  
        
        ()
       
    [<Fact>]
    member this.Iter2() =
        // integer array
        let resultInt = ref 0    
        let funInt (x:int) (y:int) =   
            resultInt := !resultInt + x + y             
            () 
        Rarr.iter2 funInt [| 1..10  |].asRarr [|2..2..20 |].asRarr 
        if !resultInt <> 165 then Assert.Fail()    
        
        // string array
        let resultStr = ref ""
        let funStr (x:string) (y:string) =
            resultStr := (!resultStr) + x  + y 
            ()
        Rarr.iter2 funStr [|"A"; "B";  "C" ; "D"  |].asRarr [|"a"; "b"; "c"; "d" |].asRarr  
        if !resultStr <> "AaBbCcDd" then Assert.Fail()   
        
        // empty array    
        let emptyArr:int Rarr = [|  |].asRarr
        let resultEpt = ref 0
        Rarr.iter2 funInt emptyArr emptyArr 
        if !resultEpt <> 0 then Assert.Fail()    

        // null array
        // Rarr cant be null: let nullArr = null:string Rarr  
        let validArray = [| "a"  |].asRarr     
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.iter2 funStr nullArr validArray |> ignore)  
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.iter2 funStr validArray nullArr |> ignore)  
        
        // len1 <> len2        
        CheckThrowsArgumentException(fun () -> Rarr.iter2 funInt [| 1..10  |].asRarr [|2..20 |].asRarr)
  
        ()
        
        
    [<Fact>]
    member this.Iteri() =
        // integer array
        let intArr = [| 1..10  |].asRarr  
        let resultInt = ref 0    
        let funInt (x:int) y =   
            resultInt := !resultInt + x + y             
            () 
        Rarr.iteri funInt intArr 
        if !resultInt <> 100 then Assert.Fail()    
        
        // string array
        let strArr = [|"Lists"; "are";  "commonly" ; "list"  |].asRarr
        let resultStr = ref 0
        let funStr (x:int) (y:string) =
            resultStr := (!resultStr) + x + y.Length
            ()
        Rarr.iteri funStr strArr  
        if !resultStr <> 26 then Assert.Fail()   
        
        // empty array    
        let emptyArr:int Rarr = [|  |].asRarr
        let resultEpt = ref 0
        Rarr.iteri funInt emptyArr 
        if !resultEpt <> 0 then Assert.Fail()    

        // null array
        // Rarr cant be null: let nullArr = null:string Rarr 
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.iteri funStr nullArr |> ignore)  
        
        ()
        
    [<Fact>]
    member this.Iteri2() =
        // integer array
        let resultInt = ref 0    
        let funInt (x:int) (y:int) (z:int) =   
            resultInt := !resultInt + x + y + z            
            () 
        Rarr.iteri2 funInt [| 1..10  |].asRarr [|2..2..20 |].asRarr 
        if !resultInt <> 210 then Assert.Fail()    
        
        // string array
        let resultStr = ref ""
        let funStr (x:int) (y:string) (z:string) =
            resultStr := (!resultStr) + x.ToString()  + y + z
            ()
        Rarr.iteri2 funStr [|"A"; "B";  "C" ; "D"  |].asRarr [|"a"; "b"; "c"; "d" |].asRarr  
        if !resultStr <> "0Aa1Bb2Cc3Dd" then Assert.Fail()   
        
        // empty array    
        let emptyArr:int Rarr = [|  |].asRarr
        let resultEpt = ref 0
        Rarr.iteri2 funInt emptyArr emptyArr 
        if !resultEpt <> 0 then Assert.Fail()    

        // null array
        // Rarr cant be null: let nullArr = null:string Rarr
        let validArray = [| "a"  |].asRarr 
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.iteri2 funStr nullArr validArray |> ignore)  
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.iteri2 funStr validArray nullArr |> ignore)  
        
        // len1 <> len2
        CheckThrowsArgumentException(fun () -> Rarr.iteri2 funInt [| 1..10  |].asRarr [|2..20 |].asRarr  |> ignore)
        
        ()                

    [<Fact>]
    member this.``pairwise should return pairs of the input array``() =
        Assert.AreEqual([| |].asRarr, Rarr.pairwise [| |].asRarr)
        Assert.AreEqual([| |].asRarr, Rarr.pairwise [|1 |].asRarr)
        Assert.AreEqual([|1,2 |].asRarr, Rarr.pairwise [|1;2 |].asRarr)
        Assert.AreEqual([|1,2; 2,3 |].asRarr, Rarr.pairwise [|1;2;3 |].asRarr)
        Assert.AreEqual([|"H","E"; "E","L"; "L","L"; "L","O" |].asRarr, Rarr.pairwise [|"H";"E";"L";"L";"O" |].asRarr)

    [<Fact>]
    member this.``pairwise should not work on null``() =
        // Rarr cant be null: CheckThrowsArgumentNullException(fun () -> Rarr.pairwise null |> ignore)
        ()

    member private this.MapTester mapInt (mapString : (string -> int) -> Rarr<string> -> Rarr<int>) =
        // empty array 
        let f x = x + 1
        let result = mapInt f [|  |].asRarr
        if result <!> [|  |].asRarr then Assert.Fail ()
        
        // int array
        let result = mapInt f [| 1..100  |].asRarr
        if result <!> [| 2..101  |].asRarr then Assert.Fail ()
        
        // string array
        let result = [| "a"; "aa"; "aaa"  |].asRarr |> mapString (fun s -> s.Length) 
        if result <!> [| 1..3  |].asRarr then Assert.Fail ()
        
        // null array
        // Rarr cant be null: let nullArg : int  Rarr = null
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> mapInt f nullArg |> ignore)
        
        ()
        
    [<Fact>]  
    member this.Map () =
        this.MapTester Rarr.map Rarr.map
        
    [<Fact>]
    member this.MapWithSideEffects () =
        let stamp = ref 0
        let f x = stamp := !stamp + 1; x + 1
        
        Rarr.map f [|  |].asRarr |> ignore
        Assert.AreEqual(0,!stamp)
        
        stamp := 0
        Rarr.map f [| 1..100  |].asRarr |> ignore
        Assert.AreEqual(100,!stamp)
        
    [<Fact>]
    member this.``Parallel.Map`` () =
        this.MapTester Rarr.Parallel.map Rarr.Parallel.map

    member private this.MapiTester mapiInt mapiString =
        // empty array 
        let f i x = (i, x + 1)
        let result = mapiInt f [|  |].asRarr
        if result <!> [|  |].asRarr then Assert.Fail ()
        
        // int array
        let result : Rarr<int*int> = mapiInt f [| 1..2  |].asRarr
        if result <!> [| (0,2); (1,3)  |].asRarr then Assert.Fail ()
        
        // string array
        let result : Rarr<int*int> = [| "a"; "aa"; "aaa"  |].asRarr |> mapiString (fun i (s:string) -> i, s.Length) 
        if result <!> [| (0,1); (1,2); (2,3)  |].asRarr then Assert.Fail ()
        
        // null array
        // Rarr cant be null: let nullArg : int  Rarr = null
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> mapiInt f nullArg |> ignore)        
        ()

    [<Fact>]
    member this.Mapi () = this.MapiTester Rarr.mapi Rarr.mapi
        

    [<Fact>]
    member this.MapiWithSideEffects () =
        let stamp = ref 0
        let f i x = stamp := !stamp + 1; (i, x + 1)
       
        Rarr.mapi f [|  |].asRarr |> ignore
        Assert.AreEqual(0,!stamp)
       
        stamp := 0
        Rarr.mapi f [| 1..100  |].asRarr |> ignore
        Assert.AreEqual(100,!stamp)
        ()
        
    [<Fact>]
    member this.``Parallel.Mapi`` () =
        this.MapiTester Rarr.Parallel.mapi Rarr.Parallel.mapi
        ()
        
    [<Fact>]
    member this.``Parallel.Iter``() =
        // integer array
        let intArr = [| 1..10  |].asRarr  
        let resultInt = ref 0    
        let funInt (x:int) =   
            lock resultInt (fun () -> resultInt := !resultInt + x)
            () 
        Rarr.Parallel.iter funInt intArr 
        if !resultInt <> 55 then Assert.Fail()    
        
        // string array
        let strArr = [|"Lists"; "are";  "commonly" ; "list"  |].asRarr
        let resultStr = ref 0
        let funStr (x : string) =
            lock resultStr (fun () -> resultStr := (!resultStr) + x.Length)
            ()
        Rarr.Parallel.iter funStr strArr  
        if !resultStr <> 20 then Assert.Fail()   
        
        // empty array    
        let emptyArr : int Rarr = [|  |].asRarr
        let resultEpt = ref 0
        Rarr.Parallel.iter funInt emptyArr 
        if !resultEpt <> 0 then Assert.Fail()    

        // null array
        // Rarr cant be null: let nullArr = null : string Rarr  
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.Parallel.iter funStr nullArr |> ignore)  
        
        ()
        
    [<Fact>]
    member this.``Parallel.Iteri``() =   
        // integer array
        let intArr = [| 1..10  |].asRarr 
                 
        let resultInt = ref 0    
        let funInt (x:int) y =   
            lock resultInt (fun () -> resultInt := !resultInt + x + y)
            () 
        Rarr.Parallel.iteri funInt intArr 
        if !resultInt <> 100 then Assert.Fail()    
        
        // string array
        let strArr = [|"Lists"; "are";  "commonly" ; "list"  |].asRarr
        let resultStr = ref 0
        let funStr (x:int) (y:string) =
            lock resultStr (fun () -> resultStr := (!resultStr) + x + y.Length)
            ()
        Rarr.Parallel.iteri funStr strArr  
        if !resultStr <> 26 then Assert.Fail()   
        
        // empty array    
        let emptyArr:int Rarr = [|  |].asRarr
        let resultEpt = ref 0
        Rarr.Parallel.iteri funInt emptyArr 
        if !resultEpt <> 0 then Assert.Fail()    

        // null array
        // Rarr cant be null: let nullArr = null:string Rarr 
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.Parallel.iteri funStr nullArr |> ignore)  
        
        ()
    
    member private this.PartitionTester partInt partString =
        // int array
        let intSrc:int  Rarr = [| 1..100  |].asRarr    
        let funcInt x = if (x%2=1) then true else false
        let intPartitioned : int Rarr * int Rarr = partInt funcInt intSrc
        if ([|1..2..100 |].asRarr,[|2..2..100 |].asRarr) <!!> intPartitioned then Assert.Fail ()
        
        let allLeft = partInt (fun _ -> true) intSrc
        if (intSrc, [| |].asRarr) <!!> allLeft then Assert.Fail()
        let allRight = partInt (fun _ -> false) intSrc
        if ([| |].asRarr, intSrc) <!!> allRight then Assert.Fail()

        
        // string array
        let stringSrc: string  Rarr = "List 1 list 2 3 4 5".Split([|' ' |], System.StringSplitOptions.RemoveEmptyEntries).asRarr
        let funcString x = match x with
                           | "list"-> true
                           | "List" -> true
                           | _ -> false
        let strPartitioned : string Rarr * string Rarr  = partString funcString stringSrc   
        if strPartitioned <!!> ([|"List";"list" |].asRarr, [| "1";"2"; "3"; "4"; "5" |].asRarr) then Assert.Fail ()
        
        // empty array
        let emptySrc :int Rarr = [|  |].asRarr
        let emptyPartitioned = partInt funcInt emptySrc
        if emptyPartitioned <!!> ([|  |].asRarr, [|  |].asRarr) then Assert.Fail()
        
        // null array
        // Rarr cant be null: let nullArr = null:string Rarr 
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> partString funcString nullArr |> ignore)
        
        
    [<Fact>]
    member this.Partition () =
        this.PartitionTester Rarr.partition Rarr.partition    

    [<Fact>]
    member this.Singleton() =
        Assert.AreEqual([|null |].asRarr, Rarr.singleton null)
        Assert.AreEqual([|"1" |].asRarr, Rarr.singleton "1")
        Assert.AreEqual([| []  |].asRarr, Rarr.singleton [])
        Assert.True([| [|  |].asRarr  |].asRarr =+= Rarr.singleton [|  |].asRarr)

    [<Fact>]
    member this.``Parallel.Partition`` () =
        this.PartitionTester Rarr.Parallel.partition Rarr.Parallel.partition    

    [<Fact>]
    member this.Contains() =
        // integer array
        let intArr = [| 2;4;6;8  |].asRarr
        let resultInt = Rarr.contains 6 intArr
        Assert.True(resultInt)

        // string array
        let strArr = [|"Lists"; "are"; "commonly" |].asRarr
        let resultStr = Rarr.contains "not" strArr
        Assert.False(resultStr)

        // empty array
        let emptyArr:int Rarr = [|  |].asRarr
        let resultEpt = Rarr.contains 4 emptyArr
        Assert.False(resultEpt)

        // null array
        // Rarr cant be null: let nullArr = null:string Rarr
        // Rarr cant be null: CheckThrowsArgumentNullException (fun () -> Rarr.contains "empty" nullArr |> ignore)

    [<Fact>]
    member this.``Slicing with first index reverse behaves as expected``()  = 
        let arr = [| 1;2;3;4;5  |].asRarr

        Assert.AreEqual(arr.[^3..], arr.[1..])

    
    [<Fact>]
    member this.``Slicing with second index reverse behaves as expected``()  = 
        let arr = [| 1;2;3;4;5  |].asRarr

        Assert.AreEqual(arr.[..^1], arr.[..3])

    
    [<Fact>]
    member this.``Slicing with both index reverse behaves as expected``()  = 
        let arr = [| 1;2;3;4;5  |].asRarr

        Assert.AreEqual(arr.[^3..^1], arr.[1..3])

    [<Fact>]
    member this.``Slicing with first index reverse and second index non reverse behaves as expected``()=
        let arr = [|1;2;3;4;5 |].asRarr

        Assert.AreEqual(arr.[^3..4], arr.[1..4])

    [<Fact>]
    member this.``Slicing with first index non reverse and second index reverse behaves as expected``()=
        let arr = [|1;2;3;4;5 |].asRarr

        Assert.AreEqual(arr.[3..^0], arr.[3..4])

    [<Fact>]
    member this.``Set slice with first index reverse behaves as expected``()  = 
        let arr1 = [| 1;2;3;4;5  |].asRarr
        let arr2 = [| 1;2;3;4;5  |].asRarr

        arr1.[^3..] <- [| 9;8;7;6  |].asRarr
        arr2.[1..] <- [| 9;8;7;6  |].asRarr

        Assert.AreEqual(arr1, arr2)

    [<Fact>]
    member this.``Set slice with second index reverse behaves as expected``()  = 
        let arr1 = [| 1;2;3;4;5  |].asRarr
        let arr2 = [| 1;2;3;4;5  |].asRarr

        arr1.[..^1] <- [| 9;8;7;6  |].asRarr
        arr2.[..3] <- [| 9;8;7;6  |].asRarr

        Assert.AreEqual(arr1, arr2)

    [<Fact>]
    member this.``Set slice with both index reverse behaves as expected``()  = 
        let arr1 = [| 1;2;3;4;5  |].asRarr
        let arr2 = [| 1;2;3;4;5  |].asRarr

        arr1.[^3..^1] <- [| 8;7;6  |].asRarr
        arr2.[1..3] <- [| 8;7;6  |].asRarr

        Assert.AreEqual(arr1, arr2)

    [<Fact>]
    member this.``Get item with reverse index behaves as expected``() = 
        let arr = [|1;2;3;4;5 |].asRarr
        Assert.AreEqual(arr.[^1], 4)

    [<Fact>]
    member this.``Set item with reverse index behaves as expected``() = 
        let arr = [|1;2;3;4;5 |].asRarr

        arr.[^0] <- 9
        Assert.AreEqual(arr.[4], 9)

    [<Fact>] 
    member this.SlicingUnboundedEnd() = 
        let arr = [|1;2;3;4;5;6 |].asRarr

        CheckThrowsArgumentException   (fun () -> arr.[-1..]|> ignore )
        Assert.AreEqual(arr.[0..], arr)
        Assert.AreEqual(arr.[1..], [2;3;4;5;6])
        Assert.AreEqual(arr.[2..], [3;4;5;6])
        Assert.AreEqual(arr.[5..], [6])
        
        
        //Assert.AreEqual(arr.[6..], ([| |].asRarr: int Rarr))
        //Assert.AreEqual(arr.[7..], ([| |].asRarr: int Rarr))
        CheckThrowsArgumentException   (fun () -> arr.[..7]  |> ignore )

    
    [<Fact>] 
    member this.SlicingUnboundedStart() = 
        let arr = [|1;2;3;4;5;6 |].asRarr

        CheckThrowsArgumentException   (fun () -> arr.[..(-1)]|> ignore )
        Assert.AreEqual(arr.[..1], [|1;2 |].asRarr)
        Assert.AreEqual(arr.[..2], [|1;2;3 |].asRarr)
        Assert.AreEqual(arr.[..3], [|1;2;3;4 |].asRarr)
        Assert.AreEqual(arr.[..4], [|1;2;3;4;5 |].asRarr)
        Assert.AreEqual(arr.[..5], [|1;2;3;4;5;6 |].asRarr)


        //Assert.AreEqual(arr.[..6], [|1;2;3;4;5;6 |].asRarr)
        //Assert.AreEqual(arr.[..7], [|1;2;3;4;5;6 |].asRarr)
        CheckThrowsArgumentException   (fun () -> arr.[..6]  |> ignore )
        CheckThrowsArgumentException   (fun () -> arr.[..7]  |> ignore )
        


    [<Fact>]
    member this.SlicingBoundedStartEnd() =
        let arr = [|1;2;3;4;5;6 |].asRarr

        Assert.AreEqual(arr.[*], arr)

        Assert.AreEqual(arr.[0..0], [|1 |].asRarr)
        Assert.AreEqual(arr.[0..1], [|1;2 |].asRarr)
        Assert.AreEqual(arr.[0..2], [|1;2;3 |].asRarr)
        Assert.AreEqual(arr.[0..3], [|1;2;3;4 |].asRarr)
        Assert.AreEqual(arr.[0..4], [|1;2;3;4;5 |].asRarr)
        Assert.AreEqual(arr.[0..5], [|1;2;3;4;5;6 |].asRarr)

        Assert.AreEqual(arr.[1..1], [|2 |].asRarr)
        Assert.AreEqual(arr.[1..2], [|2;3 |].asRarr)
        Assert.AreEqual(arr.[1..3], [|2;3;4 |].asRarr)
        Assert.AreEqual(arr.[1..4], [|2;3;4;5 |].asRarr)
        Assert.AreEqual(arr.[1..5], [|2;3;4;5;6 |].asRarr)

        Assert.AreEqual(arr.[0..1], [|1;2 |].asRarr)
        Assert.AreEqual(arr.[1..1], [|2 |].asRarr)
       
        //Assert.AreEqual(arr.[4..1], ([| |].asRarr: int Rarr))
        //Assert.AreEqual(arr.[3..1], ([| |].asRarr: int Rarr))
        //Assert.AreEqual(arr.[2..1], ([| |].asRarr: int Rarr))
        CheckThrowsArgumentException   (fun () -> arr.[2..1]  |> ignore )
        CheckThrowsArgumentException   (fun () -> arr.[3..1]  |> ignore )
        CheckThrowsArgumentException   (fun () -> arr.[4..3]  |> ignore )

    [<Fact>]
    member this.SlicingEmptyArray() = 

        let empty : obj Rarr = Rarr.empty
        Assert.AreEqual(empty.[*], ([| |].asRarr: obj Rarr))
        CheckThrowsArgumentException   (fun () -> empty.[5..3] |> ignore )
        CheckThrowsArgumentException   (fun () -> empty.[0..]  |> ignore )
        CheckThrowsArgumentException   (fun () -> empty.[0..0] |> ignore )
        CheckThrowsArgumentException   (fun () -> empty.[0..1] |> ignore )
        CheckThrowsArgumentException   (fun () -> empty.[3..5] |> ignore )


    [<Fact>]
    member this.SlicingOutOfBounds() = 
        let arr = [|1;2;3;4;5;6 |].asRarr
       
        CheckThrowsArgumentException   (fun () -> arr.[..6] |> ignore )
        CheckThrowsArgumentException   (fun () -> arr.[6..] |> ignore )

        CheckThrowsArgumentException   (fun () -> arr.[0..(-1)] |> ignore )
        CheckThrowsArgumentException   (fun () -> arr.[1..(-1)] |> ignore )
        CheckThrowsArgumentException   (fun () -> arr.[1..0] |> ignore )
        CheckThrowsArgumentException   (fun () -> arr.[0..6] |> ignore )
        CheckThrowsArgumentException   (fun () -> arr.[1..6] |> ignore )

        CheckThrowsArgumentException   (fun () -> arr.[-1..1]    |> ignore )
        CheckThrowsArgumentException   (fun () -> arr.[-3..(-4)] |> ignore )
        CheckThrowsArgumentException   (fun () -> arr.[-4..(-3)] |> ignore )


        ()