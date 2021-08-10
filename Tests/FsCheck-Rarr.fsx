#r @"C:\GitHub\FsEx\bin\Debug\netstandard2.0\FsEx.dll"
#r "nuget: FsCheck"

open System
open FsEx
open FsCheck


// these test exist as a script because they fail if part of XUnit in TestRarrProperties.fs 
// Could not load file or assembly 'FSharp.Core, Version=4.4.1.0,


let isStable sorted =  
    sorted 
    |> Seq.pairwise 
    |> Seq.forall (fun ((ia, a),(ib, b)) ->  
        if a = b then  
            ia < ib 
        else  
            true)
 
let distinctByStable<'a when 'a : comparison> (xs : 'a []) =
    let indexed = xs |> Seq.indexed |> Rarr.ofSeq
    let sorted = indexed |> Rarr.distinctBy snd
    isStable sorted 

Check.QuickThrowOnFailure distinctByStable<int>
Check.QuickThrowOnFailure distinctByStable<string>


type Pass = Yes | No

let ex f = 
    try f(); Yes with 
    | :? ArgumentException -> No
    |   e -> raise e
 
let blitWorksLikeCopy<'T when 'T : comparison> (source: 'T [], sourceIndex:int, target:'T[], targetIndex:int, count:int) =
    //Printfn.blue "source: %A" source
    let target1 = Rarr<'T>(target) 
    let src1    = Rarr<'T>(source) 
    let p1 = ex (fun ()  -> Array.Copy(source, sourceIndex, target, targetIndex, count)) 
    let p2 = ex (fun ()  -> Rarr.blit src1 sourceIndex target1 targetIndex count )  
    match p1, p2 with 
    |No, No -> true // both had an exception 
    |Yes, Yes -> 
        let t1 = target1   |> Rarr.toArray 
        //Printfn.red "t1=target : %A=%A" t1 target 
        t1 = target
    | _ -> false // only one had an exception 
    
//Check.Verbose blitWorksLikeCopy<int>
Check.QuickThrowOnFailure blitWorksLikeCopy<int>
Check.QuickThrowOnFailure blitWorksLikeCopy<string>
