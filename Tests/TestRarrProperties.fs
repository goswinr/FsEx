// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.
module FSharp.Core.UnitTests.Collections.ArrayProperties

open System
open System.Collections.Generic

open Xunit
open FsCheck
open Utils
open FsEx


// these test fail with
// System.IO.FileNotFoundException : Could not load file or assembly 'FSharp.Core, Version=4.4.1.0,
//but they pass in then script file FsCheck-Rarr.fsx



(*

let isStable sorted = sorted |> Seq.pairwise |> Seq.forall (fun ((ia, a),(ib, b)) -> if a = b then ia < ib else true)

let distinctByStable<'a when 'a : comparison> (xs : 'a []) = 
    let indexed = xs |> Seq.indexed |> Rarr.ofSeq
    let sorted = indexed |> Rarr.distinctBy snd
    isStable sorted

[<Fact>]
let ``Rarr.distinctBy is stable`` () = 

    Check.QuickThrowOnFailure distinctByStable<int>
    Check.QuickThrowOnFailure distinctByStable<string>

let blitWorksLikeCopy<'a when 'a : comparison> (source : 'a Rarr, sourceIndex, target : 'a Rarr, targetIndex, count) = 
    let target1 = Rarr.copy target
    let target2 = Rarr.copy target
    let a = runAndCheckIfAnyError (fun () -> Rarr.blit source sourceIndex target1 targetIndex count)
    let b = runAndCheckIfAnyError (fun () -> Array.Copy(Array.ofSeq source, sourceIndex, Array.ofSeq target2, targetIndex, count))
    a = b && Rarr.equals target1  target2

[<Fact>]
let ``Rarr.blit works like Rarr.Copy`` () = 
    Check.QuickThrowOnFailure blitWorksLikeCopy<int>
    Check.QuickThrowOnFailure blitWorksLikeCopy<string>

*)
