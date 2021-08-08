﻿namespace FsEx

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open FsEx.ExtensionsSeq

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] //need this so doesn't hide Seq class in C# assemblies 
module Seq =   
    

    /// Counts for how many items of the Seq the predicate returns true.    
    /// same as Seq.filter and then Seq.length 
    let inline countIf (predicate : 'T -> bool) (xs : seq<'T>) : int = //countBy is something else !!
        let mutable k = 0
        for x in xs do
            if predicate x then 
                k <- k + 1
        k
        

    /// Applies a function to each element in Seq
    /// If resulting element meets the resultPredicate it is returned , otherwise the original input is returned.   
    let mapIfResult (resultPredicate:seq<'T> -> bool) (transform:seq<'T> -> seq<'T>)  (xs: seq<'T>) : seq<'T> =
        let r = transform xs
        if resultPredicate r then r
        else xs

    /// Applies a function to each element in Seq if it meets the inputPredicate, otherwise just returns input element unchanged.
    /// If resulting element meets the resultPredicate it is returned , otherwise the original input element is returned.   
    let mapIfInputAndResult (inputPredicate:seq<'T> -> bool) (resultPredicate:seq<'T> -> bool) (transform:seq<'T> -> seq<'T>)  (xs: seq<'T>) : seq<'T> =
        if inputPredicate xs then
            let r = transform xs
            if resultPredicate r then r
            else xs
        else
            xs

    /// Returns true if the given Rarr has count items.
    let hasItems count (xs : seq<'T>) : bool =
        match xs with
        | :? ('T[]) as a ->     a.Length = count
        | :? ('T Rarr) as a  -> a.Count  = count       
        | :? ('T IList) as a -> a.Count  = count
        | _ ->
            let mutable k = 0
            use e = xs.GetEnumerator()
            while e.MoveNext() && k <= count do  k <- k+1
            k = count

    /// Returns true if the given Rarr has equal or more than count items.
    let hasMinimumItems count (xs : seq<'T>) : bool =
        match xs with
        | :? ('T[]) as a ->     a.Length >= count
        | :? ('T Rarr) as a  -> a.Count  >= count       
        | :? ('T IList) as a -> a.Count  >= count
        | _ ->
            let mutable k = 0
            use e = xs.GetEnumerator()
            while e.MoveNext() && k <= count do  k <- k+1
            k >= count

    /// Returns true if the given Rarr has equal or less than count items.
    let hasMaximumItems count (xs : seq<'T>) : bool =
        match xs with
        | :? ('T[]) as a ->     a.Length <= count
        | :? ('T Rarr) as a  -> a.Count  <= count       
        | :? ('T IList) as a -> a.Count  <= count
        | _ ->
            let mutable k = 0
            use e = xs.GetEnumerator()
            while e.MoveNext() && k <= count do  k <- k+1
            k <= count


    /// Allows for negative indices too, -1 is the last element.
    /// The resulting seq includes the item at slice-ending-index. like F# range expressions include the last integer e.g.: 0..5
    let slice startIdx endIdx (xs:seq<_>) =  
        xs.Slice(startIdx,endIdx)    

    /// Gets an item by index position in the Seq
    /// Allows for negtive index too (like Python)  
    let getNeg index  (xs:seq<_>) = 
        xs.GetNeg(index)

    /// Considers sequence cirular and move elements up or down
    /// e.g.: rotate +1 [ a, b, c, d] = [ d, a, b, c]
    /// e.g.: rotate -1 [ a, b, c, d] = [ b, c, d, a]
    let rotate r (xs:seq<_>) = 
        xs |> Rarr.ofSeq |> Rarr.rotate r
        
    /// Yields the Seq without the last element
    let skipLast (xs:seq<_>) =  seq{ 
        use e = xs.GetEnumerator() 
        if e.MoveNext() then
            let mutable prev =  e.Current
            while e.MoveNext() do
                yield  prev
                prev <- e.Current
        else
            IndexOutOfRangeException.Raise "skipLast: Empty Input Sequence"}
    
    /// splits seq in two, like Seq.filter but returning both
    /// the first Rarr has all elements where the filter function returned 'true'
    let partition predicate (xs:seq<_>) =  
        let t=Rarr()
        let f=Rarr()
        for x in xs do
            if predicate x then t.Add(x)
            else                f.Add(x)
        t,f


                
    //---------------------prev-this-next ------------------------------
    //---------------------prev-this-next ------------------------------
    //---------------------prev-this-next ------------------------------

    /// Yields a looped Seq from (first, second)  upto (last, first)
    /// The length of the resulting seq is the same as the input seq.
    /// Use Seq.windowed2 if you don't want a looped sequence.
    let thisNext (xs:seq<'T>) : seq<'T*'T> = 
        seq{use e = xs.GetEnumerator()
            if e.MoveNext() then
                let mutable prev = e.Current
                let first = e.Current
                if e.MoveNext() then
                        yield prev, e.Current 
                        prev <- e.Current 
                        while e.MoveNext() do 
                            yield  prev, e.Current 
                            prev <- e.Current
                        yield prev, first
                else
                    IndexOutOfRangeException.Raise "Seq.thisNext: Input Sequence only had one element %A" xs.ToNiceString
            else
                IndexOutOfRangeException.Raise "Seq.thisNext: Empty Input Sequence"}
    
    /// Yields a Seq from (first, second)  upto (second last, last)
    /// The length of the resulting seq is one shorter than input seq.
    /// Use Seq.thisNext if you want a looped sequence till (last, first)
    let windowed2 (xs:seq<'T>): seq<'T*'T> =  
        seq{use e = xs.GetEnumerator()
            if e.MoveNext() then
                let mutable prev = e.Current                
                if e.MoveNext() then
                    yield prev, e.Current 
                    prev <- e.Current 
                    while e.MoveNext() do 
                        yield  prev, e.Current 
                        prev <- e.Current                        
                else
                    IndexOutOfRangeException.Raise "Seq.windowed2: Input Sequence only had one element %A" xs.ToNiceString
            else
                IndexOutOfRangeException.Raise "Seq.windowed2: Empty Input Sequence"}  


    /// Yields looped Seq from (0, first, second)  upto (lastIndex, last, first)
    /// The length of the resulting seq is the same as the input seq.
    /// Use Seq.windowed2i if you don't want a looped sequence.
    let iThisNext (xs:seq<'T>): seq<int *'T*'T>  =  
        seq{use e = xs.GetEnumerator()
            let mutable kk =  0  
            if e.MoveNext() then
                let mutable prev = e.Current
                let first = e.Current
                if e.MoveNext() then
                        yield kk, prev, e.Current 
                        prev <- e.Current 
                        while e.MoveNext() do
                            kk <- kk + 1
                            yield  kk, prev, e.Current 
                            prev <- e.Current
                        kk <- kk + 1 
                        yield kk, prev, first
                else
                    IndexOutOfRangeException.Raise "thisNextLooped: Input Sequence only had one element"
            else
                IndexOutOfRangeException.Raise "thisNextLooped: Empty Input Sequence"}

    /// Yields a Seq from (0,first, second) upto (secondLastIndex, second last, last)
    /// The length of the resulting seq is one shorter than input seq.
    /// Use Seq.iTthisNext if you want a looped sequence till (lastIndex,last, first)
    let windowed2i (xs:seq<'T>): seq<int *'T*'T> =  
        seq{use e = xs.GetEnumerator()
            let mutable kk =  0  
            if e.MoveNext() then
                let mutable prev = e.Current
                if e.MoveNext() then
                    yield kk, prev, e.Current 
                    prev <- e.Current 
                    while e.MoveNext() do
                        kk <- kk + 1
                        yield  kk, prev, e.Current 
                        prev <- e.Current  
                else
                    IndexOutOfRangeException.Raise "Seq.windowed2i: Input Sequence only had one element %A" xs.ToNiceString
            else
                IndexOutOfRangeException.Raise "Seq.windowed2i: Empty Input Sequence"}  


    /// Yields a looped Seq from (last, first, second)  upto (second-last, last, first)
    /// The length of the resulting seq is the same as the input seq.
    /// Use Seq.windowed3 if you don't want a looped sequence.
    let prevThisNext (xs:seq<'T>) : seq<'T *'T*'T> =  seq { 
        use e = xs.GetEnumerator()
        if e.MoveNext() then
            let mutable prev =  e.Current
            let first = e.Current
            if e.MoveNext() then
                let mutable this = e.Current
                if e.MoveNext() then                    
                    yield indexFromBack 0 xs ,prev, this //yield Seq.last xs ,prev, this
                    yield prev, this, e.Current
                    prev <- this  
                    this <- e.Current
                    while e.MoveNext() do 
                        yield  prev, this, e.Current
                        prev <- this 
                        this <- e.Current                            
                    yield prev, this, first
                else                     
                    IndexOutOfRangeException.Raise "Seq.prevThisNextLooped: Input Sequence only had two elements: %s" xs.ToNiceString
            else
                IndexOutOfRangeException.Raise "Seq.prevThisNextLooped: Input Sequence only had one element: %s" xs.ToNiceString
        else
            IndexOutOfRangeException.Raise "Seq.prevThisNextLooped: Empty Input Sequence %A" xs} 


    /// Yields a Seq from (first, second, third)  upto (third-last, second-last, last)
    /// The length of the resulting seq is two shorter than the input seq.
    /// Use Seq.prevThisNext if you want a looped sequence upto (second-last, last, first)
    let windowed3 (xs:seq<'T>) : seq<'T *'T*'T> =  seq { 
        use e = xs.GetEnumerator()
        if e.MoveNext() then
            let mutable prev =  e.Current            
            if e.MoveNext() then
                let mutable this = e.Current
                if e.MoveNext() then
                    yield prev, this, e.Current
                    prev <- this  
                    this <- e.Current
                    while e.MoveNext() do 
                        yield  prev, this, e.Current
                        prev <- this 
                        this <- e.Current
                else                     
                    IndexOutOfRangeException.Raise "Seq.windowed3: Input Sequence only had two elements: %s" xs.ToNiceString
            else
                IndexOutOfRangeException.Raise "Seq.windowed3: Input Sequence only had one element: %s" xs.ToNiceString
        else
            IndexOutOfRangeException.Raise "Seq.windowed3: Empty Input Sequence %A" xs} 


    /// Yields looped Seq from (0, last, first, second)  upto (lastIndex, second-last, last, first)
    /// The length of the resulting seq is the same as the input seq.
    /// Use Seq.windowed3i if you don't want a looped sequence.
    let iPrevThisNext (xs:seq<'T>) : seq<int*'T *'T*'T> = seq { 
        use e = xs.GetEnumerator()        
        if e.MoveNext() then
            let mutable prev =  e.Current
            let first = e.Current
            if e.MoveNext() then
                let mutable this = e.Current
                if e.MoveNext() then
                    yield  0, indexFromBack 0 xs ,prev, this 
                    yield  1, prev, this, e.Current
                    let mutable kk = 2
                    prev <- this  
                    this <- e.Current
                    while e.MoveNext() do 
                        yield kk, prev, this, e.Current
                        kk <- kk + 1
                        prev <- this 
                        this <- e.Current                            
                    yield kk, prev, this, first
                else                     
                    IndexOutOfRangeException.Raise "prevThisNextLooped: Input Sequence %s only had two elements" xs.ToNiceString
            else
                IndexOutOfRangeException.Raise "prevThisNextLooped: Input Sequence %s only had one element" xs.ToNiceString
        else
            IndexOutOfRangeException.Raise "prevThisNextLooped: Empty Input Sequence %A" xs} 

    /// Yields a Seq from (1, first, second, third)  upto (secondLastIndex, third-last, second-last, last)
    /// The length of the resulting seq is two shorter than the input seq.
    /// Use Seq.iPrevThisNext if you want a looped sequence upto (lastIndex,second-last, last, first)
    let windowed3i (xs:seq<'T>) : seq<int*'T *'T*'T> =  seq { 
        use e = xs.GetEnumerator()        
        if e.MoveNext() then
            let mutable prev =  e.Current            
            if e.MoveNext() then
                let mutable this = e.Current
                if e.MoveNext() then                   
                    let mutable kk = 1
                    prev <- this  
                    this <- e.Current
                    while e.MoveNext() do 
                        yield  kk, prev, this, e.Current
                        kk <- kk + 1
                        prev <- this 
                        this <- e.Current
                else                     
                    IndexOutOfRangeException.Raise "Seq.windowed3i: Input Sequence only had two elements: %s" xs.ToNiceString
            else
                IndexOutOfRangeException.Raise "Seq.windowed3i: Input Sequence only had one element: %s" xs.ToNiceString
        else
            IndexOutOfRangeException.Raise "Seq.windowed3i: Empty Input Sequence %A" xs} 

