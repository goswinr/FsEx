namespace FsEx

open System
open System.Collections.Generic
open System.Runtime.CompilerServices

[<AutoOpen>]
module TypeExtensionsSeq = 
    
    /// Any int will give a valid index for given collection size.
    // e.g.: -1 is  last item 
    let inline saveIdx i len =
        let rest = i % len
        if rest >= 0 then rest // does not fail on -4 for len 4
        else len + rest

    /// Converts negative indices to positive ones 
    /// e.g.: -1 is  last item .
    let inline negIdx i len =
        let ii =  if i<0 then len+i else i
        if ii<0 || ii >= len then failwithf "Cannot get index %d of seq, array or IList with %d items" i len
        ii


    let rec private listlast (list: 'T list) =     
        match list with            
        | [x] -> x            
        | _ :: tail -> listlast tail          
        | [] -> invalidArg "list" "F# ListWasEmpty"    
        
    /// faster implemetation of Seq.last , keep till F# 4.8 is out
    let internal fastLast (source : seq<_>) = // keep this until https://github.com/dotnet/fsharp/pull/7765/files is part of fsharp core
        match source with
        | :? ('T[]) as a -> 
            if a.Length = 0 then invalidArg "source" "array sourceWasEmpty"
            else a.[a.Length - 1]

        | :? ('T IList) as a -> //ResizeArray and other collections
            if a.Count = 0 then invalidArg "source" "IList sourceWasEmpty"
            else a.[a.Count - 1]

        | :? ('T list) as a -> listlast a 

        | _ -> 
            //printfn "fastlast on seq"
            use e = source.GetEnumerator()
            if e.MoveNext() then
                let mutable res = e.Current
                while (e.MoveNext()) do res <- e.Current
                res
            else
                invalidArg "source" "seq sourceWasEmpty" 
    

    [<Extension>]
    type Collections.Generic.IEnumerable<'T>  with 

        /// Like Seq.length - 1
        [<Extension>]
        member this.LastIndex = (Seq.length this) - 1

        /// Last item in Seq
        [<Extension>]
        member this.Last = fastLast this    

        ///Allows for negative indices too, like Python, -1 is the last element.
        [<Extension>] 
        member this.Item 
            with get i   = 
                match this with
                | :? ('T IList)         as xs -> xs.[negIdx i xs.Count] //covers Array too
                | :? ('T list)          as xs -> 
                    try
                        if i<0 then List.item ((List.length xs)+i) xs
                        else List.item  i xs
                    with _ -> 
                        raise (IndexOutOfRangeException(sprintf "Cannot get index %d from F# list of length %d" i (List.length xs)))
                | _ ->  
                    try
                        if i<0 then Seq.item ((Seq.length this)+i) this
                        else Seq.item  i this
                    with _ -> 
                        raise (IndexOutOfRangeException(sprintf "Cannot get index %d from seq of length %d" i (Seq.length this)))

            and  set i (x:'T)  = 
                match this with // matching need to be inline here otherwise cast from array to IList fails
                | :? ('T IList)   as xs -> xs.[negIdx i xs.Count] <- x
                | _ -> failwithf "Cannot set items on this Seq (is it a dict, lazy or immutable ?)"

        
        ///Allows for negative indices too, like Python, -1 is the last element.
        ///The resulting seq includes the item at slice-ending-index. like F# range expressions include the last integer e.g.: 0..5
        [<Extension>]
        member this.GetSlice(startIdx,endIdx) : 'T seq = // to use slicing notation e.g. : xs.[ 1 .. -2]
            let count = lazy(Seq.length this)
            let st  = match startIdx with None -> 0              | Some i -> if i<0 then count.Value+i      else i
            let len = match endIdx   with None -> count.Value-st | Some i -> if i<0 then count.Value+i-st+1 else i-st+1
            try 
                this|> Seq.skip st |> Seq.take len
            with _ ->
                let en =  match endIdx  with None -> count.Value-1 | Some i -> if i<0 then count.Value+i else i
                let err = sprintf "GetSlice: Start index '%A' (= %d) and end index '%A'(= %d) for Seq of %d items failed" startIdx st endIdx en  count.Value
                raise (IndexOutOfRangeException(err))

            

module Seq =   
    
    ///Allows for negative indices too, -1 is the last element.
    ///The resulting seq includes the item at slice-ending-index. like F# range expressions include the last integer e.g.: 0..5
    let slice startIdx endIdx (xs:seq<_>) =    
        let count = Seq.length xs
        let st  = if startIdx < 0 then count + startIdx        else startIdx
        let len = if endIdx   < 0 then count + endIdx - st + 1 else endIdx - st + 1

        if st < 0 || st > count-1 then 
            let err = sprintf "Seq.slice: Start Index %d is out of Range. Allowed values are -%d upto %d for Seq of %d items" startIdx count (count-1) count
            raise (IndexOutOfRangeException(err))
        
        if st+len > count then 
            let err = sprintf "Seq.slice: End Index %d is out of Range. Allowed values are -%d upto %d for Seq of %d items" endIdx count (count-1) count
            raise (IndexOutOfRangeException(err)) 
            
        if len < 0 then
            let en =  if endIdx<0 then count + endIdx else endIdx
            let err = sprintf "Seq.slice: Start Index '%d' (= %d) is bigger than End Index '%d'(= %d) for Seq of %d items" startIdx st endIdx en  count
            raise (IndexOutOfRangeException(err))
            
        xs |> Seq.skip st |> Seq.take len        


    ///Considers sequence cirular and move elements up or down
    /// e.g.: rotate +1 [ a, b, c, d] = [ d, a, b, c]
    /// e.g.: rotate -1 [ a, b, c, d] = [ b, c, d, a]
    let rotate r (xs:seq<_>) = xs |> ResizeArray.ofSeq |> ResizeArray.rotate r

    
    ///Yields the Seq without the last element
    let skipLast (xs:seq<_>) =  seq{ 
        use e = xs.GetEnumerator() 
        if e.MoveNext() then
            let prev = ref e.Current
            while e.MoveNext() do
                yield  !prev
                prev := e.Current
        else
            failwith "skipLast: Empty Input Sequence"}


    ///Yields looped Seq of (this, next) from (first, second)  upto (last, first)
    ///The length of the resulting seq is the same as the input seq.
    ///Use Seq.skip.Last afterwerds or Seq.windowed if you don't want a looped seqence.
    let thisNext(xs:seq<_>) =  seq{ 
        use e = xs.GetEnumerator()
        if e.MoveNext() then
            let prev = ref e.Current
            let first = e.Current
            if e.MoveNext() then
                    yield !prev, e.Current 
                    prev := e.Current 
                    while e.MoveNext() do 
                        yield  !prev, e.Current 
                        prev := e.Current
                    yield !prev, first
            else
                failwith "thisNextLooped: Input Sequence only had one element"
        else
            failwith "thisNextLooped: Empty Input Sequence"}
    
    ///Yields looped Seq of (index,this, next) from (0,first, second)  upto (lastIndex, last, first)
    ///The length of the resulting seq is the same as the input seq.
    ///Use Seq.skip.Last afterwerds or Seq.windowed if you don't want a looped seqence.
    let iThisNext (xs:seq<_>) =  seq{ 
        use e = xs.GetEnumerator()
        let kk = ref 0  
        if e.MoveNext() then
            let prev = ref e.Current
            let first = e.Current
            if e.MoveNext() then
                    yield !kk, !prev, e.Current 
                    prev := e.Current 
                    while e.MoveNext() do
                        incr kk 
                        yield  !kk, !prev, e.Current 
                        prev := e.Current
                    incr kk 
                    yield !kk, !prev, first
            else
                failwith "thisNextLooped: Input Sequence only had one element"
        else
            failwith "thisNextLooped: Empty Input Sequence"}

    
    
    /// faster implemetation of Seq.last till F# 4.8 is out
    let lastFast (source : seq<_>) = TypeExtensionsSeq.fastLast source // keep this until https://github.com/dotnet/fsharp/pull/7765/files is part of fsharp core

    ///Yields looped Seq of (previous, this, next): from (last, first, second)  upto (second-last, last, first)
    ///The length of the resulting seq is the same as the input seq.
    ///Use Seq.skip.Last afterwerds or Seq.windowed if you don't want a looped seqence.
    let prevThisNext (xs:seq<_>) =  seq { 
        use e = xs.GetEnumerator()
        if e.MoveNext() then
            let prev = ref e.Current
            let first = e.Current
            if e.MoveNext() then
                let this = ref e.Current
                if e.MoveNext() then                    
                    yield lastFast xs ,!prev, !this //yield Seq.last xs ,!prev, !this
                    yield !prev, !this, e.Current
                    prev := !this  
                    this := e.Current
                    while e.MoveNext() do 
                        yield  !prev, !this, e.Current
                        prev := !this 
                        this := e.Current                            
                    yield !prev, !this, first
                else                     
                    failwithf "prevThisNextLooped: Input Sequence only had two elements: %s" xs.ToNiceString
            else
                failwithf "prevThisNextLooped: Input Sequence only had one element: %s" xs.ToNiceString
        else
            failwithf "prevThisNextLooped: Empty Input Sequence %A" xs} 

    ///Yields looped Seq of (index, previous, this, next): from (0, last, first, second)  upto (lastIndex, second-last, last, first)
    ///The length of the resulting seq is the same as the input seq.
    ///Use Seq.skip.Last afterwerds or Seq.windowed if you don't want a looped seqence.
    let iPrevThisNext (xs:seq<_>) =  seq { 
        use e = xs.GetEnumerator()
        let kk = ref 2
        if e.MoveNext() then
            let prev = ref e.Current
            let first = e.Current
            if e.MoveNext() then
                let this = ref e.Current
                if e.MoveNext() then
                    yield  0, lastFast xs ,!prev, !this //     yield  0, Seq.last xs ,!prev, !this
                    yield  1, !prev, !this, e.Current
                    prev := !this  
                    this := e.Current
                    while e.MoveNext() do 
                        yield !kk, !prev, !this, e.Current
                        incr kk
                        prev := !this 
                        this := e.Current                            
                    yield !kk, !prev, !this, first
                else                     
                    failwithf "prevThisNextLooped: Input Sequence %s only had two elements" xs.ToNiceString
            else
                failwithf "prevThisNextLooped: Input Sequence %s only had one element" xs.ToNiceString
        else
            failwithf "prevThisNextLooped: Empty Input Sequence %A" xs} 

