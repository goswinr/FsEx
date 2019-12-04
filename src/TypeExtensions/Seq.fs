namespace FsEx

open System
open System.Collections.Generic


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
        if ii<0 || ii >= len then failwithf "Cannot get index %d of Seq with %d items" i len
        ii

    let private get i (xs:seq<'T>) : 'T =        
        match xs with
        //| :? ('T[])  as xs -> xs.[negIdx i xs.Length] // covered by IList
        | :? ('T IList)  as xs -> xs.[negIdx i xs.Count]
        | :? ('T list)   as xs -> List.item (negIdx i (List.length xs)) xs
        | _ -> Seq.item  (negIdx i (Seq.length xs)) xs

    let private set i x (xs:seq<_>) :unit =
        match xs with
        | :? ('T[])       as xs -> xs.[negIdx i xs.Length]<- x // why not covered by IList?
        | :? ('T IList)   as xs -> xs.[negIdx i xs.Count] <- x
        | _ -> failwithf "Cannot set items on this Seq (is it a dict, lazy or immutable ?)"

    let rec private listlast (list: 'T list) =     
        match list with            
        | [x] -> x            
        | _ :: tail -> listlast tail          
        | [] -> invalidArg "list" "ListWasEmpty"    
        
    /// faster implemetation of Seq.last , keep till F# 4.8 is out
    let internal fastLast (source : seq<_>) = // keep this until https://github.com/dotnet/fsharp/pull/7765/files is part of fsharp core
        match source with
        | :? ('T[]) as a -> 
            if a.Length = 0 then invalidArg "source" "sourceWasEmpty"
            else a.[a.Length - 1]

        | :? ('T IList) as a -> //ResizeArray and other collections
            if a.Count = 0 then invalidArg "source" "sourceWasEmpty"
            else a.[a.Count - 1]

        | :? ('T list) as a -> listlast a 

        | _ -> 
            use e = source.GetEnumerator()
            if e.MoveNext() then
                let mutable res = e.Current
                while (e.MoveNext()) do res <- e.Current
                res
            else
                invalidArg "source" "sourceWasEmpty" 
    

    [<EXT>]
    type Collections.Generic.IEnumerable<'T>  with 

        /// Like Seq.length - 1
        [<EXT>]
        member this.LastIndex = (Seq.length this) - 1

        /// Last item in Seq
        [<EXT>]
        member this.Last = fastLast this
    
        [<EXT>] 
        ///Allows for negtive indices too (like Python)
        member this.Item 
            with get i   = get i this
            and  set i x = set i x this


        ///Allows for negative indices too.
        ///The resulting seq includes the item at slice-ending-index. like F# range expressions include the last integer e.g.: 0..5
        [<EXT>]
        member this.GetSlice(startIdx,endIdx) : 'T seq = // to use slicing notation e.g. : xs.[ 1 .. -2]
            let count = Seq.length this
            let st  = match startIdx with None -> 0        | Some i -> if i<0 then count+i      else i
            let len = match endIdx   with None -> count-st | Some i -> if i<0 then count+i-st+1 else i-st+1

            if st < 0 || st > count-1 then 
                let err = sprintf "GetSlice: Start index %d is out of range. Allowed values are -%d up to %d for Seq of %d items" startIdx.Value count (count-1) count
                raise (IndexOutOfRangeException(err))

            if st+len > count then 
                let err = sprintf "GetSlice: End index %d is out of range. Allowed values are -%d up to %d for Seq of %d items" endIdx.Value count (count-1) count
                raise (IndexOutOfRangeException(err)) 

            if len < 0 then
                let en =  match endIdx  with None -> count-1 | Some i -> if i<0 then count+i else i
                let err = sprintf "GetSlice: Start index '%A' (= %d) is bigger than end index '%A'(= %d) for Seq of %d items" startIdx st endIdx en  count
                raise (IndexOutOfRangeException(err)) 

            this|> Seq.skip st |> Seq.take len

module Seq =   
    
    ///Allows for negtive slice index too ( -1 = last element), returns a shallow copy including the end index.
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

