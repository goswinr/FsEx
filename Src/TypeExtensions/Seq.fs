namespace FsEx

open System
open System.Collections.Generic
open System.Runtime.CompilerServices

[<AutoOpen>]
module TypeExtensionsSeq = 
    
    let internal indexFromBack ix (xs: 'T seq) =     
        match xs with
        | :? ('T[]) as a ->     a.GetNeg(a.Length - 1 - ix)
        | :? ('T IList) as a -> a.GetNeg(a.Count  - 1 - ix) //ResizeArray and other collections           
        | :? ('T list) as a ->  List.getNeg (- 1 - ix) a
        | _ -> 
            // ther are two ways to get an item indexed from the back:
            // (1) iterate all items and keep a buffer
            // (2) iterate once to find length, and second time to find item ( no buffer)
            // using (1) here:
            use e = xs.GetEnumerator()
            if e.MoveNext() then        
                let ar = Array.zeroCreate (ix+1) // use array for buffer
                let mutable i = 0
                let mutable k = 0 
                while (e.MoveNext()) do 
                    k <-i % (ix+1)//loop index
                    ar.[k]<- e.Current 
                    i <- i+1
                if ix > i then failwithf "can't get index from back %d from  seq of %d items" ix (i+1)
                ar.GetNeg(k-ix) 
            else
                failwithf "can't get index from back %d from empty seq" ix
    

    //[<Extension>] //Error 3246
    type Collections.Generic.IEnumerable<'T>  with 
        
        
        /// Gets an item by index position in the Seq
        /// Allows for negtive index too (like Python)
        [<Extension>] 
        member this.GetNeg (index) = 
            try 
                if index >= 0 then Seq.item index this
                else indexFromBack ( 1 - index ) this
            with 
            | :? InvalidOperationException  as ex -> failwithf "seq.GetNeg(%d): Can not get %dth item of %s : %s" index index (NiceString.toNiceStringFull this) ex.Message
            | ex -> raise ex //some other error raised while constructing lazy seq

        
        /// Gets an item by index position in the Seq
        /// Allows for negtive index too (like Python)
        [<Extension;Obsolete>] 
        member this.GetItem (index) = this.GetNeg (index) // TODO Delete          

        ///Returns Seq.length - 1
        [<Extension>]
        member this.LastIndex = 
            if Seq.isEmpty this then failwithf "seq.LastIndex: Can not get LastIndex of empty Seq"
            (Seq.length this) - 1
        
        /// Gets the last item in the Seq
        [<Extension>]
        member this.Last = indexFromBack 0 this            
        
        /// Gets the second last item in the Seq
        [<Extension>] 
        member this.SecondLast = indexFromBack 1 this
        
        /// Gets the third last item in the Seq
        [<Extension>] 
        member this.ThirdLast = indexFromBack 2 this

        /// Gets the first item in the Seq
        [<Extension>]
        member this.First = 
            if Seq.isEmpty this then failwithf "seq.First: Can not get LastIndex of empty Seq"
            Seq.head this
        
        /// Gets the second item in the Seq
        [<Extension>]
        member this.Second = 
            try 
                this|> Seq.skip 1 |> Seq.head
            with 
            | :? InvalidOperationException  as ex -> failwithf "seq.Second: Can not get Second item of %s : %s"  (NiceString.toNiceStringFull this) ex.Message
            | ex -> raise ex  //some other error raised while constructing lazy seq          
        
        /// Gets the third item in the Seq
        [<Extension>]
        member this.Third = 
            try 
                this|> Seq.skip 2 |> Seq.head
            with 
            | :? InvalidOperationException  as ex -> failwithf "seq.Third: Can not get Third item of %s : %s"  (NiceString.toNiceStringFull this) ex.Message
            | ex -> raise ex   //some other error raised while constructing lazy seq         
        
 

        /// Seq.Slice
        /// Allows for negative indices too, like Python, -1 is the last element.
        /// The resulting seq includes the item at slice-ending-index. like F# range expressions include the last integer e.g.: 0..5
        [<Extension>]
        member this.Slice(startIdx:int , endIdx: int) : 'T seq = // don't overload .GetSlice .[ x ... y] directly, this would be a casting horror for Lists and arrays wher neg indices dont work
            let count = lazy(Seq.length this)
            let st  = if startIdx< 0 then count.Value + startIdx        else startIdx
            let len = if endIdx  < 0 then count.Value + endIdx - st + 1 else endIdx - st + 1
            try 
                this|> Seq.skip st |> Seq.take len
            with 
            | :? InvalidOperationException  as ex -> 
                let en =  if endIdx < 0 then count.Value + endIdx else endIdx
                failwithf "seq.GetSlice: Start index '%A' (= %d) and end index '%A'(= %d) for Seq of %d items failed for %s : %s" startIdx st endIdx en  count.Value (NiceString.toNiceStringFull this) ex.Message
            | ex -> raise ex //some other error raised while constructing lazy seq
            
        
        [<Extension>]  
        /// A property like the ToString() method, 
        /// But with richer formationg for collections
        member obj.ToNiceString = NiceString.toNiceString obj
            
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] //need this so doesn't hide Seq class in C# assemblies (should consider for other extension modules as well)
module Seq =   
    
    /// faster implemetation of Seq.last till F# 4.8  or 5.0 is out
    let lastFast (source : seq<_>) = TypeExtensionsSeq.indexFromBack 0  source // TODO keep this until https://github.com/dotnet/fsharp/pull/7765/files is part of fsharp core

    /// Allows for negative indices too, -1 is the last element.
    /// The resulting seq includes the item at slice-ending-index. like F# range expressions include the last integer e.g.: 0..5
    let slice startIdx endIdx (xs:seq<_>) =  xs.Slice(startIdx,endIdx) 

    /// Gets an item by index position in the Seq
    /// Allows for negtive index too (like Python)  
    [<Obsolete>]
    let getItem index  (xs:seq<_>) = xs.GetNeg(index) //TODO delete


    /// Gets an item by index position in the Seq
    /// Allows for negtive index too (like Python)  
    let getNeg index  (xs:seq<_>) = xs.GetNeg(index)

    /// Considers sequence cirular and move elements up or down
    /// e.g.: rotate +1 [ a, b, c, d] = [ d, a, b, c]
    /// e.g.: rotate -1 [ a, b, c, d] = [ b, c, d, a]
    let rotate r (xs:seq<_>) = xs |> Rarr.ofSeq |> Rarr.rotate r
        
    /// Yields the Seq without the last element
    let skipLast (xs:seq<_>) =  seq{ 
        use e = xs.GetEnumerator() 
        if e.MoveNext() then
            let prev = ref e.Current
            while e.MoveNext() do
                yield  !prev
                prev := e.Current
        else
            failwith "skipLast: Empty Input Sequence"}
    
    /// splits seq in two, like Seq.filter but returning both
    /// the first Rarr has all elements where the filter function returned 'true'
    let splitBy filter (xs:seq<_>) =  
        let t=Rarr()
        let f=Rarr()
        for x in xs do
            if filter x then t.Add(x)
            else             f.Add(x)
        t,f
                

    /// Yields looped Seq of (this, next) from (first, second)  upto (last, first)
    /// The length of the resulting seq is the same as the input seq.
    /// Use Seq.skipLast afterwards or Seq.windowed if you don't want a looped sequence.
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
    
    /// Yields looped Seq of (index,this, next) from (0,first, second)  upto (lastIndex, last, first)
    /// The length of the resulting seq is the same as the input seq.
    /// Use Seq.skipLast afterwards or Seq.windowed if you don't want a looped sequence.
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


    /// Yields looped Seq of (previous, this, next): from (last, first, second)  upto (second-last, last, first)
    /// The length of the resulting seq is the same as the input seq.
    /// Use Seq.skipLast afterwards or Seq.windowed if you don't want a looped sequence.
    let prevThisNext (xs:seq<_>) =  seq { 
        use e = xs.GetEnumerator()
        if e.MoveNext() then
            let prev = ref e.Current
            let first = e.Current
            if e.MoveNext() then
                let this = ref e.Current
                if e.MoveNext() then                    
                    yield indexFromBack 0 xs ,!prev, !this //yield Seq.last xs ,!prev, !this
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

    /// Yields looped Seq of (index, previous, this, next): from (0, last, first, second)  upto (lastIndex, second-last, last, first)
    /// The length of the resulting seq is the same as the input seq.
    /// Use Seq.skipLast afterwards or Seq.windowed if you don't want a looped sequence.
    let iPrevThisNext (xs:seq<_>) =  seq { 
        use e = xs.GetEnumerator()
        let kk = ref 2
        if e.MoveNext() then
            let prev = ref e.Current
            let first = e.Current
            if e.MoveNext() then
                let this = ref e.Current
                if e.MoveNext() then
                    yield  0, indexFromBack 0 xs ,!prev, !this //     yield  0, Seq.last xs ,!prev, !this
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

