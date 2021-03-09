namespace FsEx

open System
open System.Collections.Generic
open System.Runtime.CompilerServices

//[<AutoOpen>]

/// provides Extension methods on Collections.Generic.IEnumerable 
module ExtensionsSeq = 
    
    open ExtensionsIList // for a.GetNeg

    let internal indexFromBack ix (xs: 'T seq) =     
        match xs with
        | :? ('T[]) as a ->     a.GetNeg(a.Length - 1 - ix)
        | :? ('T Rarr) as a  -> a.GetNeg(a.Count  - 1 - ix) //ResizeArray and other collections           
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

    
    type Collections.Generic.IEnumerable<'T>  with 
        
        
        /// Gets an item by index position in the Seq
        /// Allows for negtive index too (like Python)
        [<Extension>] 
        member this.GetNeg (index) = 
            try 
                if index >= 0 then Seq.item index this
                else indexFromBack ( 1 - index ) this
            with 
            | :? InvalidOperationException  as ex -> failwithf "seq.GetNeg(%d): Failed to get %dth item of %s : %s" index index (NiceString.toNiceStringFull this) ex.Message
            | ex -> raise ex //some other error raised while constructing lazy seq

        
        /// Gets an item by index position in the Seq
        /// Allows for negtive index too (like Python)
        [<Extension;Obsolete>] 
        member this.GetItem (index) = this.GetNeg (index) // TODO Delete          

        ///Returns Seq.length - 1
        [<Extension>]
        member this.LastIndex = 
            if Seq.isEmpty this then failwithf "seq.LastIndex: Failed to get LastIndex of empty Seq" //TODO fix Exeption type
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
            if Seq.isEmpty this then failwithf "seq.First: Failed to get LastIndex of empty Seq"
            Seq.head this
        
        /// Gets the second item in the Seq
        [<Extension>]
        member this.Second = 
            try 
                this|> Seq.skip 1 |> Seq.head
            with 
            | :? InvalidOperationException  as ex -> failwithf "seq.Second: Failed to get second item of %s : %s"  (NiceString.toNiceStringFull this) ex.Message
            | ex -> raise ex  //some other error raised while constructing lazy seq          
        
        /// Gets the third item in the Seq
        [<Extension>]
        member this.Third = 
            try 
                this|> Seq.skip 2 |> Seq.head
            with 
            | :? InvalidOperationException  as ex -> failwithf "seq.Third: Failed to get third item of %s : %s"  (NiceString.toNiceStringFull this) ex.Message
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
            
        
        /// A property like the ToString() method, 
        /// But with richer formationg for collections
        [<Extension>]  
        member obj.ToNiceString = NiceString.toNiceString obj
            
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] //need this so doesn't hide Seq class in C# assemblies (should consider for other extension modules as well)
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
    let  hasItems count (xs : seq<'T>) : bool =
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
    let  hasMinimumItems count (xs : seq<'T>) : bool =
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
    let  hasMaximumItems count (xs : seq<'T>) : bool =
        match xs with
        | :? ('T[]) as a ->     a.Length <= count
        | :? ('T Rarr) as a  -> a.Count  <= count       
        | :? ('T IList) as a -> a.Count  <= count
        | _ ->
            let mutable k = 0
            use e = xs.GetEnumerator()
            while e.MoveNext() && k <= count do  k <- k+1
            k <= count

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
            let mutable prev =  e.Current
            while e.MoveNext() do
                yield  prev
                prev <- e.Current
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
                    failwithf "Seq.thisNext: Input Sequence only had one element %A" xs.ToNiceString
            else
                failwith "Seq.thisNext: Empty Input Sequence"}
    
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
                    failwithf "Seq.windowed2: Input Sequence only had one element %A" xs.ToNiceString
            else
                failwith "Seq.windowed2: Empty Input Sequence"}  


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
                    failwith "thisNextLooped: Input Sequence only had one element"
            else
                failwith "thisNextLooped: Empty Input Sequence"}

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
                    failwithf "Seq.windowed2i: Input Sequence only had one element %A" xs.ToNiceString
            else
                failwith "Seq.windowed2i: Empty Input Sequence"}  


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
                    failwithf "Seq.prevThisNextLooped: Input Sequence only had two elements: %s" xs.ToNiceString
            else
                failwithf "Seq.prevThisNextLooped: Input Sequence only had one element: %s" xs.ToNiceString
        else
            failwithf "Seq.prevThisNextLooped: Empty Input Sequence %A" xs} 


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
                    failwithf "Seq.windowed3: Input Sequence only had two elements: %s" xs.ToNiceString
            else
                failwithf "Seq.windowed3: Input Sequence only had one element: %s" xs.ToNiceString
        else
            failwithf "Seq.windowed3: Empty Input Sequence %A" xs} 


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
                    failwithf "prevThisNextLooped: Input Sequence %s only had two elements" xs.ToNiceString
            else
                failwithf "prevThisNextLooped: Input Sequence %s only had one element" xs.ToNiceString
        else
            failwithf "prevThisNextLooped: Empty Input Sequence %A" xs} 

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
                    failwithf "Seq.windowed3i: Input Sequence only had two elements: %s" xs.ToNiceString
            else
                failwithf "Seq.windowed3i: Input Sequence only had one element: %s" xs.ToNiceString
        else
            failwithf "Seq.windowed3i: Empty Input Sequence %A" xs} 