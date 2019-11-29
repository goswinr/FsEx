namespace FsEx

open System

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
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
            let err = sprintf "Seq.slice: Start Index '%d' (= %d) is bigger than End Index '%d'(= %d) for Seq of %d items" startIdx st startIdx en  count
            raise (IndexOutOfRangeException(err))
            
        xs |> Seq.skip st |> Seq.take len        


    ///Considers sequence cirular and move elements up or down
    /// e.g.: rotate +1 [ a, b, c, d] = [ d, a, b, c]
    /// e.g.: rotate -1 [ a, b, c, d] = [ b, c, d, a]
    let rotate r (xs:seq<_>) = xs |> ResizeArray.ofSeq |> ResizeArray.rotate r

    
    ///Yields Seq of (this, next) from (first, second)  upto (second-last, last)
    let thisAndNext (xs:seq<_>) = seq{ 
        use e = xs.GetEnumerator()        
        if e.MoveNext() then
            let prev = ref e.Current
            if e.MoveNext() then
                    yield !prev, e.Current 
                    prev := e.Current 
                    while e.MoveNext() do 
                        yield  !prev, e.Current 
                        prev := e.Current
            else
                failwith "thisNext: Input Sequence only had one element"
        else
            failwith "thisNext: Empty Input Sequence"}
    
    ///Yields Seq of (index this, this, next) from (first, second)  upto (second-last, last)
    let iThisAndNext (xs:seq<_>) = seq{
        use e = xs.GetEnumerator()
        let kk = ref 0              
        if e.MoveNext() then
            let prev = ref e.Current
            if e.MoveNext() then
                    yield !kk,!prev, e.Current 
                    prev := e.Current 
                    while e.MoveNext() do
                        incr kk 
                        yield !kk,!prev, e.Current 
                        prev := e.Current
            else
                failwith "* thisNext: Input Sequence only had one element"
        else
            failwith "* thisNext: Empty Input Sequence"}


    ///Yields looped Seq of (this, next) from (first, second)  upto (last, first)
    let thisAndNextLooped (xs:seq<_>) =  seq{ 
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
    
    ///Yields looped Seq of (this, next) from (first, second)  upto (last, first)
    let iThisAndNextLooped (xs:seq<_>) =  seq{ 
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

    ///Yields Seq of (this, next, Nextafter): from (first, second, third)  upto (third-last, second-last, last)
    let thisNextAndNextafter (xs:seq<_>) =  seq{ 
        use e = xs.GetEnumerator()
        if e.MoveNext() then
            let prev = ref e.Current
            if e.MoveNext() then
                let this = ref e.Current
                if e.MoveNext() then
                    yield !prev, !this, e.Current 
                    prev := !this 
                    this := e.Current                        
                    while e.MoveNext() do 
                        yield  !prev, !this, e.Current
                        prev := !this  
                        this := e.Current                              
                else
                    failwith "thisNextNextaftert: Input Sequence only had two elements"
            else
                failwith "thisNextNextaftert: Input Sequence only had one element"
        else
            failwith "thisNextNextaftert: Empty Input Sequence"}
    
    ///Yields looped Seq of (this, next, Nextafter): from (first, second, third)  upto (last, first, second)
    let thisNextAndNextafterLooped (xs:seq<_>) =  seq{ 
        use e = xs.GetEnumerator()
        if e.MoveNext() then
            let prev = ref e.Current
            let first = e.Current
            if e.MoveNext() then
                let this = ref e.Current
                let second = e.Current
                if e.MoveNext() then
                    yield !prev, !this, e.Current
                    prev := !this  
                    this := e.Current
                    while e.MoveNext() do 
                        yield  !prev, !this, e.Current
                        prev := !this 
                        this := e.Current                            
                    yield !prev, !this, first
                    yield !this, first, second
                else
                    failwithf "thisNextNextaftertLooped: Input Sequence %A only had two elements" xs
            else
                failwithf "thisNextNextaftertLooped: Input Sequence %A only had one element" xs
        else
            failwithf "thisNextNextaftertLooped: Empty Input Sequence %A" xs}
    
    ///Yields looped Seq of (Index of next, this, next, Nextafter): from (1, first, second, third)  upto (0, last, first, second)
    let iThisNextAndNextafterLooped (xs:seq<_>) =  seq{ 
        use e = xs.GetEnumerator()
        let kk = ref 2
        if e.MoveNext() then
            let prev = ref e.Current
            let first = e.Current
            if e.MoveNext() then
                let this = ref e.Current
                let second = e.Current
                if e.MoveNext() then
                    yield 1, !prev, !this, e.Current                    
                    prev := !this  
                    this := e.Current
                    while e.MoveNext() do 
                        yield  !kk,!prev, !this, e.Current
                        incr kk
                        prev := !this 
                        this := e.Current                            
                    yield !kk, !prev, !this, first
                    yield 0, !this, first, second
                else
                    failwithf "thisNextNextaftertLooped: Input Sequence %A only had two elements" xs
            else
                failwithf "thisNextNextaftertLooped: Input Sequence %A only had one element" xs
        else
            failwithf "thisNextNextaftertLooped: Empty Input Sequence %A" xs}    

    ///Yields looped Seq of (previous, this, next): from (last, first, second)  upto (second-last, last, first)
    ///Consider "thisNextNextafterLooped" as faster since the last element is not required from the start on.
    let prevThisNextLooped (xs:seq<_>) =  seq { 
        use e = xs.GetEnumerator()
        if e.MoveNext() then
            let prev = ref e.Current
            let first = e.Current
            if e.MoveNext() then
                let this = ref e.Current
                if e.MoveNext() then
                    yield Seq.last xs ,!prev, !this
                    yield !prev, !this, e.Current
                    prev := !this  
                    this := e.Current
                    while e.MoveNext() do 
                        yield  !prev, !this, e.Current
                        prev := !this 
                        this := e.Current                            
                    yield !prev, !this, first
                else                     
                    failwithf "prevThisNextLooped: Input Sequence %A only had two elements" xs
            else
                failwithf "prevThisNextLooped: Input Sequence %A only had one element" xs
        else
            failwithf "prevThisNextLooped: Empty Input Sequence %A" xs} 

    ///Yields looped Seq of (index, previous, this, next): from (0, last, first, second)  upto (lastIndex, second-last, last, first)
    ///Consider "iThisNextNextafterLooped" as faster since the last element is not required from the start on.
    let iPrevThisNextLooped (xs:seq<_>) =  seq { 
        use e = xs.GetEnumerator()
        let kk = ref 2
        if e.MoveNext() then
            let prev = ref e.Current
            let first = e.Current
            if e.MoveNext() then
                let this = ref e.Current
                if e.MoveNext() then
                    yield  0, Seq.last xs ,!prev, !this
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
                    failwithf "prevThisNextLooped: Input Sequence %A only had two elements" xs
            else
                failwithf "prevThisNextLooped: Input Sequence %A only had one element" xs
        else
            failwithf "prevThisNextLooped: Empty Input Sequence %A" xs} 

