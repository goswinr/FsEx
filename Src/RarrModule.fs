namespace FsEx

open System

/// Generic operations on Rarr which is like a System.Collections.Generic.List<'T> but with nicer erroe messages.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] //need this so doesn't hide Rarr class in C# assemblies (should consider for other extension modules as well)
module Rarr =    
        
    //---------------------------------------------------
    // extensions added only in FsEx:
    //----------------------------------------------------
        

    /// Gets an item at index 
    /// (use Rarr.GetNeg(i) member if you want to use negative indices too)
    let inline get index (rarr: Rarr<'T>) = Rarr.get index rarr.ResizeArray
        
    /// Sets an item at index 
    /// (use Rarr.SetNeg(i) member if you want to use negative indices too)
    let inline set index value  (rarr: Rarr<'T>)= Rarr.set index value rarr.ResizeArray

    /// Gets an item in the Rarr by index.
    /// Allows for negtive index too ( -1 is last item,  like Python)
    /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
    let inline getNeg index  (rarr: Rarr<'T>)= 
        let len = rarr.Count
        let ii =  if index < 0 then len + index else index
        if ii<0 || ii >= len then ArgumentOutOfRangeException.Raise "Rarr.GetNeg: Can't get index %d from Rarr of %d items: %A" index rarr.Count rarr
        rarr.[ii]        

    /// Sets an item in the Rarr by index.
    /// Allows for negtive index too ( -1 is last item,  like Python)
    /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
    let inline setNeg index value  (rarr: Rarr<'T>)= 
        let len = rarr.Count
        let ii =  if index < 0 then len + index else index
        if ii<0 || ii >= len then ArgumentOutOfRangeException.Raise "Rarr.SetNeg: Can't set index %d to %A rom Rarr of %d items: %A" index value rarr.Count rarr
        rarr.[ii] <- value        
   
    /// Any index will return a value.
    /// Rarr is treated as an endless loop in positive and negative direction   
    let inline getLooped index  (rarr: Rarr<'T>)= 
        let len = rarr.Count
        if len=0 then ArgumentOutOfRangeException.Raise "Rarr.GetLooped: Can't get index %d from Rarr of 0 items" index
        let t = index % len
        let ii = if t >= 0 then t  else t + len 
        rarr.[ii]              

    /// Any index will set a value.
    /// Rarr is treated as an endless loop in positive and negative direction   
    let inline setLooped index value  (rarr: Rarr<'T>) = 
        let len = rarr.Count
        if len=0 then ArgumentOutOfRangeException.Raise "Rarr.SetLooped: Can't Set index %d to %A in Rarr of 0 items" index value
        let t = index % len
        let ii = if t >= 0 then t  else t + len 
        rarr.[ii] <- value

    /// Get and remove last item from Rarr
    let inline pop  (rarr: Rarr<'T>)  =
        if rarr.Count=0 then ArgumentOutOfRangeException.Raise "Can't pop from empty Rarr"
        let i = rarr.Count - 1        
        let v = rarr.[i]
        rarr.RemoveAt(i)
        v



    /// Allows for negative indices too. ( -1 is last item, like Python)
    /// This method only exist to be consisten with other collections extensions in FsEx. like array. 
    /// You might prefer to use the F# slicing notation on Rarr.
    /// For Rarr this behaves the same way as the F# slicing notation defind in FsEx too, 
    /// (Only arrays need to use this method if they want to use negative indices since the GetSlice operators cant be overwritten.) 
    /// The resulting array includes the end index.
    /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
    let slice startIdx endIdx (rarr: Rarr<_>)  = rarr.GetSlice(startIdx, endIdx)

    let inline empty() = Rarr<_>()

    /// Create a Rarr by calling the given generator on each index.
    //-[<CompiledName("Init")>]
    let inline init count initializer : Rarr<'T> =
        if count < 0 then invalidArg "count" "The number of elements may not be negative."
        let rarr = Rarr (count)
        for i = 0 to count - 1 do rarr.Add ( initializer count)
        rarr

    /// Considers List cirular and move elements up or down
    /// e.g.: rotate +1 [ a, b, c, d] = [ d, a, b, c]
    /// e.g.: rotate -1 [ a, b, c, d] = [ b, c, d, a]
    let inline rotate k (rarr: Rarr<_>)  =  init rarr.Count (fun i -> rarr.[negIdxLooped (i-k) rarr.Count])


    /// Returns a Rarr of the index and the item. (like enumerate in Python)
    let inline indexed (rarr: Rarr<_>)  =  init rarr.Count (fun i -> i,rarr.[i])

    /// splits a Rarr in two, like Rarr.filter but returning both
    /// The first Rarr has all elements where the filter function returned 'true'
    let splitBy filter (rarr:seq<_>) =  
        let t=Rarr()
        let f=Rarr()
        for x in rarr do
            if filter x then t.Add(x)
            else             f.Add(x)
        t,f

    /// internal only for finding 
    module internal MinMax =
        //TODO test keeping of order if equal !
        
        let inline simple cmpF (xs:Rarr<'T>) =
            if xs.Count < 1 then ArgumentException.Raise "Empty %A in Rarr max / min" xs
            let mutable m = xs.[0]
            for i=1 to xs.Count-1 do
                if cmpF xs.[i] m then m <- xs.[i]
            m 
            
        let inline simple2 cmpF (xs:Rarr<'T>) =
            if xs.Count < 2 then ArgumentException.Raise "Only %d elements in %A, for Rarr first+second max / min" xs.Count xs
            let mutable m1 = xs.[0]
            let mutable m2 = xs.[1] 
            for i=1 to xs.Count-1 do
                let this = xs.[i]
                if cmpF this m1 then 
                    m2 <- m1
                    m1 <- this
                elif cmpF this m2 then
                    m2 <- this
            m1,m2  
                    

        /// If any are equal then the the order is kept by using ( a=b || ) since the comare operate does not include the equal test
        let inline sort3 cmp a b c  = 
            if a=b || cmp a b then 
                if  cmp b c then a,b,c
                else 
                    if  cmp a c then a,c,b
                    else             c,a,b
            else 
                if a=c || cmp a c then b,a,c
                else 
                    if b=c || cmp b c then b,c,a 
                    else                   c,b,a


        /// If any are equal then the the order is kept by using ( a=b || ) since the comare operate does not include the equal test
        let inline indexOfSort3By f cmp aa bb cc  = 
            let a = f aa
            let b = f bb
            let c = f cc
            if a=b || cmp a b then 
                if  cmp b c then 0,1,2
                else 
                    if  cmp a c then 0,2,1
                    else             2,0,1
            else 
                if a=c || cmp a c then 1,0,2
                else 
                    if b=c || cmp b c then 1,2,0
                    else                   2,1,0

        let inline simple3 cmpF (xs:Rarr<'T>) =
            if xs.Count < 3 then ArgumentException.Raise "Only %d elements in %A, for Rarr first+second+third max / min" xs.Count xs
            let e1 = xs.[0]
            let e2 = xs.[1]
            let e3 = xs.[2]
            // sort first 3
            let mutable m1, m2, m3 =  sort3 cmpF e1 e2 e3   // otherwise would fail on sorting first 3, test on Rarr([5;6;3;1;2;0])|> Rarr.max3 
            for i=3 to xs.Count-1 do
                let this = xs.[i]
                if cmpF this m1 then
                    m3 <- m2 
                    m2 <- m1
                    m1 <- this
                elif cmpF this m2 then
                    m3 <- m2 
                    m2 <- this
                elif cmpF this m3 then
                    m3 <- this
            m1,m2,m3 
                                    
        let inline indexByFun cmpF func (xs:Rarr<'T>) = 
            if xs.Count < 1 then ArgumentException.Raise "Empty %A, Rarr  max / min IndexByFun" xs
            let mutable f = func xs.[0]
            let mutable mf = f
            let mutable ii = 0
            for i=1 to xs.Count-1 do
                f <- func xs.[i] 
                if cmpF f mf then 
                    ii <- i
                    mf <- f
            ii

        let inline index2ByFun cmpF func (xs:Rarr<'T>) =
            if xs.Count < 2 then ArgumentException.Raise "Only %d elements in %A, for Rarr index2ByFun max / min" xs.Count xs            
            let mutable i1 = 0
            let mutable i2 = 1 
            let mutable mf1 = func xs.[i1]
            let mutable mf2 = func xs.[i2]
            let mutable f = mf1 // placeholder
            for i=1 to xs.Count-1 do
                f <- func xs.[i] 
                if cmpF f mf1 then
                    i2 <- i1 
                    i1 <- i
                    mf2 <- mf1 
                    mf1 <- f 
                elif cmpF f mf2 then
                    i2 <- i
                    mf2 <- f 
            i1,i2


        let inline index3ByFun (cmpOp:'U->'U->bool)  (byFun:'T->'U) (xs:Rarr<'T>) =
            if xs.Count < 3 then ArgumentException.Raise "Only %d elements in %A, for Rarr index3ByFun max / min" xs.Count xs 
            // sort first 3
            let mutable i1,i2,i3 =  indexOfSort3By byFun cmpOp xs.[0] xs.[1] xs.[2] // otherwise would fail on sorting first 3, test on Rarr([5;6;3;1;2;0])|> Rarr.max3 
            let mutable e1 =  byFun xs.[i1]
            let mutable e2 =  byFun xs.[i2]
            let mutable e3 =  byFun xs.[i3] 
            let mutable f = e1 // placeholder
            for i=3 to xs.Count-1 do
                f <- byFun xs.[i] 
                if cmpOp f e1 then
                    i3 <- i2 
                    i2 <- i1 
                    i1 <- i
                    e3 <- e2 
                    e2 <- e1 
                    e1 <- f 
                elif cmpOp f e2 then
                    i3 <- i2 
                    i2 <- i
                    e3 <- e2 
                    e2 <- f 
                elif cmpOp f e3 then
                    i3 <- i
                    e3 <- f 
            i1,i2,i3 
        
    

    (* covered by part copied fron Ext-Core

    // Returns the smallest element of the Rarr.
    let min rarr =     rarr |> MinMax.simple (<)  // why inline? type specialisation ?

    // Returns the biggest element of the Rarr.
    let max rarr =     rarr |> MinMax.simple (>)

    // Returns the smallest element of the Rarr.
    // Elements are compared by applying the predicate function first.
    let minBy f rarr = let i = rarr |> MinMax.indexByFun (<) f in rarr.[i]

    // Returns the biggest element of the Rarr.
    // Elements are compared by applying the predicate function first.
    let maxBy f rarr = let i = rarr |> MinMax.indexByFun (>) f in rarr.[i]
    *)

    /// Returns the Index of the smallest element of the Rarr.
    /// Elements are compared by applying the predicate function first.
    let minIndBy f rarr = rarr |> MinMax.indexByFun (<) f

    /// Returns the Index of the biggest element of the Rarr.
    /// Elements are compared by applying the predicate function first.
    let maxIndBy f rarr = rarr |> MinMax.indexByFun (>) f

    /// Returns the smallest two elements of the Rarr.
    /// If they are equal then the the order is kept
    let min2 rarr =     rarr |> MinMax.simple2 (<)

    /// Returns the biggest two elements of the Rarr.
    /// If they are equal then the the order is kept
    let max2 rarr =     rarr |> MinMax.simple2 (>)
        
    /// Returns the smallest two elements of the Rarr.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    let min2By f rarr = let i,ii = rarr |> MinMax.index2ByFun (<) f in rarr.[i],rarr.[ii]
        
    /// Returns the biggest two elements of the Rarr.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    let max2By f rarr = let i,ii = rarr |> MinMax.index2ByFun (>) f in rarr.[i],rarr.[ii]

    /// Returns the indices of the two smallest elements of the Rarr.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    let min2IndBy f rarr = rarr |> MinMax.index2ByFun (<) f

    /// Returns the indices of the two biggest elements of the Rarr.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    let max2IndBy f rarr = rarr |> MinMax.index2ByFun (>) f

    /// Returns the smallest three elements of the Rarr.
    /// If they are equal then the the order is kept
    let min3 rarr =  rarr |> MinMax.simple3 (<)

    /// Returns the biggest three elements of the Rarr.
    /// If they are equal then the the order is kept
    let max3 rarr =  rarr |> MinMax.simple3 (>)

    /// Returns the smallest three elements of the Rarr.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    let min3By f rarr = let i,ii,iii = rarr |> MinMax.index3ByFun (<) f in rarr.[i],rarr.[ii],rarr.[iii]

    /// Returns the biggest three elements of the Rarr.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    let max3By f rarr = let i,ii,iii = rarr |> MinMax.index3ByFun (>) f in rarr.[i],rarr.[ii],rarr.[iii]

    /// Returns the indices of the three smallest elements of the Rarr.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    let min3IndBy f rarr = rarr |> MinMax.index3ByFun(<) f

    /// Returns the indices of the three biggest elements of the Rarr.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    let max3IndBy f rarr = rarr |> MinMax.index3ByFun (>) f

   


    //-------------------------------------------------------------------------------------------------------------------------------
    // taken and adapded from https://github.com/jack-pappas/ExtCore/blob/master/ExtCore/Collections.Rarr.fs 
    // previously used https://github.com/dotnet/fsharp/tree/master/src/utils   
    //-------------------------------------------------------------------------------------------------------------------------------
    
    let inline checkNonNull tx rarr = 
        match rarr with null -> raise (ArgumentNullException("in module FsEx.Rarr: " + tx)) |_ -> ()

    open Microsoft.FSharp.Core
    open Microsoft.FSharp.Core.OptimizedClosures
    open System.Collections.Generic
    open LanguagePrimitives

    /// Return the length of the collection.
    //-[<CompiledName("Length")>]
    let inline length (rarr : Rarr<'T>) : int =
        rarr.Count

    /// Return true if the given array is empty, otherwise false.
    //-[<CompiledName("IsEmpty")>]
    let inline isEmpty (rarr : Rarr<'T>) : bool =
        rarr.Count = 0


    /// Create a Rarr whose elements are all initially the given value.
    //-[<CompiledName("Create")>]
    let create count value : Rarr<'T> =
        if count < 0 then
            invalidArg "count" "The number of elements may not be negative."

        let rarr = Rarr (count)
        for i = 0 to count - 1 do
            rarr.Add value
        rarr


    /// Adds an object to the end of the Rarr.
    //-[<CompiledName("Add")>]
    let inline add item (rarr : Rarr<'T>) : unit =
        rarr.Add item

    /// Determines whether an element is in the Rarr.
    //-[<CompiledName("Contains")>]
    let inline contains (value : 'T) (rarr : Rarr<'T>) : bool =
        //+ checkNonNull "rarr" rarr
        rarr.Contains value

    /// Build a Rarr from the given sequence.
    //-[<CompiledName("OfSeq")>]
    let inline ofSeq (sequence : seq<'T>) : Rarr<'T> =
        Rarr (sequence)

    /// Build a Rarr from the given list.
    //-[<CompiledName("OfList")>]
    let ofList (list : 'T list) : Rarr<'T> =
        ////+ checkNonNull "list" list
        //let len = list.Length //edit by Goswin, could be costly
        let res = Rarr<_>() //(len) 
        let rec add = function
            | [] -> ()
            | e::l -> res.Add(e); add l
        add list
        res

    /// Build a Rarr from the given array.
    //-[<CompiledName("OfArray")>]
    let inline ofArray (arr : 'T[]) : Rarr<'T> =
        Rarr (arr)


    /// Return a view of the Rarr as an enumerable object.
    //-[<CompiledName("ToSeq")>]
    let toSeq (rarr : Rarr<'T>) : seq<'T> =
        //+ checkNonNull "rarr" rarr
        Seq.readonly rarr

    /// Build a list from the given Rarr.
    //-[<CompiledName("ToList")>]
    let toList (rarr : Rarr<'T>) : 'T list =
        //+ checkNonNull "rarr" rarr
        let mutable res = []
        for i = length rarr - 1 downto 0 do
            res <- rarr.[i] :: res
        res

    /// Return a fixed-length array containing the elements of the input Rarr.
    //-[<CompiledName("ToArray")>]
    let inline toArray (rarr : Rarr<'T>) : 'T[] =
        rarr.ToArray ()

   
    /// Sorts the elements of the Rarr by mutating the Rarr in-place.
    /// Elements are compared using Operators.compare.
    //-[<CompiledName("SortInPlace")>]
    let inline sortInPlace<'T when 'T : comparison> (rarr : Rarr<'T>) : unit =
        rarr.Sort ()
        
    /// Sort the elements using the key extractor and generic comparison on the keys.
    //-[<CompiledName("SortInPlaceBy")>]
    let inline sortInPlaceBy<'T, 'Key when 'Key : comparison>     (projection : 'T -> 'Key) (rarr : Rarr<'T>) =
        rarr.Sort (fun x y ->
            compare (projection x) (projection y))

    /// Sort the elements using the given comparison function.
    //-[<CompiledName("SortInPlaceWith")>]
    let inline sortInPlaceWith (comparer : 'T -> 'T -> int) (rarr : Rarr<'T>) : unit =
        rarr.Sort (comparer)

    /// Build a new Rarr that contains the elements of the given Rarr.
    //-[<CompiledName("Copy")>]
    let inline copy (rarr : Rarr<'T>) : Rarr<'T> =
        Rarr (rarr)

    /// Return an array containing the given element.
    //-[<CompiledName("Singleton")>]
    let singleton value : Rarr<'T> =
        let rarr = Rarr ()
        rarr.Add value
        rarr

    /// Build a new Rarr that contains the elements of each of the given sequence of Rarrs.
    //-[<CompiledName("Concat")>]
    let concat (rarrs : seq<Rarr<'T>>) : Rarr<'T> =
        //+ checkNonNull "rarrs" rarrs
        let flattened = Rarr ()
        for rarr in rarrs do
            flattened.AddRange rarr
        flattened
    
    /// Build a new Rarr that contains the elements of the first Rarr followed by
    /// the elements of the second Rarr.
    //-[<CompiledName("Append")>]
    let append (rarr1 : Rarr<'T>) (rarr2 : Rarr<'T>) : Rarr<'T> =
        //+ checkNonNull "rarr1" rarr1
        //+ checkNonNull "rarr2" rarr2
        let combined = Rarr (rarr1.Count + rarr2.Count)
        combined.AddRange rarr1
        combined.AddRange rarr2
        combined

    /// Build a new Rarr that contains the given subrange specified by
    /// starting index and length.
    //-[<CompiledName("Sub")>]
    let sub (rarr : Rarr<'T>) start count : Rarr<'T> =
        //+ checkNonNull "rarr" rarr
        if start < 0 then
            invalidArg "start" "The start index cannot be less than zero (0)."
        elif count < 0 then
            invalidArg "count" "The number of elements to copy cannot be less than zero (0)."
        elif start + count > length rarr then
            invalidArg "count" "There are fewer than 'count' elements between the 'start' index and the end of the collection."
    
        rarr.GetRange (start, count)

    /// Fill a range of the collection with the given element.
    //-[<CompiledName("Fill")>]
    let fill (rarr : Rarr<'T>) start count value : unit =
        //+ checkNonNull "rarr" rarr
        if start < 0 then
            invalidArg "start" "The start index cannot be less than zero (0)."
        elif count < 0 then
            invalidArg "count" "The number of elements to copy cannot be less than zero (0)."
        elif start + count > length rarr then
            invalidArg "count" "There are fewer than 'count' elements between the 'start' index and the end of the collection."
    
        // Overwrite the items within the range using the specified value.
        for i = start to start + count - 1 do
            rarr.[i] <- value

    /// Return a new Rarr with the elements in reverse order.
    //-[<CompiledName("Rev")>]
    let rev (rarr : Rarr<'T>) : Rarr<'T> =
        //+ checkNonNull "rarr" rarr
        let len = length rarr
        let result = Rarr (len)
        for i = len - 1 downto 0 do
            result.Add rarr.[i]
        result

    /// Read a range of elements from the first Rarr and write them into the second.
    //-[<CompiledName("Blit")>]
    let blit (source : Rarr<'T>) sourceIndex (target : Rarr<'T>) targetIndex count : unit =
        //+ checkNonNull "source" source
        //+ checkNonNull "target" target
        if sourceIndex < 0 then
            invalidArg "sourceIndex" "The source index cannot be negative."
        elif targetIndex < 0 then
            invalidArg "targetIndex" "The target index cannot be negative."
        elif count < 0 then
            invalidArg "count" "Cannot copy a negative number of items."
        elif sourceIndex + count > length source then
            invalidArg "sourceIndex" "There are fewer than 'count' elements between 'sourceIndex' and the end of the source Rarr."
        elif targetIndex + count > length target then
            invalidArg "sourceIndex" "There are fewer than 'count' elements between 'sourceIndex' and the end of the target Rarr."

        for i = 0 to count - 1 do
            target.[targetIndex + i] <- source.[sourceIndex + i]

    /// Combine the two Rarrs into a Rarr of pairs.
    /// The two arrays must have equal lengths, otherwise an <c>ArgumentException</c> is raised.
    //-[<CompiledName("Zip")>]
    let zip (rarr1 : Rarr<'T1>) (rarr2 : Rarr<'T2>)
        : Rarr<'T1 * 'T2> =
        //+ checkNonNull "rarr1" rarr1
        //+ checkNonNull "rarr2" rarr2
        let len = length rarr1
        if len <> length rarr2 then
            invalidArg "rarr2" "The Rarrs have different lengths."
        let results = Rarr (len)
        for i = 0 to len - 1 do
            results.Add (rarr1.[i], rarr2.[i])
        results

    /// Split a Rarr of pairs into two Rarrs.
    //-[<CompiledName("Unzip")>]
    let unzip (rarr : Rarr<'T1 * 'T2>) : Rarr<'T1> * Rarr<'T2> =
        //+ checkNonNull "rarr" rarr
        let len = length rarr
        let results1 = Rarr (len)
        let results2 = Rarr (len)
        for i = 0 to len - 1 do
            let x, y = rarr.[i]
            results1.Add x
            results2.Add y
        results1, results2

    /// Test if any element of the array satisfies the given predicate.
    /// If the input function is <c>f</c> and the elements are <c>i0...iN</c> 
    /// then computes <c>p i0 or ... or p iN</c>.
    //-[<CompiledName("Exists")>]
    let inline exists (predicate : 'T -> bool) (rarr : Rarr<'T>) : bool =
        //+ checkNonNull "rarr" rarr
        rarr.Exists (System.Predicate predicate)
        

    /// Test elements of the two arrays pairwise to see if any pair of element satisfies the given predicate.
    /// Raise ArgumentException if the arrays have different lengths.
    //-[<CompiledName("Exists2")>]
    let exists2 predicate (rarr1 : Rarr<'T1>) (rarr2 : Rarr<'T2>) : bool =
        //+ checkNonNull "rarr1" rarr1
        //+ checkNonNull "rarr2" rarr2
        let len = length rarr1
        if len <> length rarr2 then
            invalidArg "rarr2" "The Rarrs have different lengths."

        let predicate = FSharpFunc<_,_,_>.Adapt predicate
    
        let mutable index = 0
        let mutable foundMatch = false
        while index < len && not foundMatch do
            foundMatch <- predicate.Invoke (rarr1.[index], rarr2.[index])
            index <- index + 1
        foundMatch

    /// Test if all elements of the array satisfy the given predicate.
    /// If the input function is <c>f</c> and the elements are <c>i0...iN</c> and "j0...jN"
    /// then computes <c>p i0 && ... && p iN</c>.
    //-[<CompiledName("Forall")>]
    let inline forall (predicate : 'T -> bool) (rarr : Rarr<'T>) : bool =
        //+ checkNonNull "rarr" rarr
        rarr.TrueForAll (System.Predicate predicate)
       

    /// Test elements of the two arrays pairwise to see if all pairs of elements satisfy the given predicate.
    /// Raise ArgumentException if the arrays have different lengths.
    //-[<CompiledName("Forall2")>]
    let forall2 predicate (rarr1 : Rarr<'T1>) (rarr2 : Rarr<'T2>) : bool =
        //+ checkNonNull "rarr1" rarr1
        //+ checkNonNull "rarr2" rarr2
        let len = length rarr1
        if len <> length rarr2 then
            invalidArg "rarr2" "The Rarrs have different lengths."

        let predicate = FSharpFunc<_,_,_>.Adapt predicate
    
        let mutable index = 0
        let mutable allMatch = true
        while index < len && allMatch do
            allMatch <- predicate.Invoke (rarr1.[index], rarr2.[index])
            index <- index + 1
        allMatch

    /// Return a new collection containing only the elements of the collection
    /// for which the given predicate returns <c>true</c>.
    //-[<CompiledName("Filter")>]
    let inline filter (predicate : 'T -> bool) (rarr : Rarr<'T>) : Rarr<'T> =
        //+ checkNonNull "rarr" rarr
        rarr.FindAll (System.Predicate predicate)
       

    /// <summary>
    /// Apply the given function to each element of the array. Return
    /// the array comprised of the results "x" for each element where
    /// the function returns <c>Some(x)</c>.
    /// </summary>
    //-[<CompiledName("Choose")>]
    let choose (chooser : 'T -> 'U option) (rarr : Rarr<'T>) : Rarr<'U> =
        //+ checkNonNull "rarr" rarr        
        if isEmpty rarr then // OPTIMIZATION : If the input list is empty return immediately.
            Rarr ()
        else
            let result = Rarr ()
            let count = rarr.Count

            for i = 0 to count - 1 do
                match chooser rarr.[i] with
                | None -> ()
                | Some value ->
                    result.Add value
            result

    /// <summary>
    /// Return the first element for which the given function returns <c>true</c>.
    /// Return <c>None</c> if no such element exists.
    /// </summary>
    //-[<CompiledName("TryFind")>]
    let tryFind (predicate : 'T -> bool) (rarr : Rarr<'T>) : 'T option =
        //+ checkNonNull "rarr" rarr
        let elementIndex =            
            rarr.FindIndex (System.Predicate predicate)

        match elementIndex with
        | -1 ->
            None
        | index ->
            Some rarr.[index]

    /// <summary>
    /// Return the first element for which the given function returns <c>true</c>.
    /// Raise <c>KeyNotFoundException</c> if no such element exists.
    /// </summary>
    //-[<CompiledName("Find")>]
    let find (predicate : 'T -> bool) (rarr : Rarr<'T>) : 'T =
        //+ checkNonNull "rarr" rarr
        let elementIndex =  rarr.FindIndex (System.Predicate predicate) 
        match elementIndex with
        | -1 ->
            KeyNotFoundException.Raise "Rarr.find did not find for predicate  %A in Rarr of %d items %A" predicate rarr.Count rarr            
        | index ->
            rarr.[index]

    /// Return the index of the first element in the array
    /// that satisfies the given predicate.
    //-[<CompiledName("TryFindIndex")>]
    let tryFindIndex (predicate : 'T -> bool) (rarr : Rarr<'T>) : int option =
        //+ checkNonNull "rarr" rarr
        let elementIndex =   rarr.FindIndex (System.Predicate predicate) 
        match elementIndex with
        | -1 ->
            None
        | index ->
            Some index
        
    /// <summary>
    /// Return the index of the first element in the array
    /// that satisfies the given predicate. Raise <c>KeyNotFoundException</c> if 
    /// none of the elements satisfy the predicate.
    /// </summary>
    //-[<CompiledName("FindIndex")>]
    let findIndex (predicate : 'T -> bool) (rarr : Rarr<'T>) : int =
        //+ checkNonNull "rarr" rarr
        let elementIndex =     rarr.FindIndex (System.Predicate predicate)
        match elementIndex with
        | -1 ->
            KeyNotFoundException.Raise "Rarr.findIndex did not find for predicate %A in Rarr of %d items %A" predicate rarr.Count rarr   
        | index ->
            index

    /// Return the index of the first element in the array
    /// that satisfies the given predicate.
    //-[<CompiledName("TryFindIndexIndexed")>]
    let tryFindIndexi predicate (rarr : Rarr<'T>) : int option =
        //+ checkNonNull "rarr" rarr
        let predicate = FSharpFunc<_,_,_>.Adapt predicate
        let lastIndex = length rarr - 1
        let mutable index = -1
        let mutable foundMatch = false
        while index < lastIndex && not foundMatch do
            let i = index + 1
            index <- i
            foundMatch <- predicate.Invoke (i, rarr.[i])

        if foundMatch then
            Some index
        else None

    /// <summary>
    /// Return the index of the first element in the array
    /// that satisfies the given predicate. Raise <c>KeyNotFoundException</c> if 
    /// none of the elements satisfy the predicate.
    /// </summary>
    //-[<CompiledName("FindIndexIndexed")>]
    let findIndexi predicate (rarr : Rarr<'T>) : int =
        //+ checkNonNull "rarr" rarr
        match tryFindIndexi predicate rarr with
        | Some index ->
            index
        | None ->
            KeyNotFoundException.Raise "Rarr.findIndexi did not find for predicate %A in Rarr of %d items %A" predicate rarr.Count rarr   

    /// <summary>
    /// Applies the given function to successive elements, returning the first
    /// result where function returns <c>Some(x)</c> for some x. If the function
    /// never returns <c>Some(x)</c>, returns <c>None</c>.
    /// </summary>
    //-[<CompiledName("TryPick")>]
    let tryPick (picker : 'T -> 'U option) (rarr : Rarr<'T>) : 'U option =
        //+ checkNonNull "rarr" rarr
        let count = rarr.Count
        let mutable result = None
        let mutable index = 0

        while index < count && Option.isNone result do
            result <- picker rarr.[index]
            index <- index + 1
        result

    /// <summary>
    /// Applies the given function to successive elements, returning the first
    /// result where function returns <c>Some(x)</c> for some x. If the function
    /// never returns <c>Some(x)</c>, raises KeyNotFoundException.
    /// </summary>
    //-[<CompiledName("Pick")>]
    let pick (picker : 'T -> 'U option) (rarr : Rarr<'T>) : 'U =
        //+ checkNonNull "rarr" rarr
        let count = rarr.Count
        let mutable result = None
        let mutable index = 0

        while index < count && Option.isNone result do
            result <- picker rarr.[index]
            index <- index + 1

        match result with
        | Some result ->
            result
        | None ->
            KeyNotFoundException.Raise "Rarr.pick did not find for picker %A in Rarr of %d items %A" picker rarr.Count rarr   
            

    /// Apply the given function to each element of the array.
    //-[<CompiledName("Iterate")>]
    let iter (action : 'T -> unit) (rarr : Rarr<'T>) : unit =
        //+ checkNonNull "rarr" rarr

        let count = rarr.Count
        for i = 0 to count - 1 do
            action rarr.[i]

    /// Apply the given function to each element of the array. The integer passed to the
    /// function indicates the index of element.
    //-[<CompiledName("IterateIndexed")>]
    let iteri (action : int -> 'T -> unit) (rarr : Rarr<'T>) : unit =
        //+ checkNonNull "rarr" rarr
        let action = FSharpFunc<_,_,_>.Adapt action

        let count = rarr.Count
        for i = 0 to count - 1 do
            action.Invoke (i, rarr.[i])

    /// <summary>
    /// Apply the given function to two arrays simultaneously. The two arrays
    /// must have the same lengths, otherwise an <c>ArgumentException</c> is raised.
    /// </summary>
    //-[<CompiledName("Iterate2")>]
    let iter2 action (rarr1 : Rarr<'T1>) (rarr2 : Rarr<'T1>) : unit =
        //+ checkNonNull "rarr1" rarr1
        //+ checkNonNull "rarr2" rarr2
        let len = length rarr1
        if len <> length rarr2 then
            invalidArg "rarr2" "The Rarrs have different lengths."

        let action = FSharpFunc<_,_,_>.Adapt action

        for i = 0 to len - 1 do
            action.Invoke (rarr1.[i], rarr2.[i])

    /// <summary>
    /// Apply the given function to pair of elements drawn from matching indices in two arrays,
    /// also passing the index of the elements. The two arrays must have the same lengths, 
    /// otherwise an <c>ArgumentException</c> is raised.
    /// </summary>
    //-[<CompiledName("IterateIndexed2")>]
    let iteri2 action (rarr1 : Rarr<'T1>) (rarr2 : Rarr<'T2>) : unit =
        //+ checkNonNull "rarr1" rarr1
        //+ checkNonNull "rarr2" rarr2
        let len = length rarr1
        if len <> length rarr2 then
            invalidArg "rarr2" "The Rarrs have different lengths."

        let action = FSharpFunc<_,_,_,_>.Adapt action

        for i = 0 to len - 1 do
            action.Invoke (i, rarr1.[i], rarr2.[i])

    /// <summary>
    /// Build a new array whose elements are the results of applying the given function
    /// to each of the elements of the array.
    /// </summary>
    /// <param name="mapping"></param>
    /// <param name="rarr"></param>
    /// <returns></returns>
    //-[<CompiledName("Map")>]
    let inline map (mapping : 'T -> 'U) (rarr : Rarr<'T>) : Rarr<'U> =
        //+ checkNonNull "rarr" rarr
        rarr.ConvertAll (System.Converter mapping)
        

    /// <summary>
    /// Build a new array whose elements are the results of applying the given function
    /// to each of the elements of the array. The integer index passed to the
    /// function indicates the index of element being transformed.
    /// </summary>
    /// <param name="mapping"></param>
    /// <param name="rarr"></param>
    /// <returns></returns>
    //-[<CompiledName("MapIndexed")>]
    let mapi (mapping : int -> 'T -> 'U) (rarr : Rarr<'T>) : Rarr<'U> =
        //+ checkNonNull "rarr" rarr
        let mapping = FSharpFunc<_,_,_>.Adapt mapping
        let count = rarr.Count
        let result = Rarr (count)

        for i = 0 to count - 1 do
            result.Add <| mapping.Invoke (i, rarr.[i])
        result

    /// <summary>
    /// Build a new collection whose elements are the results of applying the given function
    /// to the corresponding elements of the two collections pairwise. The two input
    /// arrays must have the same lengths.
    /// </summary>
    /// <param name="mapping"></param>
    /// <param name="rarr1"></param>
    /// <param name="rarr2"></param>
    /// <returns></returns>
    //-[<CompiledName("Map2")>]
    let map2 mapping (rarr1 : Rarr<'T1>) (rarr2 : Rarr<'T2>) : Rarr<'U> =
        //+ checkNonNull "rarr1" rarr1
        //+ checkNonNull "rarr2" rarr2
        let len = length rarr1
        if len <> length rarr2 then
            invalidArg "rarr2" "The Rarrs have different lengths."

        let mapping = FSharpFunc<_,_,_>.Adapt mapping
        let results = Rarr (len)

        for i = 0 to len - 1 do
            mapping.Invoke (rarr1.[i], rarr2.[i])
            |> results.Add
        results

    /// <summary>
    /// Build a new collection whose elements are the results of applying the given function
    /// to the corresponding elements of the two collections pairwise. The two input
    /// arrays must have the same lengths, otherwise an <c>ArgumentException</c> is raised.
    /// </summary>
    /// <param name="mapping"></param>
    /// <param name="rarr1"></param>
    /// <param name="rarr2"></param>
    /// <returns></returns>
    //-[<CompiledName("MapIndexed2")>]
    let mapi2 mapping (rarr1 : Rarr<'T1>) (rarr2 : Rarr<'T2>) : Rarr<'U> =
        //+ checkNonNull "rarr1" rarr1
        //+ checkNonNull "rarr2" rarr2
        let len = length rarr1
        if len <> length rarr2 then
            invalidArg "rarr2" "The Rarrs have different lengths."

        let mapping = FSharpFunc<_,_,_,_>.Adapt mapping
        let results = Rarr (len)

        for i = 0 to len - 1 do
            mapping.Invoke (i, rarr1.[i], rarr2.[i])
            |> results.Add
        results

    /// <summary>
    /// Apply a function to each element of the collection, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> 
    /// then computes <c>f (... (f s i0)...) iN</c>.
    /// </summary>
    /// <param name="folder"></param>
    /// <param name="state"></param>
    /// <param name="rarr"></param>
    /// <returns></returns>
    //-[<CompiledName("Fold")>]
    let fold (folder : 'State -> 'T -> 'State) state (rarr : Rarr<'T>) : 'State =
        //+ checkNonNull "rarr" rarr
        let folder = FSharpFunc<_,_,_>.Adapt folder
        let mutable state = state
        let count = rarr.Count
        for i = 0 to count - 1 do
            state <- folder.Invoke (state, rarr.[i])
        state

    /// <summary>foldSub on just part of teh Rarr</summary>
    /// <param name="folder"></param>
    /// <param name="state"></param>
    /// <param name="rarr"></param>
    /// <param name="startIndex"></param>
    /// <param name="endIndex"></param>
    /// <returns></returns>
    //-[<CompiledName("FoldSub")>]
    let foldSub folder (state : 'State) (rarr : Rarr<'T>) startIndex endIndex : 'State =
        //+ checkNonNull "rarr" rarr
        if startIndex < 0 then
            IndexOutOfRangeException.Raise "The starting index cannot be negative. but is %d" startIndex
        elif endIndex > 0 then
            IndexOutOfRangeException.Raise "The ending index cannot be negative. but is %d" endIndex
    
        let len = length rarr
        if startIndex >= len then
            IndexOutOfRangeException.Raise  "The starting index is outside the bounds of the Rarr: %d of %d" startIndex len
        elif endIndex >= len then
            IndexOutOfRangeException.Raise  "The ending index is outside the bounds of the Rarr: %d of %d" endIndex len

        let folder = FSharpFunc<_,_,_>.Adapt folder

        // Fold over the specified range of items.
        let mutable state = state
        for i = startIndex to endIndex do
            state <- folder.Invoke (state, rarr.[i])
        state

    /// <summary>
    /// Applies a function to each element of the collection, threading an accumulator argument
    /// through the computation. The integer index passed to the function indicates the
    /// index of the element within the collection.
    /// </summary>
    /// <param name="folder"></param>
    /// <param name="state"></param>
    /// <param name="rarr"></param>
    /// <returns></returns>
    //-[<CompiledName("FoldIndexed")>]
    let foldi (folder : 'State -> int -> 'T -> 'State) state (rarr : Rarr<'T>) : 'State =
        //+ checkNonNull "rarr" rarr
        let folder = FSharpFunc<_,_,_,_>.Adapt folder

        let mutable state = state
        let count = rarr.Count
        for i = 0 to count - 1 do
            state <- folder.Invoke (state, i, rarr.[i])
        state

    /// <summary>
    /// Apply a function to pairs of elements drawn from the two collections, left-to-right,
    /// threading an accumulator argument through the computation.  The two input arrays must
    /// have the same lengths, otherwise an <c>ArgumentException</c> is raised.
    /// </summary>
    /// <param name="folder"></param>
    /// <param name="state"></param>
    /// <param name="rarr1"></param>
    /// <param name="rarr2"></param>
    /// <returns></returns>
    //-[<CompiledName("Fold2")>]
    let fold2 folder (state : 'State) (rarr1 : Rarr<'T1>) (rarr2 : Rarr<'T2>) : 'State =
        //+ checkNonNull "rarr1" rarr1
        //+ checkNonNull "rarr2" rarr2
        let len = length rarr1
        if len <> length rarr2 then
            invalidArg "rarr2" "The arrays have different lengths."

        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        let mutable state = state
        for i = 0 to len - 1 do
            state <- folder.Invoke (state, rarr1.[i], rarr2.[i])
        state

    /// <summary>
    /// Apply a function to each element of the array, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are
    /// <c>i0...iN</c> then computes <c>f i0 (...(f iN s))</c>.
    /// </summary>
    /// <param name="folder"></param>
    /// <param name="rarr"></param>
    /// <param name="state"></param>
    /// <returns></returns>
    //-[<CompiledName("FoldBack")>]
    let foldBack (folder : 'T -> 'State -> 'State) (rarr : Rarr<'T>) state : 'State =
        //+ checkNonNull "rarr" rarr
        let folder = FSharpFunc<_,_,_>.Adapt folder

        let mutable state = state
        for i = rarr.Count - 1 downto 0 do
            state <- folder.Invoke (rarr.[i], state)
        state

    /// <summary>foldBackSub</summary>
    /// <param name="folder"></param>
    /// <param name="rarr"></param>
    /// <param name="startIndex"></param>
    /// <param name="endIndex"></param>
    /// <param name="state"></param>
    /// <returns></returns>
    //-[<CompiledName("FoldBackSub")>]
    let foldBackSub folder (rarr : Rarr<'T>) startIndex endIndex (state : 'State) : 'State =
        //+ checkNonNull "rarr" rarr
        if startIndex < 0 then
            IndexOutOfRangeException.Raise "The starting index cannot be negative. but is %d" startIndex
        elif endIndex > 0 then
            IndexOutOfRangeException.Raise "The ending index cannot be negative. but is %d" endIndex
    
        let len = length rarr
        if startIndex >= len then
            IndexOutOfRangeException.Raise  "The starting index is outside the bounds of the Rarr: %d of %d" startIndex len
        elif endIndex >= len then
            IndexOutOfRangeException.Raise  "The ending index is outside the bounds of the Rarr: %d of %d" endIndex len

        let folder = FSharpFunc<_,_,_>.Adapt folder

        // Fold over the specified range of items.
        let mutable state = state
        for i = endIndex downto startIndex do
            state <- folder.Invoke (rarr.[i], state)
        state

    /// <summary>foldiBack</summary>
    /// <param name="folder"></param>
    /// <param name="rarr"></param>
    /// <param name="state"></param>
    /// <returns></returns>
    //-[<CompiledName("FoldBackIndexed")>]
    let foldiBack (folder : int -> 'T -> 'State -> 'State) (rarr : Rarr<'T>) state : 'State =
        //+ checkNonNull "rarr" rarr

        let folder = FSharpFunc<_,_,_,_>.Adapt folder

        let mutable state = state
        for i = rarr.Count - 1 downto 0 do
            state <- folder.Invoke (i, rarr.[i], state)
        state

    /// <summary>
    /// Apply a function to pairs of elements drawn from the two collections, right-to-left, 
    /// threading an accumulator argument through the computation.  The two input
    /// arrays must have the same lengths, otherwise an <c>ArgumentException</c> is raised.
    /// </summary>
    /// <param name="folder"></param>
    /// <param name="rarr1"></param>
    /// <param name="rarr2"></param>
    /// <param name="state"></param>
    /// <returns></returns>
    //-[<CompiledName("FoldBack2")>]
    let foldBack2 folder (rarr1 : Rarr<'T1>) (rarr2 : Rarr<'T2>) (state : 'State) : 'State =
        //+ checkNonNull "rarr1" rarr1
        //+ checkNonNull "rarr2" rarr2

        let len = length rarr1
        if len <> length rarr2 then
            invalidArg "rarr2" "The arrays have different lengths."

        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        let mutable state = state
        for i = len - 1 downto 0 do
            state <- folder.Invoke (rarr1.[i], rarr2.[i], state)
        state

    /// <summary>
    /// Apply a function to each element of the array, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> 
    /// then computes <c>f (... (f i0 i1)...) iN</c>.
    /// Raises <c>ArgumentException</c> if the array has size zero.
    /// <summary>
    /// <param name="reduction"></param>
    /// <param name="rarr"></param>
    /// <returns></returns>
    //-[<CompiledName("Reduce")>]
    let reduce (reduction : 'T -> 'T -> 'T) (rarr : Rarr<'T>) =
        //+ checkNonNull "rarr" rarr
        if isEmpty rarr then
            invalidArg "rarr" "The Rarr is empty."

        let reduction = FSharpFunc<_,_,_>.Adapt reduction

        let mutable state = rarr.[0]
        let count = rarr.Count
        for i = 1 to count - 1 do
            state <- reduction.Invoke (state, rarr.[i])
        state

    /// <summary>
    /// Apply a function to each element of the array, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> then 
    /// computes <c>f i0 (...(f iN-1 iN))</c>.
    /// Raises <c>ArgumentException</c> if the array has size zero.
    /// </summary>
    /// <param name="reduction"></param>
    /// <param name="rarr"></param>
    /// <returns></returns>
    //-[<CompiledName("ReduceBack")>]
    let reduceBack (reduction : 'T -> 'T -> 'T) (rarr : Rarr<'T>) : 'T =
        //+ checkNonNull "rarr" rarr
        if isEmpty rarr then
            invalidArg "rarr" "The Rarr is empty."

        let reduction = FSharpFunc<_,_,_>.Adapt reduction

        let count = rarr.Count
        let mutable state = rarr.[count - 1]

        for i = count - 2 downto 0 do
            state <- reduction.Invoke (rarr.[i], state)
        state

    /// <summary>scanSub</summary>
    /// <param name="folder"></param>
    /// <param name="state"></param>
    /// <param name="rarr"></param>
    /// <param name="startIndex"></param>
    /// <param name="endIndex"></param>
    /// <returns></returns>
    //-[<CompiledName("ScanSub")>]
    let scanSub folder (state : 'State) (rarr : Rarr<'T>) startIndex endIndex : Rarr<'State> =
        //+ checkNonNull "rarr" rarr
        if startIndex < 0 then
            IndexOutOfRangeException.Raise "The starting index cannot be negative. but is %d" startIndex
        elif endIndex > 0 then
            IndexOutOfRangeException.Raise "The ending index cannot be negative. but is %d" endIndex
    
        let len = length rarr
        if startIndex >= len then
            IndexOutOfRangeException.Raise  "The starting index is outside the bounds of the Rarr: %d of %d" startIndex len
        elif endIndex >= len then
            IndexOutOfRangeException.Raise  "The ending index is outside the bounds of the Rarr: %d of %d" endIndex len

        let folder = FSharpFunc<_,_,_>.Adapt folder

        // Holds the initial and intermediate state values.
        let results = Rarr (endIndex - startIndex + 2)
        results.Add state

        // Fold over the specified range of items.
        let mutable state = state
        for i = startIndex to endIndex do
            state <- folder.Invoke (state, rarr.[i])
            results.Add state
        results

    /// <summary>
    /// Like <c>fold</c>, but return the intermediary and final results.
    /// </summary>
    /// <param name="folder"></param>
    /// <param name="state"></param>
    /// <param name="rarr"></param>
    /// <returns></returns>
    //-[<CompiledName("Scan")>]
    let scan folder (state : 'State) (rarr : Rarr<'T>) : Rarr<'State> =
        //+ checkNonNull "rarr" rarr
        scanSub folder state rarr 0 (length rarr - 1)

    /// <summary>scanBackSub</summary>
    /// <param name="folder"></param>
    /// <param name="rarr"></param>
    /// <param name="startIndex"></param>
    /// <param name="endIndex"></param>
    /// <param name="state"></param>
    /// <returns></returns>
    //-[<CompiledName("ScanBackSub")>]
    let scanBackSub folder (rarr : Rarr<'T>) startIndex endIndex (state : 'State) : Rarr<'State> =
        //+ checkNonNull "rarr" rarr
        if startIndex < 0 then
            IndexOutOfRangeException.Raise "The starting index cannot be negative. but is %d" startIndex
        elif endIndex > 0 then
            IndexOutOfRangeException.Raise "The ending index cannot be negative. but is %d" endIndex
    
        let len = length rarr
        if startIndex >= len then
            IndexOutOfRangeException.Raise  "The starting index is outside the bounds of the Rarr: %d of %d" startIndex len
        elif endIndex >= len then
            IndexOutOfRangeException.Raise  "The ending index is outside the bounds of the Rarr: %d of %d" endIndex len

        let folder = FSharpFunc<_,_,_>.Adapt folder

        // Holds the initial and intermediate state values.
        let results = Rarr (endIndex - startIndex + 2)
        results.Add state

        // Fold over the specified range of items.
        let mutable state = state
        for i = endIndex downto startIndex do
            state <- folder.Invoke (rarr.[i], state)
            results.Insert (0, state)
        results

    /// <summary>
    /// Like <c>foldBack</c>, but return both the intermediary and final results.
    /// </summary>
    /// <param name="folder"></param>
    /// <param name="rarr"></param>
    /// <param name="state"></param>
    /// <returns></returns>
    //-[<CompiledName("ScanBack")>]
    let scanBack folder (rarr : Rarr<'T>) (state : 'State) : Rarr<'State> =
        //+ checkNonNull "rarr" rarr

        scanBackSub folder rarr 0 (length rarr - 1) state

    /// <summary>
    /// Split the collection into two collections, containing the elements for which
    /// the given predicate returns <c>true</c> and <c>false</c> respectively.
    /// </summary>
    /// <param name="predicate"></param>
    /// <param name="rarr"></param>
    /// <returns></returns>
    //-[<CompiledName("Partition")>]
    let partition predicate (rarr : Rarr<'T>) : Rarr<'T> * Rarr<'T> =
        //+ checkNonNull "rarr" rarr
        let trueResults = Rarr ()
        let falseResults = Rarr ()

        let len = length rarr
        for i = 0 to len - 1 do
            let el = rarr.[i]
            if predicate el then
                trueResults.Add el
            else
                falseResults.Add el

        trueResults, falseResults

    /// <summary>
    /// Splits the collection into two (2) collections, containing the elements for which the
    /// given function returns <c>Choice1Of2</c> or <c>Choice2Of2</c>, respectively. This function is similar to
    /// <c>Rarr.partition</c>, but it allows the returned collections to have different element types.
    /// </summary>
    /// <param name="partitioner"></param>
    /// <param name="rarr"></param>
    /// <returns></returns>
    //-[<CompiledName("MapPartition")>]
    let mapPartition partitioner (rarr : Rarr<'T>) : Rarr<'U1> * Rarr<'U2> =
        //+ checkNonNull "rarr" rarr

        let results1 = Rarr ()
        let results2 = Rarr ()

        let len = length rarr
        for i = 0 to len - 1 do
            match partitioner rarr.[i] with
            | Choice1Of2 value ->
                results1.Add value
            | Choice2Of2 value ->
                results2.Add value

        results1, results2

    /// <summary>Returns the sum of the elements in the Rarr.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The resulting sum.</returns>
    //-[<CompiledName("Sum")>]
    let inline sum (rarr : Rarr< ^T>) : ^T = 
        //+ checkNonNull "rarr" rarr

        let mutable acc = LanguagePrimitives.GenericZero< (^T) >
    //    for x in rarr do
    //        acc <- Checked.(+) acc x
        for i = 0 to rarr.Count - 1 do
            acc <- Checked.(+) acc rarr.[i]
        acc

    /// <summary>Returns the sum of the results generated by applying the function to each element of the Rarr.</summary>
    /// <param name="projection">The function to transform the Rarr elements into the type to be summed.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The resulting sum.</returns>
    //-[<CompiledName("SumBy")>]
    let inline sumBy (projection : 'T -> ^U) (rarr : Rarr<'T>) : ^U = 
        //+ checkNonNull "rarr" rarr

        let mutable acc = LanguagePrimitives.GenericZero< (^U) >
    //    for x in rarr do
    //        acc <- Checked.(+) acc (projection x)
        for i = 0 to rarr.Count - 1 do
            acc <- Checked.(+) acc (projection rarr.[i])
        acc

    /// <summary>Returns the lowest of all elements of the Rarr, compared via Operators.min.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The minimum element.</returns>
    /// <exception cref="System.ArgumentException">Thrown when <paramref name="rarr"/> is empty.</exception>
    //-[<CompiledName("Min")>]
    let inline min (rarr : Rarr<'T>) =
        //+ checkNonNull "rarr" rarr
        if rarr.Count = 0 then
            invalidArg "rarr" "The input collection is empty."

        let mutable acc = rarr.[0]
        for i = 1 to rarr.Count - 1 do
            let curr = rarr.[i]
            if curr < acc then
                acc <- curr
        acc

    /// <summary>Returns the lowest of all elements of the Rarr, compared via Operators.min on the function result.</summary>
    /// <param name="projection">The function to transform the elements into a type supporting comparison.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The minimum element.</returns>
    /// <exception cref="System.ArgumentException">Thrown when <paramref name="rarr"/> is empty.</exception>
    //-[<CompiledName("MinBy")>]
    let inline minBy (projection : 'T -> 'U) (rarr : Rarr<'T>) =
        //+ checkNonNull "rarr" rarr
        if rarr.Count = 0 then
            invalidArg "rarr" "The input collection is empty."

        let mutable accv = rarr.[0]
        let mutable acc = projection accv
        for i = 1 to rarr.Count - 1 do
            let currv = rarr.[i]
            let curr = projection currv
            if curr < acc then
                acc <- curr
                accv <- currv
        accv

    /// <summary>Returns the greatest of all elements of the Rarr, compared via Operators.max on the function result.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The maximum element.</returns>
    /// <exception cref="System.ArgumentException">Thrown when <paramref name="rarr"/> is empty.</exception>
    //-[<CompiledName("Max")>]
    let inline max (rarr : Rarr<'T>) =
        //+ checkNonNull "rarr" rarr
        if rarr.Count = 0 then
            invalidArg "rarr" "The input collection is empty."

        let mutable acc = rarr.[0]
        for i = 1 to rarr.Count - 1 do
            let curr = rarr.[i]
            if curr > acc then
                acc <- curr
        acc

    /// <summary>Returns the greatest of all elements of the Rarr, compared via Operators.max on the function result.</summary>
    /// <param name="projection">The function to transform the elements into a type supporting comparison.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The maximum element.</returns>
    /// <exception cref="System.ArgumentException">Thrown when <paramref name="rarr"/> is empty.</exception>
    //-[<CompiledName("MaxBy")>]
    let inline maxBy (projection : 'T -> 'U) (rarr : Rarr<'T>) =
        //+ checkNonNull "rarr" rarr
        if rarr.Count = 0 then
            invalidArg "rarr" "The input collection is empty."
        let mutable accv = rarr.[0]
        let mutable acc = projection accv
        for i = 1 to rarr.Count - 1 do
            let currv = rarr.[i]
            let curr = projection currv
            if curr > acc then
                acc <- curr
                accv <- currv
        accv

    /// <summary>Returns the average of the elements in the Rarr.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The average of the elements in the Rarr.</returns>
    /// <exception cref="System.ArgumentException">Thrown when <paramref name="rarr"/> is empty.</exception>
    //-[<CompiledName("Average")>]
    let inline average (rarr : Rarr<'T>) : ^T =
        //+ checkNonNull "rarr" rarr
        Seq.average rarr

    /// <summary>Returns the average of the elements generated by applying the function to each element of the Rarr.</summary>
    /// <param name="projection">The function to transform the Rarr elements before averaging.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The computed average.</returns>
    /// <exception cref="System.ArgumentException">Thrown when <paramref name="rarr"/> is empty.</exception>
    //-[<CompiledName("AverageBy")>]
    let inline averageBy (projection : 'T -> ^U) (rarr : Rarr<'T>) : ^U = 
        //+ checkNonNull "rarr" rarr
        Seq.averageBy projection rarr
