namespace FsEx

open System
open System.Collections.Generic
open NiceString
//open Microsoft.FSharp.Core
//open Microsoft.FSharp.Collections
//open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators

#nowarn "44" // to disable the obsolete warning on accessing Rarr.List

/// Generic operations on Rarr which is like a System.Collections.Generic.List<'T> but with nicer error messages.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] //need this so doesn't hide Rarr class in C# assemblies (should consider for other extension modules as well)
module Rarr = 

    //---------------------------------------------------
    //        extensions added only in FsEx: (not in FSharp.Core Array module)
    //----------------------------------------------------

    /// Gets an item in the Rarr by index.
    /// Allows for negative index too ( -1 is last item,  like Python)
    /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
    let inline getNeg index  (rarr: Rarr<'T>)= 
        let len = rarr.Count
        let ii =  if index < 0 then len + index else index
        if ii<0 || ii >= len then ArgumentException.RaiseBase "Rarr.GetNeg: Failed to get index %d from Rarr of %d items: %s" index rarr.Count rarr.ToNiceStringLong
        rarr.List.[ii] // access List directly to not check index twice

    /// Sets an item in the Rarr by index.
    /// Allows for negative index too ( -1 is last item,  like Python)
    /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
    let inline setNeg index value (rarr: Rarr<'T>)= 
        let len = rarr.Count
        let ii =  if index < 0 then len + index else index
        if ii<0 || ii >= len then ArgumentException.RaiseBase "Rarr.SetNeg: Failed to set index %d to %s from Rarr of %d items: %s" index (toNiceString value) rarr.Count rarr.ToNiceStringLong
        rarr.List.[ii] <- value     // access List directly to not check index twice

    /// Any index will return a value.
    /// Rarr is treated as an endless loop in positive and negative direction
    let inline getLooped index  (rarr: Rarr<'T>)= 
        let len = rarr.Count
        if len=0 then ArgumentException.RaiseBase "Rarr.GetLooped: Failed to get index %d from Rarr of 0 items" index
        let t = index % len
        let ii = if t >= 0 then t  else t + len
        rarr.List.[ii]   // access List directly to not check index twice

    /// Any index will set a value.
    /// Rarr is treated as an endless loop in positive and negative direction
    let inline setLooped index value  (rarr: Rarr<'T>) = 
        let len = rarr.Count
        if len=0 then ArgumentException.RaiseBase "Rarr.SetLooped: Failed to Set index %d to %s in Rarr of 0 items" index (toNiceString value)
        let t = index % len
        let ii = if t >= 0 then t  else t + len
        rarr.List.[ii] <- value // access List directly to not check index twice

    /// Get and remove last item from Rarr
    let inline pop  (rarr: Rarr<'T>)  = 
        if rarr.Count=0 then ArgumentException.RaiseBase "Failed to pop from %s" rarr.ToNiceStringLong
        let i = rarr.Count - 1
        let v = rarr.List.[i] // access List directly to not check index twice
        rarr.List.RemoveAt(i)
        v

    /// Gets the second last item in the Rarr.
    /// Same as  this.[this.Count - 2]
    let inline secondLast  (rarr: Rarr<'T>)= 
        if rarr.Count < 2 then  IndexOutOfRangeException.Raise "Rarr.secondLast: Failed to get second last item of %s" rarr.ToNiceStringLong
        rarr.List.[rarr.Count - 2]

    /// Gets the third last item in the Rarr.
    /// Same as   this.[this.Count - 3]
    let inline thirdLast  (rarr: Rarr<'T>)= 
        if rarr.Count < 3 then  IndexOutOfRangeException.Raise "Rarr.thirdLast: Failed to get third last item of %s" rarr.ToNiceStringLong
        rarr.List.[rarr.Count - 3]

    /// Gets the first item in the Rarr.
    /// Same as   this.[0]
    let inline first  (rarr: Rarr<'T>)= 
        if rarr.Count = 0 then IndexOutOfRangeException.Raise "Rarr.first: Failed to get first item of %s" rarr.ToNiceStringLong
        rarr.List.[0]

    /// Gets the second item in the Rarr.
    /// Same as   this.[1]
    let inline second  (rarr: Rarr<'T>)= 
        if rarr.Count < 2 then IndexOutOfRangeException.Raise  "Rarr.second: Failed to get second item of %s" rarr.ToNiceStringLong
        rarr.List.[1]

    /// Gets the third item in the Rarr.
    /// Same as   this.[2]
    let inline third  (rarr: Rarr<'T>)= 
        if rarr.Count < 3 then IndexOutOfRangeException.Raise "Rarr.third: Failed to get third item of %s" rarr.ToNiceStringLong
        rarr.List.[2]

    /// Allows for negative indices too. ( -1 is last item, like Python)
    /// The resulting Rarr includes the end index.
    /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
    let inline slice startIdx endIdx (rarr: Rarr<_>)  = 
        rarr.GetSlice(startIdx, endIdx)


    (*

            implement those with cashing to avoid repeated index lookup, see Seq module

            /// Yields Seq from (first, second)  up to (second-last, last)
            /// not looped
            /// the resulting seq is one element shorter than the input Rarr
            let windowed2 (a:Rarr<'T>) = 
                checkCount 2 "windowed2" a
                seq {   for i = 0 to a.Count-2 do  yield a.[i], a.[i+1] }

            /// Yields looped Seq from (first, second)  up to (last, first)
            /// the resulting seq has the same element count as the input Rarr
            let thisNext (a:Rarr<'T>) = 
                checkCount 2 "thisNext" a
                seq {   for i = 0 to a.Count-2 do yield a.[i], a.[i+1]
                        yield a.[a.Count-1], a.[0] }

            /// Yields looped Seq from (last,first)  up to (second-last, last)
            /// the resulting seq has the same element count as the input Rarr
            let prevThis (a:Rarr<'T>) = 
                checkCount 2 "prevThis" a
                seq {   yield a.[a.Count-1], a.[0]
                        for i = 0 to a.Count-2 do yield a.[i], a.[i+1] }

            /// Yields Seq from (first, second, third)  up to (third-last, second-last, last)
            /// not looped
            /// the resulting seq is two elements shorter than the input Rarr
            let windowed3 (a:Rarr<'T>) = 
                checkCount 3 "windowed3" a
                seq {   for i = 0 to a.Count-3 do yield a.[i], a.[i+1], a.[i+2] }

            /// Yields looped Seq of  from (last, first, second)  up to (second-last, last, first)
            /// the resulting seq has the same element count as the input Rarr
            let prevThisNext (a:Rarr<'T>) = 
                checkCount 3 "prevThisNext" a
                seq{    yield  a.[a.Count-1], a.[0], a.[1]
                        for i = 0 to a.Count-3 do yield a.[i], a.[i+1], a.[i+2]
                        yield  a.[a.Count-2],a.[a.Count-1], a.[0] }

            /// Yields Seq from (0,first, second)  up to (lastIndex-1 , second-last, last)
            /// not looped
            /// the resulting seq is one element shorter than the input Rarr
            let windowed2i (a:Rarr<'T>) = 
                checkCount 2 "windowed2i" a
                seq {   for i = 0 to a.Count-2 do yield i, a.[i], a.[i+1] }

            /// Yields looped Seq  from (0,first, second)  up to (lastIndex, last, first)
            /// the resulting seq has the same element count as the input Rarr
            let iThisNext (a:Rarr<'T>) = 
                checkCount 2 "iThisNext" a
                seq {   for i = 0 to a.Count-2 do yield i, a.[i], a.[i+1]
                        yield  a.Count-1, a.[a.Count-1], a.[0] }

            /// Yields Seq from (1, first, second, third)  up to (lastIndex-1 , third-last, second-last, last)
            /// not looped
            /// the resulting seq is two elements shorter than the input Rarr
            let windowed3i (a:Rarr<'T>) = 
                checkCount 3 "windowed3i" a
                seq {   for i = 0 to a.Count-3 do yield i+1, a.[i], a.[i+1], a.[i+2] }

            /// Yields looped Seq from (1, last, first, second)  up to (lastIndex, second-last, last, first)
            /// the resulting seq has the same element count as the input Rarr
            let iPrevThisNext (a:Rarr<'T>) = 
                checkCount 3 "iPrevThisNext" a
                seq {   yield  0, a.[a.Count-1], a.[0], a.[1]
                        for i = 0 to a.Count-3 do yield i+1, a.[i], a.[i+1], a.[i+2]
                        yield  a.Count-1, a.[a.Count-2],a.[a.Count-1], a.[0] }

            *)


    /// Returns a Rarr containing just one element
    let singelton  (element: 'T) : Rarr<'T> = 
        let r = Rarr(1)
        r.List.Add element
        r

    /// <summary>Considers List circular and move elements up or down
    /// e.g.: rotate +1 [ a, b, c, d] = [ d, a, b, c]
    /// e.g.: rotate -1 [ a, b, c, d] = [ b, c, d, a] </summary>
    /// <param name="amount">How many elements to shift forward. Or backward if number is negative</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The new result Rarr.</returns>
    let inline rotate amount (rarr: Rarr<'T>) :  Rarr<'T>  = 
        let r = Rarr(rarr.Count)
        let li = rarr.List
        for i = 0 to rarr.Count-1 do
            r.Add <| li.[negIdxLooped (i-amount) rarr.Count]
        r

    /// Structural equality.
    /// Compares each element in both lists for equality . Rarrs must also be of same Count
    let equals (rarr1: Rarr<'T>) (rarr2: Rarr<'T>) :bool = 
        rarr1.IsEqualTo(rarr2)

    /// Returns true if the given Rarr has just one item.
    /// Same as  Rarr.hasOne
    let inline isSingelton (rarr : Rarr<'T>) : bool = 
        rarr.Count = 1

    /// Returns true if the given Rarr has just one item.
    /// Same as  Rarr.isSingelton
    let inline hasOne (rarr : Rarr<'T>) : bool = 
        rarr.Count = 1

    /// Returns true if the given Rarr is not empty.
    let inline isNotEmpty (rarr : Rarr<'T>) : bool = 
        rarr.Count <> 0

    /// Returns true if the given Rarr has count items.
    let inline hasItems count (rarr : Rarr<'T>) : bool = 
        rarr.Count = count

    /// Returns true if the given Rarr has equal or more than count items.
    let inline hasMinimumItems count (rarr : Rarr<'T>) : bool = 
        rarr.Count >= count

    /// Returns true if the given Rarr has equal or less than count items.
    let inline hasMaximumItems count (rarr : Rarr<'T>) : bool = 
        rarr.Count <= count

    /// Swap the values of two given indices in Rarr
    let inline swap i j (xs:Rarr<'T>) : unit = 
        if i < 0 then IndexOutOfRangeException.Raise "Rarr.swap: index i can't be less than 0: %d (j: %d)" i j
        if i >= xs.Count then IndexOutOfRangeException.Raise "Rarr.swap: index i can't be bigger than %d but is %d (j: %d)" (xs.Count-1) i j
        if i<>j then
            if j < 0 then IndexOutOfRangeException.Raise "Rarr.swap: index j can't be less than 0: %d (i: %d)" j i
            if j >= xs.Count then IndexOutOfRangeException.Raise "Rarr.swap: index j can't be bigger than %d but is %d (i: %d)" (xs.Count-1) j i
            // operate on underlaying list since indices are checked
            let ti = xs.List.[i]
            xs.List.[i] <- xs.List.[j]
            xs.List.[j] <- ti


    /// internal only for finding
    module private MinMax = 
        //TODO test keeping of order if equal !

        let inline simple cmpF (xs:Rarr<'T>) = 
            if xs.Count < 1 then ArgumentException.RaiseBase "Rarr.MinMax.simple: Count must be at least one: %s"  xs.ToNiceStringLong
            let mutable m = xs.[0]
            for i=1 to xs.Count-1 do
                if cmpF xs.List.[i] m then m <- xs.List.[i]
            m

        let inline simple2 cmpF (xs:Rarr<'T>) = 
            if xs.Count < 2 then ArgumentException.RaiseBase "Rarr.MinMax.simple2: Count must be at least two: %s"  xs.ToNiceStringLong
            let mutable m1 = xs.List.[0]
            let mutable m2 = xs.List.[1]
            for i=1 to xs.Count-1 do
                let this = xs.List.[i]
                if cmpF this m1 then
                    m2 <- m1
                    m1 <- this
                elif cmpF this m2 then
                    m2 <- this
            m1,m2


        /// If any are equal then the  order is kept by using ( a=b || ) since the compare operate does not include the equal test
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


        /// If any are equal then the  order is kept by using ( a=b || ) since the compare operate does not include the equal test
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
            if xs.Count < 3 then ArgumentException.RaiseBase "Rarr.MinMax.simple3: Count must be at least three: %s"  xs.ToNiceStringLong
            let e1 = xs.List.[0]
            let e2 = xs.List.[1]
            let e3 = xs.List.[2]
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
            if xs.Count < 1 then ArgumentException.RaiseBase "Rarr.MinMax.indexByFun: Count must be at least one: %s"  xs.ToNiceStringLong
            let mutable f = func xs.List.[0]
            let mutable mf = f
            let mutable ii = 0
            let li = xs.List
            for i=1 to xs.Count-1 do
                f <- func li.[i]
                if cmpF f mf then
                    ii <- i
                    mf <- f
            ii

        let inline index2ByFun cmpF func (xs:Rarr<'T>) = 
            if xs.Count < 2 then ArgumentException.RaiseBase "Rarr.MinMax.index2ByFun: Count must be at least two: %s"  xs.ToNiceStringLong
            let mutable i1 = 0
            let mutable i2 = 1
            let mutable mf1 = func xs.List.[i1]
            let mutable mf2 = func xs.List.[i2]
            let mutable f = mf1 // placeholder
            let li = xs.List
            for i=1 to xs.Count-1 do
                f <- func li.[i]
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
            if xs.Count < 3 then ArgumentException.RaiseBase "Rarr.MinMax.index3ByFun: Count must be at least three: %s"  xs.ToNiceStringLong
            // sort first 3
            let mutable i1,i2,i3 =  indexOfSort3By byFun cmpOp xs.[0] xs.[1] xs.[2] // otherwise would fail on sorting first 3, test on Rarr([5;6;3;1;2;0])|> Rarr.max3
            let mutable e1 =  byFun xs.List.[i1]
            let mutable e2 =  byFun xs.List.[i2]
            let mutable e3 =  byFun xs.List.[i3]
            let mutable f = e1 // placeholder
            let li = xs.List
            for i=3 to xs.Count-1 do
                f <- byFun li.[i]
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


    (* covered by part copied from Array module

        // Returns the smallest element of the Rarr.
        let min rarr =     rarr |> MinMax.simple (<)

        // Returns the biggest element of the Rarr.
        let max rarr =     rarr |> MinMax.simple (>)

        // Returns the smallest element of the Rarr.
        // Elements are compared by applying the predicate function first.
        let minBy f rarr = let i = rarr |> MinMax.indexByFun (<) f in rarr.List.[i]

        // Returns the biggest element of the Rarr.
        // Elements are compared by applying the predicate function first.
        let maxBy f rarr = let i = rarr |> MinMax.indexByFun (>) f in rarr.List.[i]
        *)

    /// <summary>Returns the index of the smallest of all elements of the Rarr, compared via Operators.max on the function result.</summary>
    /// <param name="projection">The function to transform the elements into a type supporting comparison.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarr is empty.</exception>
    /// <returns>The index of the smallest element.</returns>
    let inline minIndBy  (projection : 'T -> 'Key) (rarr: Rarr<'T>) : int = 
        rarr |> MinMax.indexByFun (<) projection
   
    /// <summary>Returns the index of the greatest of all elements of the Rarr, compared via Operators.max on the function result.</summary>
    /// <param name="projection">The function to transform the elements into a type supporting comparison.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarr is empty.</exception>
    /// <returns>The index of the maximum element.</returns>
    let inline maxIndBy (projection : 'T -> 'Key) (rarr: Rarr<'T>) : int =
        rarr |> MinMax.indexByFun (>) projection

    /// Returns the smallest two elements of the Rarr.
    /// If they are equal then the  order is kept
    let inline min2 rarr =     rarr |> MinMax.simple2 (<)

    /// Returns the biggest two elements of the Rarr.
    /// If they are equal then the  order is kept
    let inline max2 rarr =     rarr |> MinMax.simple2 (>)

    // TODO make consistent xml docstring on below functions:

    /// Returns the smallest two elements of the Rarr.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the order is kept
    let inline min2By f rarr = 
        let i,ii = rarr |> MinMax.index2ByFun (<) f
        rarr.List.[i],rarr.List.[ii]

    /// Returns the biggest two elements of the Rarr.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the order is kept
    let inline max2By f rarr = 
        let i,ii = rarr |> MinMax.index2ByFun (>) f
        rarr.List.[i],
        rarr.List.[ii]

    /// Returns the indices of the two smallest elements of the Rarr.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the order is kept
    let inline min2IndBy f rarr = rarr |> MinMax.index2ByFun (<) f

    /// Returns the indices of the two biggest elements of the Rarr.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the order is kept
    let inline max2IndBy f rarr = rarr |> MinMax.index2ByFun (>) f

    /// Returns the smallest three elements of the Rarr.
    /// If they are equal then the  order is kept
    let inline min3 rarr =  rarr |> MinMax.simple3 (<)

    /// Returns the biggest three elements of the Rarr.
    /// If they are equal then the  order is kept
    let inline max3 rarr =  rarr |> MinMax.simple3 (>)

    /// Returns the smallest three elements of the Rarr.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the order is kept
    let inline min3By f rarr = 
        let i,ii,iii = rarr |> MinMax.index3ByFun (<) f
        rarr.List.[i],
        rarr.List.[ii],
        rarr.List.[iii]

    /// Returns the biggest three elements of the Rarr.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the order is kept
    let inline max3By f rarr = 
        let i,ii,iii = rarr |> MinMax.index3ByFun (>) f
        rarr.List.[i],
        rarr.List.[ii],
        rarr.List.[iii]

    /// Returns the indices of the three smallest elements of the Rarr.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the order is kept
    let inline min3IndBy f rarr = rarr |> MinMax.index3ByFun(<) f

    /// Returns the indices of the three biggest elements of the Rarr.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the order is kept
    let inline max3IndBy f rarr = rarr |> MinMax.index3ByFun (>) f

    /// Return the length or count of the collection.
    /// Same as  Rarr.length
    let inline count (rarr : Rarr<'T>) : int = 
        rarr.Count

    /// Counts for how many items of the collection the predicate returns true.
    /// Same as Rarr.filter and then Rarr.length
    let inline countIf (predicate : 'T -> bool) (rarr : Rarr<'T>) : int = //countBy is something else !!
        let mutable k = 0
        let li = rarr.List
        for i=0 to rarr.Count - 1 do
            if predicate li.[i] then
                k <- k + 1
        k

    /// Adds an object to the end of the Rarr.
    let inline add item (rarr : Rarr<'T>) : unit = 
        rarr.Add item

    /// Build a Rarr from the given Array.
    let inline ofArray (arr : 'T[]) : Rarr<'T> = 
        Rarr(arr)

    /// Return a fixed-length Array containing the elements of the input Rarr.
    let inline toArray (rarr : Rarr<'T>) : 'T[] = 
        rarr.ToArray ()

    /// <summary>
    /// Splits the collection into two (2) collections, containing the elements for which the
    /// given function returns <c>Choice1Of2</c> or <c>Choice2Of2</c>, respectively. This function is similar to
    /// <c>Rarr.partition</c>, but it allows the returned collections to have different element types.</summary>
    let inline mapPartition partitioner (rarr : Rarr<'T>) : Rarr<'U1> * Rarr<'U2> = 
        let results1 = Rarr ()
        let results2 = Rarr ()
        let len = rarr.Count
        let li = rarr.List
        for i = 0 to len - 1 do
            match partitioner li.[i] with
            | Choice1Of2 value ->
                results1.Add value
            | Choice2Of2 value ->
                results2.Add value
        results1, results2

    /// Applies a function to List
    /// If resulting List meets the resultPredicate it is returned , otherwise  original input is returned.
    let inline applyIfResult (resultPredicate:Rarr<'T> -> bool) (transform:Rarr<'T> -> Rarr<'T>)  (rarr: Rarr<'T>) : Rarr<'T> = 
        let r = transform rarr
        if resultPredicate r then r
        else rarr

    /// Applies a function to List if it meets the inputPredicate, otherwise just returns input.
    /// If resulting List meets the resultPredicate it is returned , otherwise original input is returned.
    let inline applyIfInputAndResult (inputPredicate:Rarr<'T> -> bool) (resultPredicate:Rarr<'T> -> bool) (transform:Rarr<'T> -> Rarr<'T>)  (rarr: Rarr<'T>) : Rarr<'T> = 
        if inputPredicate rarr then
            let r = transform rarr
            if resultPredicate r then r
            else rarr
        else
            rarr


    //--------------------------------------------------------------------------------------------------------------------
    // ------------- implementation adapted form FSharp.Core Array module: ------------------------------------------------
    //
    //               alternatives:
    //               https://github.com/fsprojects/FSharpx.Collections/blob/master/src/FSharpx.Collections/ResizeArray.fs
    //               https://github.com/jack-pappas/ExtCore/blob/master/ExtCore/Collections.Rarr.fs
    //               https://github.com/dotnet/fsharp/tree/master/src/utils
    //---------------------------------------------------------------------------------------------------------------------

    /// a StructBox for keys in case the key type is itself a type using null as a representation
    [<Struct; NoComparison; NoEquality>]
    type private StructBox<'T when 'T:equality>(value:'T) = // from fsharp/FSharp.Core/seqcore.fs
        member x.Value = value
        static member Comparer = 
            let gcomparer = HashIdentity.Structural<'T>
            { new IEqualityComparer<StructBox<'T>> with
                   member _.GetHashCode(v) = gcomparer.GetHashCode(v.Value)
                   member _.Equals(a,b)    = gcomparer.Equals(a.Value, b.Value) }

    /// <summary>Returns a new Rarr that contains all pairings (or combinations)  of elements from the first and second Rarrs.</summary>
    /// <param name="rarr1">The first input Rarr.</param>
    /// <param name="rarr2">The second input Rarr.</param>
    /// <returns>The resulting Rarr of pairs of length: rarr1.Count * rarr2.Count.</returns>
    let allPairs (rarr1: Rarr<'T>) (rarr2: Rarr<'U>) :Rarr<'T*'U> = 
        let res = Rarr(rarr1.Count * rarr2.Count)
        let l1 = rarr1.List
        let l2 = rarr2.List
        for i = 0 to rarr1.Count-1 do
            for j = 0 to rarr2.Count-1 do
                res.Add (l1.[i], l2.[j])
        res


    /// <summary>Builds a new Rarr that contains the elements of the first Rarr followed by the elements of the second Rarr.</summary>
    /// <param name="rarr1">The first input Rarr.</param>
    /// <param name="rarr2">The second input Rarr.</param>
    /// <returns>The resulting Rarr of length: rarr1.Count + rarr2.Count..</returns>
    let inline append (rarr1: Rarr<'T>) (rarr2: Rarr<'T>) = 
        let res = rarr1.Clone()
        res.AddRange(rarr2)
        res


    /// <summary>Returns the average of the elements in the Rarr.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when <c>Rarr</c> is empty.</exception>
    /// <returns>The average of the elements in the Rarr.</returns>
    let inline average (rarr: Rarr<'T>) = 
        if rarr.Count = 0 then ArgumentException.RaiseBase "Rarr.average: Count must be at least one: %s" rarr.ToNiceStringLong
        let mutable acc = LanguagePrimitives.GenericZero< ^T>
        let li = rarr.List
        for i = 0 to rarr.Count - 1 do
            acc <- Checked.(+) acc li.[i]
        LanguagePrimitives.DivideByInt< ^T> acc rarr.Count


    /// <summary>Returns the average of the elements generated by applying the function to each element of the Rarr.</summary>
    /// <param name="projection">The function to transform the Rarr elements before averaging.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when <c>Rarr</c> is empty.</exception>
    /// <returns>The computed average.</returns>
    let inline averageBy (projection: 'T -> ^Key) (rarr: Rarr<'T>) : ^Key = 
        if rarr.Count = 0 then ArgumentException.RaiseBase "Rarr.averageBy: Count must be at least one: %s" rarr.ToNiceStringLong
        let mutable acc = LanguagePrimitives.GenericZero< ^Key>
        let li = rarr.List
        for i = 0 to rarr.Count - 1 do
            acc <- Checked.(+) acc (projection li.[i])
        LanguagePrimitives.DivideByInt< ^Key> acc rarr.Count

    /// <summary>Applies the given function to each element of the Rarr. Returns
    /// the Rarr comprised of the results "x" for each element where
    /// the function returns Some(x)</summary>
    /// <param name="chooser">The function to generate options from the elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The Rarr of results.</returns>
    let inline choose (chooser : 'T -> 'U option) (rarr : Rarr<'T>) : Rarr<'U> = 
        let result = Rarr ()
        let count = rarr.Count
        let li = rarr.List
        for i = 0 to count - 1 do
            match chooser li.[i] with
            | None -> ()
            | Some value ->
                result.Add value
        result

    /// <summary>Divides the input Rarr into chunks of size at most <c>chunkSize</c>.</summary>
    /// <param name="chunkSize">The maximum size of each chunk.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The Rarr divided into chunks.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when <c>chunkSize</c> is not positive.</exception>
    let chunkBySize chunkSize (rarr: Rarr<'T>) : Rarr<Rarr<'T>> = 
        if chunkSize <= 0 then ArgumentException.RaiseBase "Rarr.chunkBySize: chunkSize %d must be bigger than 0" chunkSize
        let len = rarr.Count
        if len = 0 then
            Rarr(0)
        elif chunkSize > len then
            Rarr([rarr.Clone()])
        else
            let chunkCount = (len - 1) / chunkSize + 1
            let res = Rarr(chunkCount)
            let mutable sub = Rarr(0)
            let li = rarr.List
            for i=0 to rarr.Count-1 do
                if i % chunkSize = 0 then
                    sub <- Rarr(chunkSize)
                    res.Add(sub)
                sub.Add li.[i]
            res

    /// <summary>For each element of the Rarr, applies the given function. Concatenates all the results and return the combined Rarr.</summary>
    /// <param name="mapping">The function to create sub-Rarrs from the input Rarr elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The concatenation of the sub-Rarrs.</returns>
    let collect (mapping: 'T -> Rarr<'U>)  (rarr: Rarr<'T>) : Rarr<'U> = 
        //let collect (mapping: 'T -> seq<'U>)  (rarr: Rarr<'T>) : Rarr<'U> = // tests don't pass like that
        let res = Rarr(rarr.Count)
        for e in rarr do
            res.AddRange(mapping e)
        res

    /// <summary>Compares two Rarrs using the given comparison function, element by element.</summary>
    /// <param name="comparer">A function that takes an element from each Rarr and returns an int.
    /// If it evaluates to a non-zero value iteration is stopped and that value is returned.</param>
    /// <param name="rarr1">The first input Rarr.</param>
    /// <param name="rarr2">The second input Rarr.</param>
    /// <returns>Returns the first non-zero result from the comparison function. If the first Rarr has
    /// a larger element, the return value is always positive. If the second Rarr has a larger
    /// element, the return value is always negative. When the elements are equal in the two
    /// Rarrs, 1 is returned if the first Rarr is longer, 0 is returned if they are equal in
    /// length, and -1 is returned when the second Rarr is longer.</returns>
    let inline compareWith ((*[<InlineIfLambda>]*) comparer: 'T -> 'T -> int) (rarr1: Rarr<'T>) (rarr2: Rarr<'T>) = 
        let length1 = rarr1.Count
        let length2 = rarr2.Count
        let mutable i = 0
        let mutable result = 0
        if length1 < length2 then
            while i < rarr1.Count && result = 0 do
                result <- comparer rarr1.List.[i] rarr2.List.[i]
                i <- i + 1
        else
            while i < rarr2.Count && result = 0 do
                result <- comparer rarr1.List.[i] rarr2.List.[i]
                i <- i + 1
        if result <> 0 then result
        elif length1 = length2 then 0
        elif length1 < length2 then -1
        else 1


    /// <summary>Builds a new Rarr that contains the elements of each of the given sequence of Rarrs.</summary>
    /// <param name="rarrs">The input sequence of Rarrs.</param>
    /// <returns>The concatenation of the sequence of input Rarrs.</returns>
    let concat (rarrs: seq<Rarr<'T>>) : Rarr<'T>= 
        //let concat (rarrs: Rarr<Rarr<'T>>) : Rarr<'T> =  // test don't pass with this
        //if rarrs.Count = 0 then
        if Seq.isEmpty rarrs then
            Rarr(0)
        else
            let res = Rarr() //rarrs.[0].Clone()
            for r in rarrs do res.AddRange(r)
            //for i=1 to rarrs.(xs.Count-1) do res.AddRange(rarrs.[i])
            res

    /// <summary>Tests if the Rarr contains the specified element.</summary>
    /// <param name="value">The value to locate in the input Rarr.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns><c>true</c> if the input Rarr contains the specified element; false otherwise.</returns>
    let inline contains value (rarr: Rarr<'T>) = 
        rarr.List.Contains(value)

    /// <summary>Builds a new Rarr that contains the elements of the given Rarr.
    /// A shallow copy by calling rarr.GetRange(0,rarr.Count) </summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>A copy of the input Rarr.</returns>
    let inline copy (rarr: Rarr<'T>) = 
        rarr.GetRange(0,rarr.Count) // fastest way to create a shallow copy


    /// <summary>Reads a range of elements from the first Rarr and write them into the second. The target Rarr must already have the required minimum size to fit targetStartIndex + count.</summary>
    /// <param name="source">The source Rarr.</param>
    /// <param name="sourceIndex">The starting index of the source Rarr.</param>
    /// <param name="target">The target Rarr.</param>
    /// <param name="targetStartIndex">The starting index of the target Rarr.</param>
    /// <param name="count">The number of elements to copy.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when any of sourceIndex,targetStartIndex or count are negative, or when there aren't enough elements in source or target.</exception>
    let inline blit (source: Rarr<'T>) (sourceIndex: int) (target: Rarr<'T>) (targetStartIndex: int) (count: int) :unit= 
        if sourceIndex < 0  then  ArgumentException.RaiseBase "Rarr.blit: sourceIndex %d cannot be negative." sourceIndex
        if targetStartIndex < 0  then  ArgumentException.RaiseBase "Rarr.blit:targetStartIndex %d cannot be negative." targetStartIndex
        if count < 0  then  ArgumentException.RaiseBase "Rarr.blit: count %d cannot be negative." count
        if source.Count < sourceIndex + count then  ArgumentException.RaiseBase "Rarr.blit: source.Count %d is smaller than  sourceIndex %d + count %d."   source.Count  sourceIndex  count
        if target.Count < targetStartIndex + count then  ArgumentException.RaiseBase "Rarr.blit: target.Count %d is smaller than  targetStartIndex %d + count %d."  target.Count  targetStartIndex count
        let mutable j = targetStartIndex
        for i = sourceIndex to sourceIndex + count - 1 do
            target.[j] <- source.[i]
            j <-j+1

    /// <summary>Reads a range of elements from the first Rarr and write them into the second. The target Rarr increases in size if needed.
    /// But it needs to have  minimum <c>targetStartIndex</c> elements already.</summary>
    /// <param name="source">The source Rarr.</param>
    /// <param name="sourceIndex">The starting index of the source Rarr.</param>
    /// <param name="target">The target Rarr.</param>
    /// <param name="targetStartIndex">The starting index of the target Rarr.</param>
    /// <param name="count">The number of elements to copy.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when any of sourceIndex, targetStartIndex or count are negative, or when there aren't enough elements in source.</exception>
    let inline blitExtend (source: Rarr<'T>) (sourceIndex: int) (target: Rarr<'T>) (targetStartIndex: int) (count: int) :unit= 
        if sourceIndex < 0  then  ArgumentException.RaiseBase "Rarr.blit: sourceIndex %d cannot be negative." sourceIndex
        if targetStartIndex < 0  then  ArgumentException.RaiseBase "Rarr.blit: targetStartIndex %d cannot be negative." targetStartIndex
        if count < 0  then  ArgumentException.RaiseBase "Rarr.blit: count %d cannot be negative." count
        if source.Count < sourceIndex + count then  ArgumentException.RaiseBase "Rarr.blit: source.Count %d is smaller than  sourceIndex %d + count %d." source.Count sourceIndex  count
        if target.Count < targetStartIndex then  ArgumentException.RaiseBase "Rarr.blit: target.Count %d is smaller than  targetStartIndex %d." target.Count targetStartIndex
        let mutable j = targetStartIndex
        let tlasti = target.Count-1
        for i = sourceIndex to sourceIndex + count - 1 do
            if j > tlasti then
                target.Add(source.[i]) //increases in size if needed
            else
                target.[j] <- source.[i]
            j<-j+1


    let inline private countByImpl (comparer: IEqualityComparer<'SafeKey>) (projection: 'T->'SafeKey) (getKey: 'SafeKey->'Key) (rarr: Rarr<'T>) :Rarr<'Key*int> = 
        let length = rarr.Count
        if length = 0 then
            Rarr(0)
        else
            let dict = Dictionary comparer
            // Build the groupings
            for v in rarr do
                let safeKey = projection v
                let mutable prev = Unchecked.defaultof<_>
                if dict.TryGetValue(safeKey, &prev) then
                    dict.[safeKey] <- prev + 1
                else
                    dict.[safeKey] <- 1
            let res = Rarr(dict.Count)
            for group in dict do
                res.Add( getKey group.Key, group.Value)
            res
    /// <summary>Applies a key-generating function to each element of an Rarr and returns an Rarr yielding unique
    /// keys and their number of occurrences in the original Rarr.</summary>
    /// <param name="projection">A function transforming each item of the input Rarr into a key to be
    /// compared against the others.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The result Rarr.</returns>
    let countBy (projection: 'T->'Key) (rarr: Rarr<'T>) : Rarr<'Key * int> = 
        if typeof<'Key>.IsValueType
            // We avoid wrapping a StructBox, because under 64 JIT we get some "hard" tail-calls which affect performance
            then countByImpl HashIdentity.Structural<'Key> projection id rarr

            // Wrap a StructBox around all keys in case the key type is itself a type using null as a representation
            else countByImpl StructBox<'Key>.Comparer (fun t -> StructBox (projection t)) (fun sb -> sb.Value) rarr


    /// <summary>Creates an Rarr whose elements are all initially the given value.</summary>
    /// <param name="count">The length of the Rarr to create.</param>
    /// <param name="value">The value for the elements.</param>
    /// <returns>The created Rarr.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when count is negative.</exception>
    let create (count: int) (value: 'T) = 
        if count < 0 then  ArgumentException.RaiseBase "Rarr.create: count %d cannot be negative." count
        let rarr= Rarr(count)
        for i = 0 to count-1 do
            rarr.Add value
        rarr


    /// <summary>Returns an Rarr that contains no duplicate entries according to generic hash and
    /// equality comparisons on the entries.
    /// If an element occurs multiple times in the Rarr then the later occurrences are discarded.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The result Rarr.</returns>
    let distinct (rarr: Rarr<'T>) = 
        let temp = Rarr()
        let hashSet = HashSet<'T>(HashIdentity.Structural<'T>)
        let li = rarr.List
        for i=0 to rarr.Count-1 do
            let v = li.[i]
            if hashSet.Add(v) then
                temp.Add v
        temp


    /// <summary>Returns an Rarr that contains no duplicate entries according to the
    /// generic hash and equality comparisons on the keys returned by the given key-generating function.
    /// If an element occurs multiple times in the Rarr then the later occurrences are discarded.</summary>
    /// <param name="projection">A function transforming the Rarr items into comparable keys.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The result Rarr.</returns>
    let distinctBy (projection:'T -> 'Key) (rarr: Rarr<'T>) : Rarr<'T> = 
        if rarr.Count < 2 then
            rarr.Clone() // 0 or 1 item, just clone
        else
            let temp = Rarr()
            let hashSet = HashSet<'Key>(HashIdentity.Structural<_>)
            let li = rarr.List
            for i=0 to rarr.Count-1 do
                let v = li.[i]
                if hashSet.Add(projection v) then
                    temp.Add v
            temp



    /// <summary>Returns an empty Rarr of the given type. With initial capacity zero.</summary>
    /// <returns>The empty Rarr.</returns>
    [<GeneralizableValue>]
    let inline empty<'T> : Rarr<'T> = 
        Rarr<'T>(0)


    /// <summary>Returns the only element of the Rarr.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The only element of the Rarr.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when the input does not have precisely one element.</exception>
    let exactlyOne (rarr: Rarr<'T>) = 
        if rarr.Count = 1 then rarr.List.[0]
        else ArgumentException.RaiseBase "Rarr.exactlyOne: Rarr has %d elements, not one." rarr.Count


    /// <summary>Returns a new list with the distinct elements of the input Rarr which do not appear in the itemsToExclude sequence.
    /// Uses generic hash and equality comparisons to compare values.</summary>
    /// <param name="itemsToExclude">A sequence whose elements that also occur in the input Rarr will cause those elements to be
    /// removed from the result.</param>
    /// <param name="rarr">An Rarr whose elements that are not also in itemsToExclude will be returned.</param>
    /// <returns>An Rarr that contains the distinct elements of <c>Rarr</c> that do not appear in <c>itemsToExclude</c>.</returns>
    let except (itemsToExclude: seq<'T>) (rarr: Rarr<'T>) : Rarr<'T> = 
        if rarr.Count = 0 then
            rarr
        else
            let cached = HashSet(itemsToExclude, HashIdentity.Structural)
            let res = Rarr()
            let li = rarr.List
            for i=0 to rarr.Count-1 do
                let e = li.[i]
                if cached.Add e then
                    res.Add e
            res

    /// <summary>Tests if any element of the Rarr satisfies the given predicate.
    /// The predicate is applied to the elements of the input Rarr. If any application
    /// returns true then the overall result is <c>true</c> and no further elements are tested.
    /// Otherwise, false is returned.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns><c>true</c> if any result from <c>predicate</c> is <c>true</c>.</returns>
    let exists (predicate: 'T -> bool) (rarr: Rarr<'T>) :bool = 
        rarr.Exists (System.Predicate predicate)


    /// <summary>Tests if any pair of corresponding elements of the Rarrs satisfies the given predicate.
    /// The predicate is applied to matching elements in the two collections up to the lesser of the
    /// two lengths of the collections. If any application returns true then the overall result is
    /// true and no further elements are tested. Otherwise, if one collections is longer
    /// than the other then the <c>ArgumentException</c> exception is raised.
    /// Otherwise, false is returned.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="rarr1">The first input Rarr.</param>
    /// <param name="rarr2">The second input Rarr.</param>
    /// <returns><c>true</c> if any result from <c>predicate</c> is <c>true</c>.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>
    let exists2 (predicate:'T->'U->bool) (rarr1: Rarr<'T>) (rarr2: Rarr<'U>) :bool = 
        let len1 = rarr1.Count
        if len1 <> rarr2.Count then ArgumentException.RaiseBase "Rarr.exists2: count of rarr1 %d does not match rarr2 %d." rarr1.Count rarr2.Count
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(predicate)
        let rec loop i = i < len1 && (f.Invoke(rarr1.List.[i], rarr2.List.[i]) || loop (i+1))
        loop 0

    /// <summary>Fills a range of elements of the Rarr with the given value. Extends the Rarr if needed</summary>
    /// <param name="target">The target Rarr.</param>
    /// <param name="startIndex">The index of the first element to set.</param>
    /// <param name="count">The number of elements to set.</param>
    /// <param name="value">The value to set.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when either startIndex or count is negative.</exception>
    let fill (target: Rarr<'T>) (startIndex: int) (count: int) (value: 'T) = 
        if startIndex < 0  then  ArgumentException.RaiseBase "Rarr.fill: startIndex %d cannot be negative." startIndex
        if count < 0  then  ArgumentException.RaiseBase "Rarr.fill: count %d cannot be negative." count
        if target.Count < startIndex then  ArgumentException.RaiseBase "Rarr.fill: target.Count %d is smaller than  startIndex %d."   target.Count  startIndex
        let tlasti = target.Count-1
        for j = startIndex to startIndex + count - 1 do
            if j > tlasti then
                target.Add(value)
            else
                target.[j] <- value

    /// <summary>Returns a new collection containing only the elements of the collection
    /// for which the given predicate returns <c>true</c>.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>An Rarr containing the elements for which the given predicate returns true.</returns>
    let inline filter (predicate:'T->bool) (rarr: Rarr<'T>) = 
        rarr.FindAll (System.Predicate predicate)


    /// <summary>Returns the first element for which the given function returns <c>true</c>.
    /// Raise <see cref="T:System.Collections.Generic.KeyNotFoundException"/> if no such element exists.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <exception cref="T:System.Collections.Generic.KeyNotFoundException">Thrown if <c>predicate</c> never returns true.</exception>
    /// <returns>The first element for which <c>predicate</c> returns true.</returns>
    let find (predicate : 'T -> bool) (rarr: Rarr<'T>) = 
        let elementIndex =  rarr.FindIndex (System.Predicate predicate)
        match elementIndex with
        | -1 ->
            KeyNotFoundException.Raise "Rarr.find did not find for predicate in Rarr of %d items." rarr.Count
        | index ->
            rarr.List.[index]

    /// <summary>Returns the last element for which the given function returns <c>true</c>.
    /// Raise <see cref="T:System.Collections.Generic.KeyNotFoundException"/> if no such element exists.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <exception cref="T:System.Collections.Generic.KeyNotFoundException">Thrown if <c>predicate</c> never returns true.</exception>
    /// <returns>The last element for which <c>predicate</c> returns true.</returns>
    let findBack (predicate:'T->bool) (rarr: Rarr<'T>) = 
        let li = rarr.List
        let rec loop i = 
            if i < 0 then
                KeyNotFoundException.Raise "Rarr.findBack: not found in %d items %s" rarr.Count rarr.ToNiceStringLong
            else
                if predicate li.[i] then li.[i]  else loop (i-1)
        loop (rarr.Count-1)


    /// <summary>Returns the index of the first element in the Rarr that satisfies the given predicate. Raise <see cref="T:System.Collections.Generic.KeyNotFoundException"/> if
    /// none of the elements satisfy the predicate.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <exception cref="T:System.Collections.Generic.KeyNotFoundException">Thrown if <c>predicate</c> never returns true.</exception>
    /// <returns>The index of the first element in the Rarr that satisfies the given predicate.</returns>
    let findIndex (predicate:'T->bool) (rarr: Rarr<'T>) = 
        let elementIndex =  rarr.FindIndex (System.Predicate predicate)
        match elementIndex with
        | -1 ->
            KeyNotFoundException.Raise "Rarr.findIndex did not find for predicate in Rarr of %d items." rarr.Count
        | index ->
            index


    /// <summary>Returns the index of the last element in the Rarr
    /// that satisfies the given predicate. Raise <see cref="T:System.Collections.Generic.KeyNotFoundException"/> if
    /// none of the elements satisfy the predicate.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <exception cref="T:System.Collections.Generic.KeyNotFoundException">Thrown if <c>predicate</c> never returns true.</exception>
    /// <returns>The index of the last element in the Rarr that satisfies the given predicate.</returns>
    let findIndexBack (predicate:'T->bool) (rarr: Rarr<'T>) = 
        let li = rarr.List
        let rec go n = 
            if n < 0 then
                KeyNotFoundException.Raise "Rarr.findIndexBack: not found in %s" rarr.ToNiceStringLong
            elif predicate li.[n] then
                n
            else go (n-1)
        go (rarr.Count-1)


    /// <summary>Applies a function to each element of the collection, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes
    /// <c>f (... (f s i0)...) iN</c></summary>
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The final state.</returns>
    let fold<'T, 'State> (folder: 'State -> 'T -> 'State) (state: 'State) (rarr: Rarr<'T>) = 
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(folder)
        let mutable state = state
        let li = rarr.List
        for i = 0 to rarr.Count-1 do
            state <- f.Invoke(state, li.[i])
        state


    /// <summary>Applies a function to pairs of elements drawn from the two collections,
    /// left-to-right, threading an accumulator argument
    /// through the computation. The two input
    /// Rarrs must have the same lengths, otherwise an <c>ArgumentException</c> is
    /// raised.</summary>
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="rarr1">The first input Rarr.</param>
    /// <param name="rarr2">The second input Rarr.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>
    /// <returns>The final state.</returns>
    let fold2<'T1, 'T2, 'State>  folder (state: 'State) (rarr1: 'T1 Rarr) (rarr2: 'T2  Rarr) = 
        if rarr1.Count <> rarr2.Count then ArgumentException.RaiseBase "Rarr.fold2: count of rarr1 %d does not match rarr2 %d." rarr1.Count rarr2.Count
        let f = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt(folder)
        let mutable state = state
        for i = 0 to rarr1.Count-1 do
            state <- f.Invoke(state, rarr1.List.[i], rarr2.List.[i])
        state


    /// <summary>Applies a function to each element of the Rarr, starting from the end, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes
    /// <c>f i0 (...(f iN s))</c></summary>
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <param name="state">The initial state.</param>
    /// <returns>The state object after the folding function is applied to each element of the Rarr.</returns>
    let foldBack<'T, 'State> (folder: 'T -> 'State -> 'State) (rarr: Rarr<'T>) (state: 'State) = 
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(folder)
        let mutable res = state
        let li = rarr.List
        for i = rarr.Count-1 downto 0 do
            res <- f.Invoke(li.[i], res)
        res


    /// <summary>Apply a function to pairs of elements drawn from the two collections, right-to-left,
    /// threading an accumulator argument through the computation. The two input
    /// Rarrs must have the same lengths, otherwise an <c>ArgumentException</c> is
    /// raised.</summary>
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="rarr1">The first input Rarr.</param>
    /// <param name="rarr2">The second input Rarr.</param>
    /// <param name="state">The initial state.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>
    /// <returns>The final state.</returns>
    let foldBack2<'T1, 'T2, 'State>  folder (rarr1: 'T1 Rarr) (rarr2: 'T2  Rarr) (state: 'State) = 
        let len = rarr1.Count
        if len <> rarr2.Count then ArgumentException.RaiseBase "Rarr.foldBack2: count of rarr1 %d does not match rarr2 %d." rarr1.Count rarr2.Count
        let mutable res = state
        let f = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt(folder)
        for i = len-1 downto 0 do
            res <- f.Invoke(rarr1.List.[i], rarr2.List.[i], res)
        res


    /// <summary>Tests if all elements of the Rarr satisfy the given predicate.
    /// The predicate is applied to the elements of the input collection. If any application
    /// returns false then the overall result is false and no further elements are tested.
    /// Otherwise, true is returned.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns><c>true</c> if all of the Rarr elements satisfy the predicate.</returns>
    let forall (predicate: 'T -> bool) (rarr: Rarr<'T>) = 
        rarr.TrueForAll (System.Predicate predicate)


    /// <summary>Tests if all corresponding elements of the Rarr satisfy the given predicate pairwise.
    /// The predicate is applied to matching elements in the two collections up to the lesser of the
    /// two lengths of the collections. If any application returns false then the overall result is
    /// false and no further elements are tested. Otherwise, if one collection is longer
    /// than the other then the <c>ArgumentException</c> exception is raised.
    /// Otherwise, true is returned.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="rarr1">The first input Rarr.</param>
    /// <param name="rarr2">The second input Rarr.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>
    /// <returns><c>true</c> if all of the Rarr elements satisfy the predicate.</returns>
    let forall2 (predicate:'T->'U->bool) (rarr1: Rarr<'T>) (rarr2: Rarr<'U>) : bool = 
        let len1 = rarr1.Count
        if len1 <> rarr2.Count then ArgumentException.RaiseBase "Rarr.forall2: count of rarr1 %d does not match rarr2 %d." rarr1.Count rarr2.Count
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(predicate)
        let rec loop i = i >= len1 || (f.Invoke(rarr1.List.[i], rarr2.List.[i]) && loop (i+1))
        loop 0

    /// <summary>Gets an element from an Rarr. (Use Rarr.getNeg(i) function if you want to use negative indices too.)</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <param name="index">The input index.</param>
    /// <returns>The value of the Rarr at the given index.</returns>
    /// <exception cref="T:System.IndexOutOfRangeException">Thrown when the index is negative or the input Rarr does not contain enough elements.</exception>
    let inline get (rarr: Rarr<'T>) index = 
        rarr.[index]


    /// <summary>Builds a new Rarr that contains the given subrange specified by starting index and length.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <param name="startIndex">The index of the first element of the sub Rarr.</param>
    /// <param name="count">The length of the sub Rarr.</param>
    /// <returns>The created sub Rarr.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when either startIndex or count is negative, or when there aren't enough elements in the input Rarr.</exception>
    let sub (rarr: Rarr<'T>) (startIndex: int) (count: int) = 
        // nice ArgumentException will be thrown by this
        rarr.GetRange(startIndex,count)

    let inline private groupByImpl (comparer: IEqualityComparer<'SafeKey>) (keyf: 'T->'SafeKey) (getKey: 'SafeKey->'Key) (rarr: Rarr<'T>) :  Rarr<'Key * Rarr<'T>>= 
        let length = rarr.Count
        if length = 0 then
           Rarr(0)
        else
            let dict = Dictionary<_, Rarr<_>> comparer
            // Build the groupings
            let li = rarr.List
            for i = 0 to length - 1 do
                let v = li.[i]
                let safeKey = keyf v
                let mutable prev = Unchecked.defaultof<_>
                if dict.TryGetValue(safeKey, &prev) then
                    prev.Add v
                else
                    let prev = Rarr ()
                    dict.[safeKey] <- prev
                    prev.Add v
            // Return the rarr-of-rarrs.
            let result = Rarr(dict.Count)
            for group in dict do
                result.Add( getKey group.Key, group.Value)
            result

    /// <summary>Applies a key-generating function to each element of an Rarr and yields an Rarr of
    /// unique keys. Each unique key contains an Rarr of all elements that match
    /// to this key.</summary>
    /// <param name="projection">A function that transforms an element of the Rarr into a comparable key.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The result Rarr.</returns>
    let groupBy (projection:'T -> 'Key) (rarr:Rarr<'T>) :  Rarr<'Key * Rarr<'T>> = 
        if typeof<'Key>.IsValueType then
            // We avoid wrapping a StructBox, because under 64 JIT we get some "hard" tail-calls which affect performance
             groupByImpl HashIdentity.Structural<'Key> projection id rarr
        else
            // Wrap a StructBox around all keys in case the key type is itself a type using null as a representation
             groupByImpl StructBox<'Key>.Comparer (fun t -> StructBox (projection t)) (fun sb -> sb.Value) rarr


    /// <summary>Returns the first element of the Rarr.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The first element of the Rarr.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarr is empty.</exception>
    let inline head (rarr: Rarr<'T>) = 
        if rarr.Count = 0 then ArgumentException.RaiseBase "Rarr.head: The input Rarr is empty." else rarr.List.[0]


    /// <summary>Builds a new Rarr whose elements are the corresponding elements of the input Rarr
    /// paired with the integer index (from 0) of each element.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The Rarr of indexed elements.</returns>
    let indexed (rarr: Rarr<'T>) :  Rarr<int * 'T> = 
        let res = Rarr(rarr.Count)
        let li = rarr.List
        for i = 0 to rarr.Count-1 do
            res.Add (i, li.[i])
        res

    /// <summary>Creates an Rarr given the dimension and a generator function to compute the elements.</summary>
    /// <param name="count">The number of elements to initialize.</param>
    /// <param name="initializer">The function to generate the initial values for each index.</param>
    /// <returns>The created Rarr.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when count is negative.</exception>
    let inline init (count:int) (initializer:int->'T) = 
        if count < 0 then ArgumentException.RaiseBase "Rarr.init: count %d is negative." count
        let res = Rarr(count)
        for i = 0 to count-1 do
            res.Add (initializer i)
        res

    /// <summary>Return a new Rarr with a new item inserted before the given index.(does NOT modify in place !)</summary>
    /// <param name="index">The index where the item should be inserted.</param>
    /// <param name="value">The value to insert.</param>
    /// <param name="source">The input Rarr.</param>
    /// <returns>The result Rarr.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when index is below not within source.Count.</exception>
    let insertAt (index: int) (value: 'T) (source: Rarr<'T>) : Rarr<'T> = 
        if index < 0 || index > source.Count then ArgumentException.RaiseBase "Rarr.insertAt: index %d not within source.Count %d." index source.Count
        let r = source.Clone()
        r.Insert(index,value)
        r


    /// <summary>Return a new Rarr with new items inserted before the given index.(does NOT modify in place !) If required increases the count of the Rarr.</summary>
    /// <param name="index">The index where the items should be inserted.</param>
    /// <param name="values">The values to insert.</param>
    /// <param name="source">The input Rarr.</param>
    /// <returns>The result Rarr.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when index is below not within source.Count.</exception>
    let insertManyAt (index: int) (values: seq<'T>) (source: Rarr<'T>) : Rarr<'T> = 
        if index < 0 || index > source.Count  then ArgumentException.RaiseBase "Rarr.insertManyAt: index %d and  not within source.Count %d." index source.Count
        let r = source.Clone()
        r.InsertRange(index,values)
        r

    /// <summary>Returns true if the given Rarr is empty, otherwise false.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns><c>true</c> if the Rarr is empty.</returns>
    let inline isEmpty (rarr: Rarr<'T>) = 
        rarr.Count = 0

    /// <summary>Gets an element from an Rarr.</summary>
    /// <param name="index">The input index.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The value of the Rarr at the given index.</returns>
    /// <exception cref="T:System.IndexOutOfRangeException">Thrown when the index is negative or the input Rarr does not contain enough elements.</exception>
    let inline item index (rarr: Rarr<'T>) = 
        rarr.[index]

    /// <summary>Applies the given function to each element of the Rarr.</summary>
    /// <param name="action">The function to apply.</param>
    /// <param name="rarr">The input Rarr.</param>
    let inline iter ((*[<InlineIfLambda>]*) action) (rarr: Rarr<'T>) = // TODO activate InlineIfLambda
        let li = rarr.List
        for i = 0 to rarr.Count-1 do
            action li.[i]


    /// <summary>Applies the given function to pair of elements drawn from matching indices in two Rarrs. The
    /// two Rarrs must have the same lengths, otherwise an <c>ArgumentException</c> is
    /// raised.</summary>
    /// <param name="action">The function to apply.</param>
    /// <param name="rarr1">The first input Rarr.</param>
    /// <param name="rarr2">The second input Rarr.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>
    let inline iter2 (action : 'T -> 'U -> unit) (rarr1: Rarr<'T>) (rarr2: Rarr<'U>) = 
        if rarr1.Count <> rarr2.Count then ArgumentException.RaiseBase "Rarr.iter2: count of rarr1 %d does not match rarr2 %d." rarr1.Count rarr2.Count
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(action)
        for i = 0 to rarr1.Count-1 do
            f.Invoke(rarr1.List.[i], rarr2.List.[i])


    /// <summary>Applies the given function to each element of the Rarr. The integer passed to the
    /// function indicates the index of element.</summary>
    /// <param name="action">The function to apply to each index and element.</param>
    /// <param name="rarr">The input Rarr.</param>
    let inline iteri (action : int -> 'T -> unit)  (rarr: Rarr<'T>) = 
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(action)
        let li = rarr.List
        for i = 0 to rarr.Count-1 do
            f.Invoke(i, li.[i])


    /// <summary>Applies the given function to pair of elements drawn from matching indices in two Rarrs,
    /// also passing the index of the elements. The two Rarrs must have the same lengths,
    /// otherwise an <c>ArgumentException</c> is raised.</summary>
    /// <param name="action">The function to apply to each index and pair of elements.</param>
    /// <param name="rarr1">The first input Rarr.</param>
    /// <param name="rarr2">The second input Rarr.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>
    let inline iteri2 (action : int -> 'T -> 'U -> unit)  (rarr1: Rarr<'T>) (rarr2: Rarr<'U>) = 
        if rarr1.Count <> rarr2.Count then ArgumentException.RaiseBase "Rarr.iteri2: count of rarr1 %d does not match rarr2 %d." rarr1.Count rarr2.Count
        let f = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt(action)
        for i = 0 to rarr1.Count-1 do
            f.Invoke(i, rarr1.List.[i], rarr2.List.[i])


    /// <summary>Returns the last element of the Rarr.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The last element of the Rarr.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when the input does not have any elements.</exception>
    let inline last (rarr: Rarr<'T>) = 
        if rarr.Count = 0 then ArgumentException.RaiseBase "Rarr.last: The input Rarr is empty."
        rarr.List.[rarr.Count-1]


    /// <summary>Returns the length of an Rarr. You can also use property rarr.Count.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The length or count of the Rarr.</returns>
    let inline length (rarr: Rarr<'T>) = 
        rarr.Count

    /// <summary>Builds a new Rarr whose elements are the results of applying the given function
    /// to each of the elements of the Rarr.</summary>
    /// <param name="mapping">The function to transform elements of the Rarr.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The Rarr of transformed elements.</returns>
    let inline map ( mapping: 'T -> 'U) (rarr: Rarr<'T>) : Rarr<'U> = 
         rarr.ConvertAll (System.Converter mapping)

    /// <summary>Builds a new collection whose elements are the results of applying the given function
    /// to the corresponding elements of the two collections pairwise. The two input
    /// Rarrs must have the same lengths, otherwise an <c>ArgumentException</c> is
    /// raised.</summary>
    /// <param name="mapping">The function to transform the pairs of the input elements.</param>
    /// <param name="rarr1">The first input Rarr.</param>
    /// <param name="rarr2">The second input Rarr.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>
    /// <returns>The Rarr of transformed elements.</returns>
    let map2 (mapping: 'T1 ->'T2 ->'U) (rarr1: Rarr<'T1>) (rarr2: Rarr<'T2>) : Rarr<'U> = 
        if rarr1.Count <> rarr2.Count then ArgumentException.RaiseBase "Rarr.map2: count of rarr1 %d does not match rarr2 %d." rarr1.Count rarr2.Count
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(mapping)
        let res = Rarr<'U>(rarr1.Count)
        for i = 0 to rarr1.Count-1 do
            res.Add(f.Invoke(rarr1.List.[i], rarr2.List.[i]) )
        res

    /// <summary>Builds a new collection whose elements are the results of applying the given function
    /// to the corresponding triples from the three collections. The three input
    /// Rarrs must have the same length, otherwise an <c>ArgumentException</c> is
    /// raised.</summary>
    /// <param name="mapping">The function to transform the pairs of the input elements.</param>
    /// <param name="rarr1">The first input Rarr.</param>
    /// <param name="rarr2">The second input Rarr.</param>
    /// <param name="rarr3">The third input Rarr.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>
    /// <returns>The Rarr of transformed elements.</returns>
    let map3 (mapping: 'T1 ->'T2 ->'T3 ->'U)  (rarr1: Rarr<'T1> ) (rarr2: Rarr<'T2> ) (rarr3: Rarr<'T3> ) : Rarr<'U> = 
        let len1 = rarr1.Count
        if len1 <> rarr2.Count || len1 <> rarr3.Count then ArgumentException.RaiseBase "Rarr.map3: count of rarr1 %d does not match rarr2 %d or rarr3 %d" rarr1.Count rarr2.Count rarr3.Count
        let f = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt(mapping)
        let res = Rarr(len1)
        for i = 0 to rarr1.Count-1 do
            res.Add <|  f.Invoke(rarr1.List.[i], rarr2.List.[i], rarr3.List.[i])
        res


    /// <summary>Combines map and fold. Builds a new Rarr whose elements are the results of applying the given function
    /// to each of the elements of the input Rarr. The function is also used to accumulate a final value.</summary>
    /// <param name="mapping">The function to transform elements from the input Rarr and accumulate the final value.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The Rarr of transformed elements, and the final accumulated value.</returns>
    let mapFold<'T, 'State, 'Result> (mapping: 'State -> 'T -> 'Result * 'State) state (rarr:Rarr<'T>) : Rarr<'Result> * 'State = 
        match rarr.Count with
        | 0   -> Rarr(0), state
        | len ->
            let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(mapping)
            let mutable acc = state
            let res = Rarr(len)
            let li = rarr.List
            for i = 0 to len-1 do
                let h, s = f.Invoke(acc, li.[i])
                res.Add h
                acc <- s
            res, acc

    /// <summary>Combines map and foldBack. Builds a new Rarr whose elements are the results of applying the given function
    /// to each of the elements of the input Rarr. The function is also used to accumulate a final value.</summary>
    /// <param name="mapping">The function to transform elements from the input Rarr and accumulate the final value.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <param name="state">The initial state.</param>
    /// <returns>The Rarr of transformed elements, and the final accumulated value.</returns>
    let mapFoldBack<'T, 'State, 'Result> (mapping: 'T -> 'State -> 'Result * 'State) (rarr:Rarr<'T>) state : Rarr<'Result> * 'State= 
        match rarr.Count with
        | 0 -> Rarr(0), state
        | len ->
            let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(mapping)
            let mutable acc = state
            let res = Rarr(len)
            for i = 0 to len-1 do
                res.Add Unchecked.defaultof<'Result> // needs to be filled already because of 'downto' loop
            let li = rarr.List
            for i = len - 1 downto 0 do
                let h, s = f.Invoke(li.[i], acc)
                res.[i] <- h
                acc <- s
            res, acc

    /// <summary>Builds a new Rarr whose elements are the results of applying the given function
    /// to each of the elements of the Rarr. The integer index passed to the
    /// function indicates the index of element being transformed.</summary>
    /// <param name="mapping">The function to transform elements and their indices.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The Rarr of transformed elements.</returns>
    let mapi (mapping: int -> 'T -> 'U) (rarr: Rarr<'T>) : Rarr<'U> = 
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(mapping)
        let res = Rarr(rarr.Count)
        let li = rarr.List
        for i = 0 to rarr.Count-1 do
            res.Add <|  f.Invoke(i, li.[i])
        res

    /// <summary>Builds a new collection whose elements are the results of applying the given function
    /// to the corresponding elements of the two collections pairwise, also passing the index of
    /// the elements. The two input Rarrs must have the same lengths, otherwise an <c>ArgumentException</c> is
    /// raised.</summary>
    /// <param name="mapping">The function to transform pairs of input elements and their indices.</param>
    /// <param name="rarr1">The first input Rarr.</param>
    /// <param name="rarr2">The second input Rarr.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>
    /// <returns>The Rarr of transformed elements.</returns>
    let mapi2 (mapping: int -> 'T1 -> 'T2-> 'U) (rarr1: Rarr<'T1>) (rarr2: Rarr<'T2>) : Rarr<'U>  = 
        let f = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt(mapping)
        if rarr1.Count <> rarr2.Count then ArgumentException.RaiseBase "Rarr.mapi2: count of rarr1 %d does not match rarr2 %d." rarr1.Count rarr2.Count
        let res = Rarr(rarr1.Count)
        for i = 0 to rarr1.Count-1 do
            res.Add <|  f.Invoke(i, rarr1.List.[i], rarr2.List.[i])
        res

    /// <summary>Returns the greatest of all elements of the Rarr, compared via Operators.max on the function result.
    /// Throws ArgumentException for empty Rarrs.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarr is empty.</exception>
    /// <returns>The maximum element.</returns>
    let inline max (rarr: Rarr<'T>) = 
        if rarr.Count = 0 then ArgumentException.RaiseBase "Rarr.max: Count must be at least one: %s"  rarr.ToNiceStringLong
        let mutable acc = rarr.List.[0]        
        let li = rarr.List
        for i = 1 to rarr.Count - 1 do
            let curr = li.[i]
            if curr > acc then
                acc <- curr
        acc

    /// <summary>Returns the greatest of all elements of the Rarr, compared via Operators.max on the function result.</summary>
    /// <param name="projection">The function to transform the elements into a type supporting comparison.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarr is empty.</exception>
    /// <returns>The maximum element.</returns>
    let inline maxBy (projection : 'T -> 'Key) (rarr: Rarr<'T>) : 'T = 
        if rarr.Count = 0 then ArgumentException.RaiseBase "Rarr.maxBy: Count must be at least one: %s"  rarr.ToNiceStringLong
        let mutable accv = rarr.List.[0]
        if rarr.Count =  1 then 
            accv // if len = 1 then don't call the projection not even once !
        else 
            let mutable acc = projection accv
            let li = rarr.List
            for i = 1 to rarr.Count - 1 do
                let currv = li.[i]
                let curr = projection currv
                if curr > acc then
                    acc <- curr
                    accv <- currv
            accv

    /// <summary>Returns the lowest of all elements of the Rarr, compared via Operators.min.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarr is empty.</exception>
    /// <returns>The minimum element.</returns>
    let inline min (rarr: Rarr<'T>) : 'T= 
        if rarr.Count = 0 then ArgumentException.RaiseBase "Rarr.min: Count must be at least one: %s"  rarr.ToNiceStringLong
        let mutable acc = rarr.List.[0]
        let li = rarr.List
        for i = 1 to rarr.Count - 1 do
            let curr = li.[i]
            if curr < acc then
                acc <- curr
        acc

    /// <summary>Returns the lowest of all elements of the Rarr, compared via Operators.min on the function result.</summary>
    /// <param name="projection">The function to transform the elements into a type supporting comparison.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarr is empty.</exception>
    /// <returns>The minimum element.</returns>
    let inline minBy ((*[<InlineIfLambda>]*) projection : 'T -> 'Key) (rarr: Rarr<'T>) : 'T = 
        if rarr.Count = 0 then ArgumentException.RaiseBase "Rarr.minBy: Count must be at least one: %s"  rarr.ToNiceStringLong
        let mutable accv = rarr.List.[0]
        if rarr.Count =  1 then 
            accv // if len = 1 then don't call the projection not even once !
        else
            let mutable acc = projection accv
            let li = rarr.List
            for i = 1 to rarr.Count - 1 do
                let currv = li.[i]
                let curr = projection currv
                if curr < acc then
                    acc <- curr
                    accv <- currv
            accv

    /// <summary>Builds an Rarr from the given list.</summary>
    /// <param name="list">The input list.</param>
    /// <returns>The Rarr of elements from the list.</returns>
    let ofList (list: list<'T>) = 
        let res = Rarr<_>()
        let rec add = function
            | [] -> ()
            | e::l -> res.Add(e); add l
        add list
        res

    /// <summary>Builds a new Rarr from the given enumerable object. However if input is a Rarr or Generic.List it is returned directly</summary>
    /// <param name="source">The input sequence.</param>
    /// <returns>The Rarr of elements from the sequence.</returns>
    let ofSeq (source : seq<'T>) = 
        match source with
        | :? Rarr<'T> as r -> r
        | :? Collections.Generic.List<'T> as l -> Rarr.createDirectly(l)
        | _  -> Rarr (source)


    /// <summary>Returns an Rarr of each element in the input Rarr and its predecessor, with the
    /// exception of the first element which is only returned as the predecessor of the second element.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The result Rarr.</returns>
    let pairwise (rarr: Rarr<'T>) = 
        if rarr.Count < 2 then
            Rarr(0)
        else
            init (rarr.Count-1) (fun i -> rarr.List.[i], rarr.List.[i+1])


    /// <summary>Splits the collection into two collections, containing the
    /// elements for which the given predicate returns <c>true</c> and <c>false</c>
    /// respectively.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>A pair of Rarrs. The first containing the elements the predicate evaluated to <c>true</c> ,
    /// and the second containing those evaluated to <c>false</c>.</returns>
    let partition (predicate:'T->bool) (rarr : Rarr<'T>) : Rarr<'T> * Rarr<'T> = 
        let trueResults  = Rarr()
        let falseResults = Rarr()
        let len = rarr.Count
        let li = rarr.List
        for i = 0 to len - 1 do
            let el = li.[i]
            if predicate el then
                trueResults.Add el
            else
                falseResults.Add el
        trueResults, falseResults


    /// <summary>Returns an Rarr with all elements permuted according to the specified permutation.</summary>
    /// <param name="indexMap">The function that maps input indices to output indices.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The output Rarr.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when indexMap does not produce a valid permutation.</exception>
    let permute (indexMap:int -> int) (rarr: Rarr<'T>) : Rarr<'T>= 
        let res  = rarr.Clone()
        let inv = Array.zeroCreate rarr.Count
        let li = rarr.List
        for i = 0 to rarr.Count - 1 do
            let j = indexMap i
            if j < 0 || j >= rarr.Count then ArgumentException.RaiseBase "Rarr.permute: the indexMap generated %d from %d but only 0 to %d is allowed" j i (rarr.Count-1)
            res.[j] <- li.[i]
            inv.[j] <- 1uy
        for i = 0 to rarr.Count - 1 do
            if inv.[i] <> 1uy then ArgumentException.RaiseBase "Rarr.permute: the indexMap function did not generated %d a new value for " i
        res


    /// <summary>Applies the given function to successive elements, returning the first
    /// result where function returns <c>Some(x)</c> for some <c>x</c>. If the function
    /// never returns <c>Some(x)</c> then <see cref="T:System.Collections.Generic.KeyNotFoundException"/> is raised.</summary>
    /// <param name="chooser">The function to generate options from the elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <exception cref="T:System.Collections.Generic.KeyNotFoundException">Thrown if every result from
    /// <c>chooser</c> is <c>None</c>.</exception>
    /// <returns>The first result.</returns>
    let pick (chooser:'T -> 'U option) (rarr: Rarr<'T>) = 
        let li = rarr.List
        let rec loop i = 
            if i >= rarr.Count then
                KeyNotFoundException.Raise "Rarr.pick: Key not found in %d elements" rarr.Count
            else
                match chooser li.[i] with
                | None -> loop(i+1)
                | Some res -> res
        loop 0


    /// <summary>Applies a function to each element of the Rarr, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c>
    /// then computes <c>f (... (f i0 i1)...) iN</c>.
    /// Raises ArgumentException if the Rarr has size zero.</summary>
    /// <param name="reduction">The function to reduce a pair of elements to a single element.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarr is empty.</exception>
    /// <returns>The final result of the reductions.</returns>
    let reduce (reduction:'T -> 'T -> 'T) (rarr: Rarr<'T>) = 
        if rarr.Count = 0 then ArgumentException.RaiseBase "Rarr.reduce: Count must be at least one: %s"  rarr.ToNiceStringLong
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(reduction)
        let li = rarr.List
        let mutable res = li.[0]
        for i = 1 to rarr.Count - 1 do
            res <- f.Invoke(res, li.[i])
        res


    /// <summary>Applies a function to each element of the Rarr, starting from the end, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c>
    /// then computes <c>f i0 (...(f iN-1 iN))</c>.</summary>
    /// <param name="reduction">A function that takes in the next-to-last element of the list and the
    /// current accumulated result to produce the next accumulated result.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarr is empty.</exception>
    /// <returns>The final result of the reductions.</returns>
    let reduceBack (reduction:'T -> 'T -> 'T) (rarr: Rarr<'T>) = 
        if rarr.Count = 0 then ArgumentException.RaiseBase "Rarr.reduceBack: Count must be at least one: %s"  rarr.ToNiceStringLong
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(reduction)
        let mutable res = rarr.Last
        let li = rarr.List
        for i = rarr.Count - 2 downto 0 do
            res <- f.Invoke(li.[i], res)
        res



    /// <summary>Return a new Rarr with the item at a given index removed. (does NOT modify in place !)</summary>
    /// <param name="index">The index of the item to be removed.</param>
    /// <param name="source">The input Rarr.</param>
    /// <returns>The result Rarr.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when index is outside 0..source.Length - 1</exception>
    let removeAt (index: int) (source: Rarr<'T>) : Rarr<'T> = 
        if index < 0 || index >= source.Count then ArgumentException.RaiseBase "Rarr.removeAt: index %d not within source.Count %d." index source.Count
        let r = source.Clone()
        r.RemoveAt(index)
        r

    /// <summary>Return a new Rarr with the number of items starting at a given index removed. (does NOT modify in place !)</summary>
    /// <param name="index">The index of the item to be removed.</param>
    /// <param name="count">The number of items to remove.</param>
    /// <param name="source">The input Rarr.</param>
    /// <returns>The result Rarr.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when index is outside 0..source.Length - count</exception>
    let removeManyAt (index: int) (count: int) (source: Rarr<'T>) : Rarr<'T> = 
        if index < 0 || index > source.Count  - count then ArgumentException.RaiseBase "Rarr.removeManyAt: index %d and count %d not within source.Count %d." index count source.Count
        let r = source.Clone()
        r.RemoveRange(index,count)
        r

    /// <summary>Creates an Rarr by replicating the given initial value.</summary>
    /// <param name="count">The number of elements to replicate.</param>
    /// <param name="initial">The value to replicate</param>
    /// <returns>The generated Rarr.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when count is negative.</exception>
    let replicate count (initial:'T) = 
        if count < 0 then  ArgumentException.RaiseBase "Rarr.replicate: count %d cannot be negative "count
        let arr = Rarr(count)
        for _ = 0 to count-1 do
            arr.Add ( initial)
        arr

    /// <summary>Returns a new Rarr with the elements in reverse order.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The reversed Rarr.</returns>
    let rev (rarr: Rarr<'T>) = 
        let len = rarr.Count
        let result = Rarr (len)
        let li = rarr.List
        for i = len - 1 downto 0 do
            result.Add li.[i]
        result


    /// <summary>Like <c>fold</c>, but return the intermediary and final results.</summary>
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="stateInit">The initial state.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The Rarr of state values. This Rarr has the initial state and the state for all elements in the input Rarr, so one more item than the input.</returns>
    let scan<'T, 'State> (folder:'State -> 'T -> 'State) (stateInit: 'State) (rarr: Rarr<'T>) :Rarr<'State> = 
        let folder = OptimizedClosures.FSharpFunc<_,_,_>.Adapt folder
        // Holds the initial and intermediate state values.
        let results = Rarr (rarr.Count+1)
        results.Add stateInit
        // Fold over the specified range of items.
        let mutable state = stateInit
        let li = rarr.List
        for i = 0 to rarr.Count-1 do
            state <- folder.Invoke (state, li.[i])
            results.Add state
        results

    /// <summary>Like <c>foldBack</c>, but return both the intermediary and final results.</summary>
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <param name="stateInit">The initial state.</param>
    /// <returns>The Rarr of state values. Count = input count + 1 </returns>
    let scanBack<'T, 'State> (folder:'T -> 'State -> 'State) (rarr: Rarr<'T>) (stateInit: 'State) : Rarr<'State> = 
        let folder = OptimizedClosures.FSharpFunc<_,_,_>.Adapt folder
        let results = Rarr (rarr.Count+1)// Holds the initial and intermediate state values.
        for _ = 0 to rarr.Count do // to fill up list +1 with default values
            results.Add stateInit
        // Fold over the specified range of items.
        let mutable state = stateInit
        let li = rarr.List
        for i = rarr.Count-1 downto 0 do
            state <- folder.Invoke (li.[i],state)
            results.[i] <- state
        results


    /// <summary>Sets an element of an Rarr. (use Rarr.setNeg(i) function if you want to use negative indices too)</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <param name="index">The input index.</param>
    /// <param name="value">The input value.</param>
    /// <exception cref="T:System.IndexOutOfRangeException">Thrown when the index is negative or the input Rarr does not contain enough elements.</exception>
    let inline set (rarr: Rarr<'T>) index value = 
        rarr.[index] <- value


    /// <summary>Returns an Rarr that contains one item only.</summary>
    /// <param name="value">The input item.</param>
    /// <returns>The result Rarr of one item.</returns>
    let inline singleton value = 
        let res = Rarr(1)
        res.Add value
        res


    /// <summary>Builds a new Rarr that contains the elements of the given Rarr, excluding the first N elements.</summary>
    /// <param name="count">The number of elements to skip.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>A copy of the input Rarr, after removing the first N elements.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when count is negative or exceeds the number of
    /// elements in the Rarr.</exception>
    let skip count (rarr: Rarr<'T>) = 
        if count < 0 || count > rarr.Count then ArgumentException.RaiseBase "Rarr.skip: count %d is not in range of 0 to rarr.Count %d " count rarr.Count
        if count = rarr.Count then
            Rarr()
        else
            rarr.GetRange(count, rarr.Count-count)


    /// <summary>Bypasses elements in an Rarr while the given predicate returns <c>true</c>, and then returns
    /// the remaining elements in a new Rarr.</summary>
    /// <param name="predicate">A function that evaluates an element of the Rarr to a boolean value.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The created sub Rarr.</returns>
    let skipWhile (predicate:'T->bool) (rarr: Rarr<'T>) = 
        let mutable i = 0
        let li = rarr.List
        while i < rarr.Count && predicate li.[i] do
            i <- i + 1
        if i = rarr.Count then
            Rarr()
        else
            rarr.GetRange(i, rarr.Count-i)


    /// <summary>Sorts the elements of an Rarr, returning a new Rarr. Elements are compared using <see cref="M:Microsoft.FSharp.Core.Operators.compare"/>.
    /// This means "Z" is before "a". This is different from Collections.Generic.List.Sort() where "a" is before "Z" using IComparable interface.
    /// This is NOT a stable sort, i.e. the original order of equal elements is not necessarily preserved.
    /// For a stable sort, consider using Seq.Sort</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>A new sorted Rarr.</returns>
    let sort<'T when 'T : comparison> (rarr : Rarr<'T>) : Rarr<'T> = 
        let r = rarr.GetRange(0,rarr.Count) // fastest way to create a shallow copy
        r.Sort(Operators.compare)
        r


    /// <summary>Sorts the elements of an Rarr, using the given projection for the keys and returning a new Rarr.
    /// Elements are compared using <see cref="M:Microsoft.FSharp.Core.Operators.compare"/>.
    /// This means "Z" is before "a". This is different from Collections.Generic.List.Sort() where "a" is before "Z" using IComparable interface.
    /// This is NOT a stable sort, i.e. the original order of equal elements is not necessarily preserved.
    /// For a stable sort, consider using Seq.sort.</summary>
    /// <param name="projection">The function to transform Rarr elements into the type that is compared.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The sorted Rarr.</returns>
    let sortBy<'T, 'Key when 'Key : comparison> (projection : 'T -> 'Key) (rarr : Rarr<'T>) : Rarr<'T> = 
        let r = rarr.GetRange(0,rarr.Count) // fastest way to create a shallow copy
        r.Sort (fun x y -> Operators.compare (projection x) (projection y))
        r


    /// <summary>Sorts the elements of an Rarr, in descending order, using the given projection for the keys and returning a new Rarr.
    /// Elements are compared using <see cref="M:Microsoft.FSharp.Core.Operators.compare"/>.
    /// This means in ascending sorting "Z" is before "a". This is different from Collections.Generic.List.Sort() where "a" is before "Z" using IComparable interface.
    /// This is NOT a stable sort, i.e. the original order of equal elements is not necessarily preserved.
    /// For a stable sort, consider using Seq.sort.</summary>
    /// <param name="projection">The function to transform Rarr elements into the type that is compared.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The sorted Rarr.</returns>
    let inline sortByDescending<'T, 'Key when 'Key : comparison> (projection : 'T -> 'Key) (rarr : Rarr<'T>) : Rarr<'T> = 
        let r = rarr.GetRange(0,rarr.Count) // fastest way to create a shallow copy
        r.Sort (fun x y -> Operators.compare (projection y) (projection x)) // x and y are swapped for descending order
        r


    /// <summary>Sorts the elements of an Rarr, in descending order, returning a new Rarr. Elements are compared using <see cref="M:Microsoft.FSharp.Core.Operators.compare"/>.
    /// This means in ascending sorting "Z" is before "a". This is different from Collections.Generic.List.Sort() where "a" is before "Z" using IComparable interface.
    /// This is NOT a stable sort, i.e. the original order of equal elements is not necessarily preserved.
    /// For a stable sort, consider using Seq.sort.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The sorted Rarr.</returns>
    let sortDescending<'T when 'T : comparison> (rarr : Rarr<'T>) : Rarr<'T> = 
        let r = rarr.GetRange(0,rarr.Count) // fastest way to create a shallow copy
        r.Sort(Operators.compare) // Operators.compare is need to match sorting of Array.sort
        r.Reverse()
        r


    /// <summary>Sorts the elements of an Rarr by mutating the Rarr in-place, using the given comparison function.
    /// Elements are compared using <see cref="M:Microsoft.FSharp.Core.Operators.compare"/>.
    /// This means in ascending sorting "Z" is before "a". This is different from Collections.Generic.List.Sort() where "a" is before "Z" using IComparable interface.
    /// This is NOT a stable sort, i.e. the original order of equal elements is not necessarily preserved.
    /// For a stable sort, consider using Seq.sort.</summary>
    /// <param name="rarr">The input Rarr.</param>
    let sortInPlace<'T when 'T : comparison> (rarr : Rarr<'T>) : unit = 
        rarr.Sort(Operators.compare) // Operators.compare is need to match sorting of Array.sort


    /// <summary>Sorts the elements of an Rarr by mutating the Rarr in-place, using the given projection for the keys.
    /// Elements are compared using <see cref="M:Microsoft.FSharp.Core.Operators.compare"/>.
    /// This means in ascending sorting "Z" is before "a". This is different from Collections.Generic.List.Sort() where "a" is before "Z" using IComparable interface.
    /// This is NOT a stable sort, i.e. the original order of equal elements is not necessarily preserved.
    /// For a stable sort, consider using Seq.sort.</summary>
    /// <param name="projection">The function to transform Rarr elements into the type that is compared.</param>
    /// <param name="rarr">The input Rarr.</param>
    let sortInPlaceBy<'T, 'Key when 'Key : comparison> (projection : 'T -> 'Key) (rarr : Rarr<'T>) : unit = 
        rarr.Sort (fun x y -> Operators.compare (projection x) (projection y))

    /// <summary>Sorts the elements of an Rarr by mutating the Rarr in-place, using the given comparison function as the order.
    /// This is NOT a stable sort, i.e. the original order of equal elements is not necessarily preserved.
    /// For a stable sort, consider using Seq.sort.</summary>
    /// <param name="comparer">The function to compare pairs of Rarr elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    let sortInPlaceWith (comparer : 'T -> 'T -> int) (rarr : Rarr<'T>) : unit = 
        rarr.Sort (comparer)

    /// <summary>Sorts the elements of an Rarr, using the given comparison function as the order, returning a new Rarr.
    /// This is NOT a stable sort, i.e. the original order of equal elements is not necessarily preserved.
    /// For a stable sort, consider using Seq.sort.</summary>
    /// <param name="comparer">The function to compare pairs of Rarr elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The sorted Rarr.</returns>
    let sortWith (comparer : 'T -> 'T -> int) (rarr : Rarr<'T>) : Rarr<'T> = 
        let r = rarr.GetRange(0,rarr.Count) // fastest way to create a shallow copy
        r.Sort (comparer)
        r

    /// <summary>Splits an Rarr into two Rarrs, at the given index.</summary>
    /// <param name="index">The index at which the Rarr is split.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The two split Rarrs.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when split index exceeds the number of elements in the Rarr.</exception>
    let splitAt index (rarr: Rarr<'T>) = 
        if index < 0 || index > rarr.Count then ArgumentException.RaiseBase "Rarr.splitAt: index %d is not in range  0 to rarr.Count-1 (%d)" index (rarr.Count-1)
        rarr.GetRange(0, index), rarr.GetRange(index, rarr.Count-index)


    /// <summary>Splits the input Rarr into at most <c>chunkCount</c> chunks.
    /// If the list can not be split evenly the initial elements will be one bigger than the later elements. Just like with Array.splitInto.</summary>
    /// <param name="chunkCount">The maximum number of chunks.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The Rarr split into chunks.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when <c>count</c> is not positive.</exception>
    let splitInto (chunkCount:int) (rarr: Rarr<'T>) : Rarr<Rarr<'T>> = 
        if chunkCount < 1  then ArgumentException.RaiseBase "Rarr.splitInto: count %d is less than 1" chunkCount
        let len = rarr.Count
        if len = 0 then
            Rarr(0)
        else
            let mutable chunksize = 
                (float len) / (float chunkCount)
                |> ceil
                |> int
            let oneBiggerFor = len % chunkCount // so the first few list might be bigger
            let mutable k = 0
            let mutable sub = Rarr(chunksize)
            let res = Rarr(chunkCount)
            res.Add(sub)
            for i = 0 to len - 1 do
                //printfn "i %d v: %d chunksize %d" i rarr.[i] chunksize
                if k = chunksize  then
                    sub <- Rarr(chunksize)
                    res.Add(sub)
                    k <- 0
                    if res.Count = oneBiggerFor + 1  then // reduce list size once
                        chunksize <- chunksize - 1
                sub.Add(rarr.[i])
                k <- k+1
            res


    /// <summary>Returns the sum of the elements in the Rarr.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The resulting sum.</returns>
    let inline sum (rarr: ^T Rarr ) : ^T = 
        let mutable acc = LanguagePrimitives.GenericZero< ^T>
        let li = rarr.List
        for i = 0 to rarr.Count - 1 do
            acc <- Checked.(+) acc li.[i]
        acc


    /// <summary>Returns the sum of the results generated by applying the function to each element of the Rarr.</summary>
    /// <param name="projection">The function to transform the Rarr elements into the type to be summed.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The resulting sum.</returns>
    let inline sumBy (projection: 'T -> ^Key) (rarr: Rarr<'T>) : ^Key = 
        let mutable acc = LanguagePrimitives.GenericZero< ^Key>
        let li = rarr.List
        for i = 0 to rarr.Count - 1 do
            acc <- Checked.(+) acc (projection li.[i])
        acc


    /// <summary>Returns a new Rarr containing the elements of the original except the first element.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the Rarr is empty.</exception>
    /// <returns>A new Rarr containing the elements of the original except the first element.</returns>
    let tail (rarr: Rarr<'T>) = 
        if rarr.Count = 0  then ArgumentException.RaiseBase "Rarr.tail: input Rarr is empty"
        rarr.GetRange(1,rarr.Count-1)


    /// <summary>Returns the first N elements of the Rarr.
    /// Throws <c>ArgumentException</c> if the count exceeds the number of elements in the Rarr.
    /// Use <c>Rarr.truncate</c> to returns as many items as the Rarr contains instead of throwing an exception.</summary>
    /// <param name="count">The number of items to take.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The result Rarr.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarr is empty or count exceeds the number of elements in the list.</exception>
    let take count (rarr: Rarr<'T>) = 
        if count < 0 then  ArgumentException.RaiseBase "Rarr.take: count %d cannot be negative." count
        if count = 0 then
            Rarr(0)
        else
            if count > rarr.Count then
                ArgumentException.RaiseBase "Rarr.take: count %d > rarr.Count %d." count rarr.Count
            rarr.GetRange(0,count)



    /// <summary>Returns an Rarr that contains all elements of the original Rarr while the
    /// given predicate returns <c>true</c>, and then returns no further elements.</summary>
    /// <param name="predicate">A function that evaluates to false when no more items should be returned.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The result Rarr.</returns>
    let takeWhile (predicate:'T->bool) (rarr: Rarr<'T>) = 
        let li = rarr.List
        if rarr.Count = 0 then
            Rarr(0)
        else
            let mutable count = 0
            while count < rarr.Count && predicate li.[count] do
                count <- count + 1
            rarr.GetRange(0,count)



    /// <summary>Builds a list from the given Rarr.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The list of Rarr elements.</returns>
    let toList (rarr:Rarr<'T>) : list<'T> = 
        let li = rarr.List
        let mutable res = []
        for i = rarr.Count - 1 downto 0 do
            res <- li.[i] :: res
        res


    /// <summary>Views the given Rarr as a sequence. using Seq.readonly </summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The sequence of Rarr elements.</returns>
    let toSeq (rarr:Rarr<'T>) : seq<'T>= 
        Seq.readonly  rarr


    /// <summary>Returns the transpose of the given sequence of Rarrs.</summary>
    /// <param name="rarrs">The input sequence of Rarrs.</param>
    /// <returns>The transposed Rarr.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>
    let transpose (rarrs: Rarr<Rarr<'T>>) : Rarr<Rarr<'T>> = 
        // originally let transpose (rarrs: seq<Rarr<'T>>) : Rarr<Rarr<'T>> = 
        let len = rarrs.Count
        if len = 0 then
            Rarr(0)
        else
            let lenInner = rarrs.[0].Count
            for j in 1..len-1 do
                if lenInner <> rarrs.[j].Count then
                    ArgumentException.RaiseBase "Rarr.transpose: the count %d in sub Rarr %d does not match the count of the first Rarr %d." rarrs.[j].Count j lenInner
            let result = Rarr(lenInner)
            for i in 0..lenInner-1 do
                let sub = Rarr(len)
                result.Add(sub)
                for j in 0..len-1 do
                    sub.Add(rarrs.List.[j].List.[i])
            result


    /// <summary>Returns at most N elements in a new Rarr. When count is negative return empty Rarr.</summary>
    /// <param name="count">The maximum number of items to return.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The result Rarr.</returns>
    let truncate count (rarr: Rarr<'T>) : Rarr<'T> = 
        if count <= 0 then Rarr(0)
        else
            let c = Operators.min count rarr.Count
            rarr.GetRange(0, c)



    /// <summary>Returns the only element of the Rarr or <c>None</c> if Rarr is empty or contains more than one element.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The only element of the Rarr or <c>None</c>.</returns>
    let tryExactlyOne (rarr: Rarr<'T>) :option<'T> = 
        if rarr.Count = 1 then Some rarr.List.[0]
        else None


    /// <summary>Returns the first element for which the given function returns <c>true</c>.
    /// Return None if no such element exists.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The first element that satisfies the predicate, or <c>None</c>.</returns>
    let tryFind (predicate:'T->bool) (rarr: Rarr<'T>) :option<'T> = 
        let elementIndex = 
            rarr.FindIndex (System.Predicate predicate)
        match elementIndex with
        | -1 ->
            None
        | index ->
            Some rarr.List.[index]


    /// <summary>Returns the last element for which the given function returns <c>true</c>.
    /// Return None if no such element exists.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The last element that satisfies the predicate, or <c>None</c>.</returns>
    let tryFindBack (predicate:'T->bool) (rarr: Rarr<'T>) :option<'T> = 
        let li = rarr.List
        let rec loop i = 
            if i < 0 then None else
            if predicate li.[i] then Some li.[i]  else loop (i-1)
        loop ((rarr.Count-1))


    /// <summary>Returns the index of the first element in the Rarr
    /// that satisfies the given predicate.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The index of the first element that satisfies the predicate, or <c>None</c>.</returns>
    let tryFindIndex (predicate:'T->bool) (rarr: Rarr<'T>) : option<int>= 
        let elementIndex =   rarr.FindIndex (System.Predicate predicate)
        match elementIndex with
        | -1 ->
            None
        | index ->
            Some index


    /// <summary>Returns the index of the last element in the Rarr
    /// that satisfies the given predicate.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The index of the last element that satisfies the predicate, or <c>None</c>.</returns>
    let tryFindIndexBack (predicate:'T->bool) (rarr: Rarr<'T>) : option<int>= 
        let li = rarr.List
        let rec loop i = 
            if i < 0 then None else
            if predicate li.[i] then Some i  else loop (i-1)
        loop ((rarr.Count-1))


    /// <summary>Returns the first element of the Rarr, or
    /// <c>None</c> if the Rarr is empty.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The first element of the Rarr or <c>None</c>.</returns>
    let tryHead (rarr: Rarr<'T>) = 
        if rarr.Count = 0 then None
        else Some rarr.List.[0]


    /// <summary>Tries to find the nth element in the Rarr.
    /// Returns <c>None</c> if index is negative or the input Rarr does not contain enough elements.</summary>
    /// <param name="index">The index of element to retrieve.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The nth element of the Rarr or <c>None</c>.</returns>
    let tryItem index (rarr: Rarr<'T>) = 
        if index < 0 || index >= rarr.Count then None
        else Some(rarr.List.[index])


    /// <summary>Returns the last element of the Rarr.
    /// Return <c>None</c> if no such element exists.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The last element of the Rarr or <c>None</c>.</returns>
    let tryLast (rarr: Rarr<'T>) = 
        if rarr.Count = 0 then None
        else Some rarr.List.[rarr.Count-1]


    /// <summary>Applies the given function to successive elements, returning the first
    /// result where function returns <c>Some(x)</c> for some <c>x</c>. If the function
    /// never returns <c>Some(x)</c> then <c>None</c> is returned.</summary>
    /// <param name="chooser">The function to transform the Rarr elements into options.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The first transformed element that is <c>Some(x)</c>.</returns>
    let tryPick chooser (rarr: Rarr<'T>) = 
        let li = rarr.List
        let rec loop i = 
            if i >= rarr.Count then
                None
            else
                match chooser li.[i] with
                | None -> loop(i+1)
                | res -> res
        loop 0


    /// <summary>Returns an Rarr that contains the elements generated by the given computation.
    /// The given initial <c>state</c> argument is passed to the element generator.</summary>
    /// <param name="generator">A function that takes in the current state and returns an option tuple of the next
    /// element of the Rarr and the next state value.</param>
    /// <param name="state">The initial state value.</param>
    /// <returns>The result Rarr.</returns>
    let unfold<'T, 'State> (generator: 'State -> ('T*'State) option) (state: 'State) = 
        let res = Rarr()
        let rec loop state = 
            match generator state with
            | None -> ()
            | Some (x, s') ->
                res.Add(x)
                loop s'
        loop state
        res


    /// <summary>Splits an Rarr of pairs into two Rarrs.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The two Rarrs.</returns>
    let unzip (rarr: Rarr<'T*'U>) : Rarr<'T> * Rarr<'U>= 
        let len = rarr.Count
        let res1 = Rarr(len)
        let res2 = Rarr(len)
        let li = rarr.List
        for i = 0 to rarr.Count-1 do
            let x, y = li.[i]
            res1.Add <|  x
            res2.Add <|  y
        res1, res2


    /// <summary>Splits an Rarr of triples into three Rarrs.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The tuple of three Rarrs.</returns>
    let unzip3 (rarr: Rarr<'T*'U*'V>) = 
        let len = rarr.Count
        let res1 = Rarr(len)
        let res2 = Rarr(len)
        let res3 = Rarr(len)
        let li = rarr.List
        for i = 0 to rarr.Count-1 do
            let x, y, z = li.[i]
            res1.Add <|  x
            res2.Add <|  y
            res3.Add <|  z
        res1, res2, res3

    /// <summary>Return a new Rarr with the item at a given index set to the new value.(does NOT modify in place !)</summary>
    /// <param name="index">The index of the item to be replaced.</param>
    /// <param name="value">The new value.</param>
    /// <param name="source">The input Rarr.</param>
    /// <returns>The result Rarr.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when index is not within  source.Count </exception>
    let updateAt (index: int) (value: 'T) (source: Rarr<'T>) : Rarr<'T> = 
        if index < 0 || index >= source.Count  then ArgumentException.RaiseBase "Rarr.updateAt: index %d  not within source.Count %d." index  source.Count
        let r = source.Clone()
        r.List.[index] <- value
        r


    /// <summary>Returns a new Rarr containing only the elements of the Rarr
    /// for which the given predicate returns <c>true</c>.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>An Rarr containing the elements for which the given predicate returns true.</returns>
    let where (predicate:'T->bool) (rarr: Rarr<'T>) = 
        filter predicate rarr


    /// <summary>Returns an Rarr of sliding windows containing elements drawn from the input Rarr. Each window is returned as a fresh Rarr.</summary>
    /// <param name="windowSize">The number of elements in each window.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The result Rarr.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when windowSize is not positive.</exception>
    let windowed windowSize (rarr: Rarr<'T>) = 
        if windowSize <= 0 then  ArgumentException.RaiseBase "Rarr.windowed: windowSize %d cannot be negative or 0." windowSize
        let len = rarr.Count
        if windowSize > len then
            Rarr(0)
        else
            let res=Rarr(len - windowSize + 1)
            for i = 0 to len - windowSize do
                res.Add <|  rarr.GetRange(i, windowSize)
            res


    /// <summary>Combines the two Rarrs into an Rarr of pairs. The two Rarrs must have equal lengths, otherwise an <c>ArgumentException</c> is raised.</summary>
    /// <param name="rarr1">The first input Rarr.</param>
    /// <param name="rarr2">The second input Rarr.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>
    /// <returns>The Rarr of tupled elements.</returns>
    let zip (rarr1: Rarr<'T>) (rarr2: Rarr<'U>) = 
        let len1 = rarr1.Count
        if len1 <> rarr2.Count then ArgumentException.RaiseBase "Rarr.zip: count of rarr1 %d does not match rarr2 %d." rarr1.Count rarr2.Count
        let res = Rarr(len1)
        for i = 0 to rarr1.Count-1 do
            res.Add (rarr1.List.[i], rarr2.List.[i])
        res


    /// <summary>Combines three Rarrs into an Rarr of pairs. The three Rarrs must have equal lengths, otherwise an <c>ArgumentException</c> is raised.</summary>
    /// <param name="rarr1">The first input Rarr.</param>
    /// <param name="rarr2">The second input Rarr.</param>
    /// <param name="rarr3">The third input Rarr.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>
    /// <returns>The Rarr of tupled elements.</returns>
    let zip3 (rarr1: Rarr<'T>) (rarr2: Rarr<'U>) (rarr3: Rarr<'V>) = 
        let len1 = rarr1.Count
        if len1 <> rarr2.Count || len1 <> rarr3.Count then ArgumentException.RaiseBase "Rarr.zip3: count of rarr1 %d does not match rarr2 %d or rarr3 %d." rarr1.Count rarr2.Count rarr3.Count
        let res = Rarr(len1)
        for i = 0 to rarr1.Count-1 do
            res.Add (rarr1.List.[i], rarr2.List.[i], rarr3.List.[i])
        res

    /// Parallel operations on Rarr using Threading.Tasks.Parallel.For
    module Parallel = 

        open System.Threading.Tasks

        /// <summary>Apply the given function to each element of the Rarr. Return
        /// the Rarr comprised of the results "x" for each element where
        /// the function returns Some(x).
        /// Performs the operation in parallel using <see cref="M:System.Threading.Tasks.Parallel.For" />.
        /// The order in which the given function is applied to elements of the input Rarr is not specified.</summary>
        /// <param name="chooser">The function to generate options from the elements.</param>
        /// <param name="rarr">The input Rarr.</param>
        /// <returns>The Rarr of results.</returns>
        let choose (chooser: 'T -> option<'U> ) (rarr: Rarr<'T>) : Rarr<'U>= 
            let inputLength = rarr.Count
            let isChosen: bool[] = Array.zeroCreate inputLength
            let results: 'U []   = Array.zeroCreate inputLength
            let mutable outputLength = 0
            let li = rarr.List
            Parallel.For(0,
                            inputLength,
                            (fun () -> 0),
                            (fun i _ count ->
                                match chooser li.[i] with
                                | None -> count
                                | Some v ->
                                    isChosen.[i] <-  true
                                    results.[i]  <-  v
                                    count + 1),
                                Action<int> (fun x -> System.Threading.Interlocked.Add(&outputLength, x) |> ignore )
                                ) |> ignore

            let output = Rarr(outputLength)
            for i = 0 to isChosen.Length - 1 do
                if isChosen.[i] then
                    output.Add results.[i]
            output



        /// <summary>For each element of the Rarr, apply the given function. Concatenate all the results and return the combined Rarr.
        /// Performs the operation in parallel using <see cref="M:System.Threading.Tasks.Parallel.For" />.
        /// The order in which the given function is applied to elements of the input Rarr is not specified.</summary>
        /// <param name="mapping"></param>
        /// <param name="rarr">The input Rarr.</param>
        /// <returns>'U[]</returns>
        let collect (mapping: 'T -> Rarr<'U>)  (rarr: Rarr<'T>) : Rarr<'U>= 
            let inputLength = rarr.Count
            let result = create inputLength Unchecked.defaultof<_>
            let li = rarr.List
            Parallel.For(0, inputLength,
                (fun i -> result.[i] <- mapping li.[i])) |> ignore
            concat result


        /// <summary>Create an Rarr given the dimension and a generator function to compute the elements.
        /// Performs the operation in parallel using <see cref="M:System.Threading.Tasks.Parallel.For" />.
        /// The order in which the given function is applied to indices is not specified.</summary>
        /// <param name="count"></param>
        /// <param name="initializer"></param>
        /// <returns>The Rarr of results.</returns>
        let init count (initializer:int->'T) :Rarr<'T> = 
            let result = create count Unchecked.defaultof<_>
            Parallel.For (0, count, fun i -> result.[i] <- initializer i) |> ignore
            result


        /// <summary>Apply the given function to each element of the Rarr.
        /// Performs the operation in parallel using <see cref="M:System.Threading.Tasks.Parallel.For" />.
        /// The order in which the given function is applied to elements of the input Rarr is not specified.</summary>
        /// <param name="action"></param>
        /// <param name="rarr">The input Rarr.</param>
        let iter (action:'T->unit) (rarr: Rarr<'T>) = 
            let li = rarr.List
            Parallel.For (0, rarr.Count, fun i -> action li.[i]) |> ignore


        /// <summary>Apply the given function to each element of the Rarr. The integer passed to the
        /// function indicates the index of element.
        /// Performs the operation in parallel using <see cref="M:System.Threading.Tasks.Parallel.For" />.
        /// The order in which the given function is applied to elements of the input Rarr is not specified.</summary>
        /// <param name="action"></param>
        /// <param name="rarr">The input Rarr.</param>
        let iteri (action:int->'T->unit) (rarr: Rarr<'T>) = 
            let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(action)
            let li = rarr.List
            Parallel.For (0, rarr.Count, fun i -> f.Invoke(i, li.[i])) |> ignore


        /// <summary>Build a new Rarr whose elements are the results of applying the given function
        /// to each of the elements of the Rarr.
        /// Performs the operation in parallel using <see cref="M:System.Threading.Tasks.Parallel.For" />.
        /// The order in which the given function is applied to elements of the input Rarr is not specified.</summary>
        /// <param name="mapping"></param>
        /// <param name="rarr">The input Rarr.</param>
        /// <returns>The Rarr of results.</returns>
        let map (mapping: 'T -> 'U) (rarr: Rarr<'T>) : Rarr<'U>= 
            let inputLength = rarr.Count
            let result = create inputLength Unchecked.defaultof<_>
            let li = rarr.List
            Parallel.For(0, inputLength, fun i ->
                result.[i] <- mapping li.[i]) |> ignore
            result


        /// <summary>Build a new Rarr whose elements are the results of applying the given function
        /// to each of the elements of the Rarr. The integer index passed to the
        /// function indicates the index of element being transformed.
        /// Performs the operation in parallel using <see cref="M:System.Threading.Tasks.Parallel.For" />.
        /// The order in which the given function is applied to elements of the input Rarr is not specified.</summary>
        /// <param name="mapping"></param>
        /// <param name="rarr">The input Rarr.</param>
        /// <returns>The Rarr of results.</returns>
        let mapi (mapping:int-> 'T -> 'U) (rarr: Rarr<'T>) = 
            let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(mapping)
            let inputLength = rarr.Count
            let result = create inputLength  Unchecked.defaultof<_>
            let li = rarr.List
            Parallel.For(0, inputLength, fun i ->
                result.[i] <- f.Invoke (i, li.[i])) |> ignore
            result


        /// <summary>Split the collection into two collections, containing the
        /// elements for which the given predicate returns <c>true</c> and <c>false</c>
        /// respectively
        /// Performs the operation in parallel using <see cref="M:System.Threading.Tasks.Parallel.For" />.
        /// The order in which the given function is applied to indices is not specified.</summary>
        /// <param name="predicate">The function to test the input elements.</param>
        /// <param name="rarr">The input Rarr.</param>
        /// <returns>The two Rarrs of results.</returns>
        let partition (predicate:'T->bool) (rarr: Rarr<'T>) = 
            let inputLength = rarr.Count
            let isTrue = Array.zeroCreate inputLength
            let mutable trueLength = 0
            let li = rarr.List
            Parallel.For(   0,
                            inputLength,
                            (fun () -> 0),
                            (fun i _ trueCount ->
                            if predicate li.[i] then
                                isTrue.[i] <- true
                                trueCount + 1
                            else
                                trueCount),
                            Action<int> (fun x -> System.Threading.Interlocked.Add(&trueLength, x) |> ignore) ) |> ignore

            let res1 = Rarr(trueLength)
            let res2 = Rarr(inputLength - trueLength)

            for i = 0 to isTrue.Length-1 do
                if isTrue.[i] then
                    res1.Add li.[i]
                else
                    res2.Add li.[i]

            res1, res2




