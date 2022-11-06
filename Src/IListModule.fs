namespace FsEx

open System
open System.Collections.Generic
open NiceString

/// Generic operations on System.Collections.Generic.IList interface.
/// This module contains all functios from the Rarr module, except those where in Place mutation take place
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] //need this so doesn't hide IList class in C# assemblies (should consider for other extension modules as well)
[<RequireQualifiedAccess>]
module IList = 

    //---------------------------------------------------
    //        extensions added only in FsEx: (not in FSharp.Core Array module)
    //----------------------------------------------------
    type IList<'T> with 
        member this.ToNiceStringLong = 
            NiceString.toNiceStringLong this


    /// Gets an item in the IList by index.
    /// Allows for negative index too ( -1 is last item,  like Python)
    /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
    let inline getNeg index  (iList: IList<'T>)= 
        let len = iList.Count
        let ii =  if index < 0 then len + index else index
        if ii<0 || ii >= len then ArgumentException.RaiseBase "FsEx.IList.GetNeg: Failed to get index %d from IList of %d items: %s" index iList.Count iList.ToNiceStringLong
        iList.[ii] // access List directly to not check index twice

    /// Sets an item in the IList by index.
    /// Allows for negative index too ( -1 is last item,  like Python)
    /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
    let inline setNeg index value (iList: IList<'T>)= 
        let len = iList.Count
        let ii =  if index < 0 then len + index else index
        if ii<0 || ii >= len then ArgumentException.RaiseBase "FsEx.IList.SetNeg: Failed to set index %d to %s from IList of %d items: %s" index (toNiceString value) iList.Count iList.ToNiceStringLong
        iList.[ii] <- value     // access List directly to not check index twice

    /// Any index will return a value.
    /// IList is treated as an endless loop in positive and negative direction
    let inline getLooped index  (iList: IList<'T>)= 
        let len = iList.Count
        if len=0 then ArgumentException.RaiseBase "FsEx.IList.GetLooped: Failed to get index %d from IList of 0 items" index
        let t = index % len
        let ii = if t >= 0 then t  else t + len
        iList.[ii]   // access List directly to not check index twice

    /// Any index will set a value.
    /// IList is treated as an endless loop in positive and negative direction
    let inline setLooped index value  (iList: IList<'T>) = 
        let len = iList.Count
        if len=0 then ArgumentException.RaiseBase "FsEx.IList.SetLooped: Failed to Set index %d to %s in IList of 0 items" index (toNiceString value)
        let t = index % len
        let ii = if t >= 0 then t  else t + len
        iList.[ii] <- value // access List directly to not check index twice

    /// Get and remove last item from IList
    let inline pop  (iList: IList<'T>)  = 
        if iList.Count=0 then ArgumentException.RaiseBase "Failed to pop from %s" iList.ToNiceStringLong
        let i = iList.Count - 1
        let v = iList.[i] // access List directly to not check index twice
        iList.RemoveAt(i)
        v

    /// Gets the second last item in the IList.
    /// Same as  this.[this.Count - 2]
    let inline secondLast  (iList: IList<'T>)= 
        if iList.Count < 2 then  IndexOutOfRangeException.Raise "FsEx.IList.secondLast: Failed to get second last item of %s" iList.ToNiceStringLong
        iList.[iList.Count - 2]

    /// Gets the third last item in the IList.
    /// Same as   this.[this.Count - 3]
    let inline thirdLast  (iList: IList<'T>)= 
        if iList.Count < 3 then  IndexOutOfRangeException.Raise "FsEx.IList.thirdLast: Failed to get third last item of %s" iList.ToNiceStringLong
        iList.[iList.Count - 3]

    /// Gets the first item in the IList.
    /// Same as   this.[0]
    let inline first  (iList: IList<'T>)= 
        if iList.Count = 0 then IndexOutOfRangeException.Raise "FsEx.IList.first: Failed to get first item of %s" iList.ToNiceStringLong
        iList.[0]

    /// Gets the the only item in the FsEx.IList.
    /// Fails if the IList does not have exactly one element.
    let inline firstAndOnly (iList: IList<'T>) =
        if iList.Count = 0 then IndexOutOfRangeException.Raise  "FsEx.IList.firstOnly: Failed to get first item of empty IList<%s>" (typeof<'T>).FullName
        if iList.Count > 1 then IndexOutOfRangeException.Raise  "FsEx.IList.firstOnly: IList<%s> is expected to have only one item but has %d IList: %s" (typeof<'T>).FullName iList.Count iList.ToNiceStringLong
        iList.[0]

    /// Gets the second item in the IList.
    /// Same as   this.[1]
    let inline second  (iList: IList<'T>)= 
        if iList.Count < 2 then IndexOutOfRangeException.Raise  "IList.second: Failed to get second item of %s" iList.ToNiceStringLong
        iList.[1]

    /// Gets the third item in the IList.
    /// Same as   this.[2]
    let inline third  (iList: IList<'T>)= 
        if iList.Count < 3 then IndexOutOfRangeException.Raise "FsEx.IList.third: Failed to get third item of %s" iList.ToNiceStringLong
        iList.[2]
    
    /// Slice the IList given start and end index.
    /// Allows for negative indices too. ( -1 is last item, like Python)
    /// The resulting IList includes the end index.
    /// Raises an ArgumentException if indices are out of range.
    /// If you don't want an exception to be raised for index overflow use IList.trim.
    /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
    let inline slice (startIdx:int) (endIdx:int) (iList: IList<'T>) : Rarr<'T> = 
        let st = negIdx startIdx iList.Count
        let en = negIdx endIdx iList.Count
        let r = new Rarr<'T>(endIdx-startIdx)
        for i = st to en do r.Add iList.[i] 
        r    
    
    /// Trim items from start and end.
    /// If the sum of fromStartCount and fromEndCount is bigger than iList.Count it returns an empty IList.
    /// If you want an exception to be raised for index overflow use IList.slice with negative end index.
    let inline trim fromStartCount fromEndCount (iList: IList<'T>) : Rarr<'T> = 
        let c = iList.Count
        if fromStartCount + fromEndCount >= c then Rarr<'T>(0)
        else 
            rarr{ for i = fromStartCount to c-fromStartCount-fromEndCount do iList.[i] }             
    
    //------------------------------------------------------------------
    //---------------------prev-this-next ------------------------------
    //------------------------------------------------------------------
    // these functions below also exist on Seq module:

    /// Yields Seq from (first, second)  up to (second-last, last).
    /// Not looped.
    /// The resulting seq is one element shorter than the input IList.
    let windowed2 (iList:IList<'T>) =         
        if iList.Count <= 2 then ArgumentException.RaiseBase "FsEx.IList.windowed2 input has less than two items:\r\n%s" iList.ToNiceStringLong
        seq {   for i = 0 to iList.Count-2 do   iList.[i], iList.[i+1] }

    /// Yields looped Seq from (first, second)  up to (last, first).
    /// The resulting seq has the same element count as the input IList.
    let thisNext (iList:IList<'T>) = 
        if iList.Count <= 2 then ArgumentException.RaiseBase "FsEx.IList.thisNext input has less than two items:\r\n%s" iList.ToNiceStringLong
        seq {   for i = 0 to iList.Count-2 do  iList.[i], iList.[i+1]
                iList.[iList.Count-1], iList.[0] }

    /// Yields looped Seq from (last,first)  up to (second-last, last).
    /// The resulting seq has the same element count as the input IList.
    let prevThis (iList:IList<'T>) = 
        if iList.Count <= 2 then ArgumentException.RaiseBase "FsEx.IList.prevThis input has less than two items:\r\n%s" iList.ToNiceStringLong
        seq {   iList.[iList.Count-1], iList.[0]
                for i = 0 to iList.Count-2 do  iList.[i], iList.[i+1] }

    /// Yields Seq from (first, second, third)  up to (third-last, second-last, last).
    /// Not looped.
    /// The resulting seq is two elements shorter than the input IList.
    let windowed3 (iList:IList<'T>) = 
        if iList.Count <= 3 then ArgumentException.RaiseBase "FsEx.IList.windowed3 input has less than three items:\r\n%s" iList.ToNiceStringLong
        seq { for i = 0 to iList.Count-3 do  iList.[i], iList.[i+1], iList.[i+2] }

    /// Yields looped Seq of  from (last, first, second)  up to (second-last, last, first).
    /// The resulting seq has the same element count as the input IList.
    let prevThisNext (iList:IList<'T>) = 
        if iList.Count <= 3 then ArgumentException.RaiseBase "FsEx.IList.prevThisNext input has less than three items:\r\n%s" iList.ToNiceStringLong
        seq{iList.[iList.Count-1], iList.[0], iList.[1]
            for i = 0 to iList.Count-3 do  iList.[i], iList.[i+1], iList.[i+2]
            iList.[iList.Count-2],iList.[iList.Count-1], iList.[0] }

    /// Yields Seq from (0,first, second)  up to (lastIndex-1 , second-last, last).
    /// Not looped.
    /// The resulting seq is one element shorter than the input IList.
    let windowed2i (iList:IList<'T>) = 
        if iList.Count <= 2 then ArgumentException.RaiseBase "FsEx.IList.windowed2i input has less than two items:\r\n%s" iList.ToNiceStringLong
        seq { for i = 0 to iList.Count-2 do  i, iList.[i], iList.[i+1] }

    /// Yields looped Seq  from (0,first, second)  up to (lastIndex, last, first).
    /// The resulting seq has the same element count as the input IList.
    let iThisNext (iList:IList<'T>) = 
        if iList.Count <= 2 then ArgumentException.RaiseBase "FsEx.IList.iThisNext input has less than two items:\r\n%s" iList.ToNiceStringLong
        seq {   for i = 0 to iList.Count-2 do  i, iList.[i], iList.[i+1]
                iList.Count-1, iList.[iList.Count-1], iList.[0] }

    /// Yields Seq from (1, first, second, third)  up to (lastIndex-1 , third-last, second-last, last).
    /// Not looped.
    /// The resulting seq is two elements shorter than the input IList.
    let windowed3i (iList:IList<'T>) = 
        if iList.Count <= 3 then ArgumentException.RaiseBase "FsEx.IList.windowed3i input has less than three items:\r\n%s" iList.ToNiceStringLong
        seq { for i = 0 to iList.Count-3 do  i+1, iList.[i], iList.[i+1], iList.[i+2] }

    /// Yields looped Seq from (1, last, first, second)  up to (lastIndex, second-last, last, first)
    /// The resulting seq has the same element count as the input IList.
    let iPrevThisNext (iList:IList<'T>) = 
        if iList.Count <= 3 then ArgumentException.RaiseBase "FsEx.IList.iPrevThisNext input has less than three items:\r\n%s" iList.ToNiceStringLong
        seq {   0, iList.[iList.Count-1], iList.[0], iList.[1]
                for i = 0 to iList.Count-3 do  i+1, iList.[i], iList.[i+1], iList.[i+2]
                iList.Count-1, iList.[iList.Count-2],iList.[iList.Count-1], iList.[0] }
            
    //------------------------------------------------------------------    
    
    
    /// <summary>Returns a IList that contains one item only.</summary>
    /// <param name="value">The input item.</param>
    /// <returns>The result IList of one item.</returns>
    let inline singleton value = 
        let res = Rarr(1)
        res.Add value
        res

    /// <summary>Considers List circular and move elements up or down
    /// e.g.: rotate +1 [ a, b, c, d] = [ d, a, b, c]
    /// e.g.: rotate -1 [ a, b, c, d] = [ b, c, d, a] </summary>
    /// <param name="amount">How many elements to shift forward. Or backward if number is negative</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The new result IList.</returns>
    let inline rotate amount (iList: IList<'T>) :  IList<'T>  = 
        let r = Rarr(iList.Count)
        let li = iList
        for i = 0 to li.Count - 1 do
            r.Add <| li.[negIdxLooped (i-amount) iList.Count]
        r

    /// Structural equality.
    /// Compares each element in both lists for equality. ILists must also be of same Count
    let equals (this: IList<'T>) (other: IList<'T>) :bool = 
        if Object.ReferenceEquals(this,other) then true
        elif this.Count <> other.Count then false
        else
            let comparer = EqualityComparer<'T>.Default // for  structural equality to be implemented on this class without putting the <'T when 'T : equality> constraint on 'T?
            let rec eq i = 
                if i < this.Count then
                    if comparer.Equals(this.[i] , other.[i]) then
                        eq (i+1)
                    else
                        false
                else
                    true
            eq 0

    /// Returns true if the given IList has just one item.
    /// Same as  IList.hasOne
    let inline isSingleton (iList : IList<'T>) : bool = 
        iList.Count = 1

    /// Returns true if the given IList has just one item.
    /// Same as  IList.isSingleton
    let inline hasOne (iList : IList<'T>) : bool = 
        iList.Count = 1

    /// Returns true if the given IList is not empty.
    let inline isNotEmpty (iList : IList<'T>) : bool = 
        iList.Count <> 0

    /// Returns true if the given IList has count items.
    let inline hasItems count (iList : IList<'T>) : bool = 
        iList.Count = count

    /// Returns true if the given IList has equal or more than count items.
    let inline hasMinimumItems count (iList : IList<'T>) : bool = 
        iList.Count >= count

    /// Returns true if the given IList has equal or less than count items.
    let inline hasMaximumItems count (iList : IList<'T>) : bool = 
        iList.Count <= count

    /// Swap the values of two given indices in IList
    let inline swap i j (xs:IList<'T>) : unit = 
        if i < 0 then IndexOutOfRangeException.Raise "FsEx.IList.swap: index i can't be less than 0: %d (j: %d)" i j
        if i >= xs.Count then IndexOutOfRangeException.Raise "FsEx.IList.swap: index i can't be bigger than %d but is %d (j: %d)" (xs.Count-1) i j
        if i<>j then
            if j < 0 then IndexOutOfRangeException.Raise "FsEx.IList.swap: index j can't be less than 0: %d (i: %d)" j i
            if j >= xs.Count then IndexOutOfRangeException.Raise "FsEx.IList.swap: index j can't be bigger than %d but is %d (i: %d)" (xs.Count-1) j i
            // operate on underlying list since indices are checked
            let ti = xs.[i]
            xs.[i] <- xs.[j]
            xs.[j] <- ti


    /// internal only for finding
    module private MinMax = 
        //TODO test keeping of order if equal !

        (*
        let inline simple cmpF (xs:IList<'T>) = 
            if xs.Count < 1 then ArgumentException.RaiseBase "FsEx.IList.MinMax.simple: Count must be at least one: %s"  xs.ToNiceStringLong
            let mutable m = xs.[0]
            for i=1 to xs.Count-1 do
                if cmpF xs.[i] m then m <- xs.[i]
            m
        *)

        let inline simple2 cmpF (xs:IList<'T>) = 
            if xs.Count < 2 then ArgumentException.RaiseBase "FsEx.IList.MinMax.simple2: Count must be at least two: %s"  xs.ToNiceStringLong
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

        let inline simple3 cmpF (xs:IList<'T>) = 
            if xs.Count < 3 then ArgumentException.RaiseBase "FsEx.IList.MinMax.simple3: Count must be at least three: %s"  xs.ToNiceStringLong
            let e1 = xs.[0]
            let e2 = xs.[1]
            let e3 = xs.[2]
            // sort first 3
            let mutable m1, m2, m3 =  sort3 cmpF e1 e2 e3   // otherwise would fail on sorting first 3, test on Rarr([5;6;3;1;2;0])|> IList.max3
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

        let inline indexByFun cmpF func (xs:IList<'T>) = 
            if xs.Count < 1 then ArgumentException.RaiseBase "FsEx.IList.MinMax.indexByFun: Count must be at least one: %s"  xs.ToNiceStringLong
            let mutable f = func xs.[0]
            let mutable mf = f
            let mutable ii = 0
            let li = xs
            for i=1 to xs.Count-1 do
                f <- func li.[i]
                if cmpF f mf then
                    ii <- i
                    mf <- f
            ii

        let inline index2ByFun cmpF func (xs:IList<'T>) = 
            if xs.Count < 2 then ArgumentException.RaiseBase "FsEx.IList.MinMax.index2ByFun: Count must be at least two: %s"  xs.ToNiceStringLong
            let mutable i1 = 0
            let mutable i2 = 1
            let mutable mf1 = func xs.[i1]
            let mutable mf2 = func xs.[i2]
            let mutable f = mf1 // placeholder
            let li = xs
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


        let inline index3ByFun (cmpOp:'U->'U->bool)  (byFun:'T->'U) (xs:IList<'T>) = 
            if xs.Count < 3 then ArgumentException.RaiseBase "FsEx.IList.MinMax.index3ByFun: Count must be at least three: %s"  xs.ToNiceStringLong
            // sort first 3
            let mutable i1,i2,i3 =  indexOfSort3By byFun cmpOp xs.[0] xs.[1] xs.[2] // otherwise would fail on sorting first 3, test on Rarr([5;6;3;1;2;0])|> IList.max3
            let mutable e1 =  byFun xs.[i1]
            let mutable e2 =  byFun xs.[i2]
            let mutable e3 =  byFun xs.[i3]
            let mutable f = e1 // placeholder
            let li = xs
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


    (* covered by part copied from Array module:
        let min iList =     iList |> MinMax.simple (<)
        let max iList =     iList |> MinMax.simple (>)
        let minBy f iList = let i = iList |> MinMax.indexByFun (<) f in iList.[i]
        let maxBy f iList = let i = iList |> MinMax.indexByFun (>) f in iList.[i]
        *)

    /// <summary>Returns the index of the smallest of all elements of the IList, compared via Operators.max on the function result.</summary>
    /// <param name="projection">The function to transform the elements into a type supporting comparison.</param>
    /// <param name="iList">The input IList.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input IList is empty.</exception>
    /// <returns>The index of the smallest element.</returns>
    let inline minIndBy  (projection : 'T -> 'Key) (iList: IList<'T>) : int = 
        iList |> MinMax.indexByFun (<) projection

    /// <summary>Returns the index of the greatest of all elements of the IList, compared via Operators.max on the function result.</summary>
    /// <param name="projection">The function to transform the elements into a type supporting comparison.</param>
    /// <param name="iList">The input IList.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input IList is empty.</exception>
    /// <returns>The index of the maximum element.</returns>
    let inline maxIndBy (projection : 'T -> 'Key) (iList: IList<'T>) : int =
        iList |> MinMax.indexByFun (>) projection

    /// Returns the smallest two elements of the IList.
    /// If they are equal then the  order is kept
    let inline min2 iList = iList |> MinMax.simple2 (<)

    /// Returns the biggest two elements of the IList.
    /// If they are equal then the  order is kept
    let inline max2 iList = iList |> MinMax.simple2 (>)

    // TODO make consistent xml docstring on below functions:

    /// Returns the smallest two elements of the IList.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the order is kept
    let inline min2By f iList = 
        let i,ii = iList |> MinMax.index2ByFun (<) f
        iList.[i],iList.[ii]

    /// Returns the biggest two elements of the IList.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the order is kept
    let inline max2By f iList = 
        let i,ii = iList |> MinMax.index2ByFun (>) f
        iList.[i],
        iList.[ii]

    /// Returns the indices of the two smallest elements of the IList.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the order is kept
    let inline min2IndBy f iList = iList |> MinMax.index2ByFun (<) f

    /// Returns the indices of the two biggest elements of the IList.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the order is kept
    let inline max2IndBy f iList = iList |> MinMax.index2ByFun (>) f

    /// Returns the smallest three elements of the IList.
    /// If they are equal then the order is kept
    let inline min3 iList =  iList |> MinMax.simple3 (<)

    /// Returns the biggest three elements of the IList.
    /// If they are equal then the order is kept
    let inline max3 iList =  iList |> MinMax.simple3 (>)

    /// Returns the smallest three elements of the IList.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the order is kept
    let inline min3By f iList = 
        let i,ii,iii = iList |> MinMax.index3ByFun (<) f
        iList.[i],
        iList.[ii],
        iList.[iii]

    /// Returns the biggest three elements of the IList.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the order is kept
    let inline max3By f iList = 
        let i,ii,iii = iList |> MinMax.index3ByFun (>) f
        iList.[i],
        iList.[ii],
        iList.[iii]

    /// Returns the indices of the three smallest elements of the IList.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the order is kept
    let inline min3IndBy f iList = iList |> MinMax.index3ByFun(<) f

    /// Returns the indices of the three biggest elements of the IList.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the order is kept
    let inline max3IndBy f iList = iList |> MinMax.index3ByFun (>) f

    /// Return the length or count of the collection.
    /// Same as  IList.length
    let inline count (iList : IList<'T>) : int = 
        iList.Count

    /// Counts for how many items of the collection the predicate returns true.
    /// Same as IList.filter and then IList.length
    let inline countIf (predicate : 'T -> bool) (iList : IList<'T>) : int = //countBy is something else !!
        let mutable k = 0
        let li = iList
        for i=0 to li.Count - 1 do
            if predicate li.[i] then
                k <- k + 1
        k

    /// Adds an object to the end of the IList.
    let inline add item (iList : IList<'T>) : unit = 
        iList.Add item


    /// <summary>
    /// Splits the collection into two (2) collections, containing the elements for which the
    /// given function returns <c>Choice1Of2</c> or <c>Choice2Of2</c>, respectively. This function is similar to
    /// <c>IList.partition</c>, but it allows the returned collections to have different element types.</summary>
    let inline mapPartition partitioner (iList : IList<'T>) : Rarr<'U1> * Rarr<'U2> = 
        let results1 = Rarr ()
        let results2 = Rarr ()
        let len = iList.Count
        let li = iList
        for i = 0 to len - 1 do
            match partitioner li.[i] with
            | Choice1Of2 value ->
                results1.Add value
            | Choice2Of2 value ->
                results2.Add value
        results1, results2

    /// Applies a function to List
    /// If resulting List meets the resultPredicate it is returned , otherwise  original input is returned.
    let inline applyIfResult (resultPredicate:IList<'T> -> bool) (transform:IList<'T> -> IList<'T>)  (iList: IList<'T>) : IList<'T> = 
        let r = transform iList
        if resultPredicate r then r
        else iList

    /// Applies a function to List if it meets the inputPredicate, otherwise just returns input.
    /// If resulting List meets the resultPredicate it is returned , otherwise original input is returned.
    let inline applyIfInputAndResult (inputPredicate:IList<'T> -> bool) (resultPredicate:IList<'T> -> bool) (transform:IList<'T> -> IList<'T>)  (iList: IList<'T>) : IList<'T> = 
        if inputPredicate iList then
            let r = transform iList
            if resultPredicate r then r
            else iList
        else
            iList
    
    /// Returns all elements that exists more than once in IList.
    /// Each element that exists more than once is only returned once.
    /// Returned order is by first occurrence of first duplicate.
    let duplicates (xs:IList<'T>) = 
        let h = Hashset<'T>()
        let t = Hashset<'T>() 
        let r = Rarr()
        for i = 0 to xs.Count - 1 do
            let x = xs.[i]
            // first Add should be false, second Add true, to recognize the first occurrence of a duplicate:        
            let isDup = if h.Add x then false else t.Add x
            if isDup then 
                r.Add x
        r
    
    /// Returns all elements that exists more than once in IList.
    /// Each element that exists more than once is only returned once.
    /// Returned order is by first occurrence of first duplicate.
    let duplicatesBy (f:'T->'U) (xs:IList<'T>) = 
        let h = Hashset<'U>()
        let t = Hashset<'U>()
        let r = Rarr()
        for i = 0 to xs.Count - 1 do
            let x = xs.[i]
            let u = f x
            // first Add should be false, second Add true, to recognize the first occurrence of a duplicate:        
            let isDup = if h.Add u then false else t.Add u
            if isDup then 
                r.Add x
        r
    
    /// <summary>Splits the collection into three collections, 
    /// first  containing the elements for which the given predicate1 returns <c>true</c> ,
    /// second containing the elements for which the given predicate2 returns <c>true</c> (and all previous predicates returned <c>false</c>),
    /// third the rest.</summary>
    /// <param name="predicate1">The first function to test the input elements.</param>
    /// <param name="predicate2">The second function to test the input elements.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>Three Rarrs. </returns>
    let partition3 (predicate1:'T->bool) (predicate2:'T->bool) (iList : IList<'T>) : Rarr<'T> * Rarr<'T>* Rarr<'T> = 
        let p1True  = Rarr()
        let p2True = Rarr()
        let allFalse = Rarr()
        let len = iList.Count
        let li = iList
        for i = 0 to len - 1 do
            let el = li.[i]
            if predicate1 el then
                p1True.Add el
            else
                if predicate2 el then
                    p2True.Add el
                else
                    allFalse.Add el
        p1True, p2True, allFalse

    /// <summary>Splits the collection into four collections, 
    /// first  containing the elements for which the given predicate1 returns <c>true</c> ,
    /// second containing the elements for which the given predicate2 returns <c>true</c> (and all previous predicates returned <c>false</c>),
    /// third  containing the elements for which the given predicate3 returns <c>true</c> (and all previous predicates returned <c>false</c>),
    /// fourth the rest.</summary>
    /// <param name="predicate1">The first  function to test the input elements.</param>
    /// <param name="predicate2">The second function to test the input elements.</param>
    /// <param name="predicate3">The third  function to test the input elements.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>Four Rarrs. </returns>
    let partition4 (predicate1:'T->bool) (predicate2:'T->bool) (predicate3:'T->bool) (iList : IList<'T>) : Rarr<'T> * Rarr<'T>* Rarr<'T>* Rarr<'T> = 
        let p1True  = Rarr()
        let p2True = Rarr()
        let p3True = Rarr()
        let allFalse = Rarr()
        let len = iList.Count
        let li = iList
        for i = 0 to len - 1 do
            let el = li.[i]
            if predicate1 el then
                p1True.Add el
            else
                if predicate2 el then
                    p2True.Add el
                else
                    if predicate3 el then
                        p3True.Add el
                    else
                        allFalse.Add el

        p1True, p2True, p3True, allFalse

    /// <summary>Splits the collection into five collections, 
    /// first  containing the elements for which the given predicate1 returns <c>true</c> ,
    /// second containing the elements for which the given predicate2 returns <c>true</c> (and all previous predicates returned <c>false</c>),
    /// third  containing the elements for which the given predicate3 returns <c>true</c> (and all previous predicates returned <c>false</c>),
    /// fourth containing the elements for which the given predicate4 returns <c>true</c> (and all previous predicates returned <c>false</c>),
    /// fifth the rest.</summary>
    /// <param name="predicate1">The first  function to test the input elements.</param>
    /// <param name="predicate2">The second function to test the input elements.</param>
    /// <param name="predicate3">The third  function to test the input elements.</param>
    /// <param name="predicate4">The fourth function to test the input elements.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>Five Rarrs. </returns>
    let partition5 (predicate1:'T->bool) (predicate2:'T->bool) (predicate3:'T->bool) (predicate4:'T->bool)  (iList : IList<'T>) : Rarr<'T> * Rarr<'T>* Rarr<'T>* Rarr<'T>* Rarr<'T> = 
        let p1True = Rarr()
        let p2True = Rarr()
        let p3True = Rarr()
        let p4True = Rarr()
        let allFalse = Rarr()
        let len = iList.Count
        let li = iList
        for i = 0 to len - 1 do
            let el = li.[i]
            if predicate1 el then
                p1True.Add el
            else
                if predicate2 el then
                    p2True.Add el
                else
                    if predicate3 el then
                        p3True.Add el
                    else
                        if predicate4 el then
                            p4True.Add el
                        else
                            allFalse.Add el

        p1True, p2True, p3True, p4True, allFalse

    //--------------------------------------------------------------------------------------------------------------------
    // ------------- implementation adapted form FSharp.Core Array module: ------------------------------------------------
    //
    //               alternatives:
    //               https://github.com/fsprojects/FSharpx.Collections/blob/master/src/FSharpx.Collections/ResizeArray.fs
    //               https://github.com/jack-pappas/ExtCore/blob/master/ExtCore/Collections.IList.fs
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

    /// <summary>Returns a new IList that contains all pairings (or combinations)  of elements from the first and second Rarrs.</summary>
    /// <param name="iList1">The first input IList.</param>
    /// <param name="iList2">The second input IList.</param>
    /// <returns>The resulting IList of pairs of length: iList1.Count * iList2.Count.</returns>
    let allPairs (iList1: IList<'T>) (iList2: IList<'U>) :Rarr<'T*'U> = 
        let res = Rarr(iList1.Count * iList2.Count)
        let l1 = iList1
        let l2 = iList2
        for i = 0 to iList1.Count-1 do
            for j = 0 to iList2.Count-1 do
                res.Add (l1.[i], l2.[j])
        res


    /// <summary>Builds a new IList that contains the elements of the first IList followed by the elements of the second IList.</summary>
    /// <param name="iList1">The first input IList.</param>
    /// <param name="iList2">The second input IList.</param>
    /// <returns>The resulting IList of length: iList1.Count + iList2.Count..</returns>
    let inline append (iList1: IList<'T>) (iList2: IList<'T>) = 
        let res = Rarr.ofIList iList1
        res.AddRange(iList2)
        res


    /// <summary>Returns the average of the elements in the IList.</summary>
    /// <param name="iList">The input IList.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when <c>IList</c> is empty.</exception>
    /// <returns>The average of the elements in the IList.</returns>
    let inline average (iList: IList<'T>) = 
        if iList.Count = 0 then ArgumentException.RaiseBase "FsEx.IList.average: Count must be at least one: %s" iList.ToNiceStringLong
        let mutable acc = LanguagePrimitives.GenericZero< ^T>
        let li = iList
        for i = 0 to li.Count - 1 do
            acc <- Checked.(+) acc li.[i]
        LanguagePrimitives.DivideByInt< ^T> acc iList.Count


    /// <summary>Returns the average of the elements generated by applying the function to each element of the IList.</summary>
    /// <param name="projection">The function to transform the IList elements before averaging.</param>
    /// <param name="iList">The input IList.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when <c>IList</c> is empty.</exception>
    /// <returns>The computed average.</returns>
    let inline averageBy (projection: 'T -> ^Key) (iList: IList<'T>) : ^Key = 
        if iList.Count = 0 then ArgumentException.RaiseBase "FsEx.IList.averageBy: Count must be at least one: %s" iList.ToNiceStringLong
        let mutable acc = LanguagePrimitives.GenericZero< ^Key>
        let li = iList
        for i = 0 to li.Count - 1 do
            acc <- Checked.(+) acc (projection li.[i])
        LanguagePrimitives.DivideByInt< ^Key> acc iList.Count

    /// <summary>Applies the given function to each element of the IList. 
    /// Returns the IList comprised of the results "x" for each element where
    /// the function returns Some(x)</summary>
    /// <param name="chooser">The function to generate options from the elements.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The IList of results.</returns>
    let inline choose (chooser : 'T -> 'U option) (iList : IList<'T>) : Rarr<'U> = 
        let result = Rarr()        
        let li = iList
        for i = 0 to li.Count - 1 do
            match chooser li.[i] with
            | None -> ()
            | Some value ->
                result.Add value
        result

    /// <summary>Divides the input IList into chunks of size at most <c>chunkSize</c>.</summary>
    /// <param name="chunkSize">The maximum size of each chunk.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The IList divided into chunks.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when <c>chunkSize</c> is not positive.</exception>
    let chunkBySize chunkSize (iList: IList<'T>) : Rarr<Rarr<'T>> = 
        if chunkSize <= 0 then ArgumentException.RaiseBase "FsEx.IList.chunkBySize: chunkSize %d must be bigger than 0" chunkSize
        let len = iList.Count
        if len = 0 then
            Rarr(0)
        elif chunkSize > len then
            Rarr.singleton (Rarr.ofIList iList)
        else
            let chunkCount = (len - 1) / chunkSize + 1
            let res = Rarr(chunkCount)
            let mutable sub = Rarr(0)
            let li = iList
            for i=0 to li.Count - 1 do
                if i % chunkSize = 0 then
                    sub <- Rarr(chunkSize)
                    res.Add(sub)
                sub.Add li.[i]
            res

    /// <summary>For each element of the IList, applies the given function. Concatenates all the results and return the combined IList.</summary>
    /// <param name="mapping">The function to create sub-Rarrs from the input IList elements.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The concatenation of the sub-Rarrs.</returns>
    let collect (mapping: 'T -> IList<'U>)  (iList: IList<'T>) : Rarr<'U> =         
        let res = Rarr(iList.Count)
        for e in iList do
            res.AddRange(mapping e)
        res

    /// <summary>Compares two Rarrs using the given comparison function, element by element.</summary>
    /// <param name="comparer">A function that takes an element from each IList and returns an int.
    /// If it evaluates to a non-zero value iteration is stopped and that value is returned.</param>
    /// <param name="iList1">The first input IList.</param>
    /// <param name="iList2">The second input IList.</param>
    /// <returns>Returns the first non-zero result from the comparison function. If the first IList has
    /// a larger element, the return value is always positive. If the second IList has a larger
    /// element, the return value is always negative. When the elements are equal in the two
    /// Rarrs, 1 is returned if the first IList is longer, 0 is returned if they are equal in
    /// length, and -1 is returned when the second IList is longer.</returns>
    let inline compareWith ((*[<InlineIfLambda>]*) comparer: 'T -> 'T -> int) (iList1: IList<'T>) (iList2: IList<'T>) = 
        let length1 = iList1.Count
        let length2 = iList2.Count
        let mutable i = 0
        let mutable result = 0
        if length1 < length2 then
            while i < iList1.Count && result = 0 do
                result <- comparer iList1.[i] iList2.[i]
                i <- i + 1
        else
            while i < iList2.Count && result = 0 do
                result <- comparer iList1.[i] iList2.[i]
                i <- i + 1
        if result <> 0 then result
        elif length1 = length2 then 0
        elif length1 < length2 then -1
        else 1


    /// <summary>Builds a new IList that contains the elements of each of the given sequence of sequences.</summary>
    /// <param name="seqOfSeq">The input sequence of Rarrs.</param>
    /// <returns>The concatenation of the sequence of input Rarrs.</returns>
    let concat (seqOfSeq: seq<#seq<'T>>) : Rarr<'T>=         
        if Seq.isEmpty seqOfSeq then
            Rarr(0)
        else
            let res = Rarr() //iLists.[0].Clone()
            for r in seqOfSeq do res.AddRange(r)
            //for i=1 to iLists.(xs.Count-1) do res.AddRange(iLists.[i])
            res

    /// <summary>Tests if the IList contains the specified element.</summary>
    /// <param name="value">The value to locate in the input IList.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns><c>true</c> if the input IList contains the specified element; false otherwise.</returns>
    let inline contains value (iList: IList<'T>) = 
        iList.Contains(value)


    /// <summary>Reads a range of elements from the first IList and write them into the second. The target IList must already have the required minimum size to fit targetStartIndex + count.</summary>
    /// <param name="source">The source IList.</param>
    /// <param name="sourceIndex">The starting index of the source IList.</param>
    /// <param name="target">The target IList.</param>
    /// <param name="targetStartIndex">The starting index of the target IList.</param>
    /// <param name="count">The number of elements to copy.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when any of sourceIndex,targetStartIndex or count are negative, or when there aren't enough elements in source or target.</exception>
    let inline blit (source: IList<'T>) (sourceIndex: int) (target: IList<'T>) (targetStartIndex: int) (count: int) :unit= 
        if sourceIndex < 0  then  ArgumentException.RaiseBase "FsEx.IList.blit: sourceIndex %d cannot be negative." sourceIndex
        if targetStartIndex < 0  then  ArgumentException.RaiseBase "FsEx.IList.blit:targetStartIndex %d cannot be negative." targetStartIndex
        if count < 0  then  ArgumentException.RaiseBase "FsEx.IList.blit: count %d cannot be negative." count
        if source.Count < sourceIndex + count then  ArgumentException.RaiseBase "FsEx.IList.blit: source.Count %d is smaller than  sourceIndex %d + count %d."   source.Count  sourceIndex  count
        if target.Count < targetStartIndex + count then  ArgumentException.RaiseBase "FsEx.IList.blit: target.Count %d is smaller than  targetStartIndex %d + count %d."  target.Count  targetStartIndex count
        let mutable j = targetStartIndex
        for i = sourceIndex to sourceIndex + count - 1 do
            target.[j] <- source.[i]
            j <-j+1

    /// <summary>Reads a range of elements from the first IList and write them into the second. The target IList increases in size if needed.
    /// But it needs to have  minimum <c>targetStartIndex</c> elements already.</summary>
    /// <param name="source">The source IList.</param>
    /// <param name="sourceIndex">The starting index of the source IList.</param>
    /// <param name="target">The target IList.</param>
    /// <param name="targetStartIndex">The starting index of the target IList.</param>
    /// <param name="count">The number of elements to copy.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when any of sourceIndex, targetStartIndex or count are negative, or when there aren't enough elements in source.</exception>
    let inline blitExtend (source: IList<'T>) (sourceIndex: int) (target: IList<'T>) (targetStartIndex: int) (count: int) :unit= 
        if sourceIndex < 0  then  ArgumentException.RaiseBase "FsEx.IList.blit: sourceIndex %d cannot be negative." sourceIndex
        if targetStartIndex < 0  then  ArgumentException.RaiseBase "FsEx.IList.blit: targetStartIndex %d cannot be negative." targetStartIndex
        if count < 0  then  ArgumentException.RaiseBase "FsEx.IList.blit: count %d cannot be negative." count
        if source.Count < sourceIndex + count then  ArgumentException.RaiseBase "FsEx.IList.blit: source.Count %d is smaller than  sourceIndex %d + count %d." source.Count sourceIndex  count
        if target.Count < targetStartIndex then  ArgumentException.RaiseBase "FsEx.IList.blit: target.Count %d is smaller than  targetStartIndex %d." target.Count targetStartIndex
        let mutable j = targetStartIndex
        let tlasti = target.Count-1
        for i = sourceIndex to sourceIndex + count - 1 do
            if j > tlasti then
                target.Add(source.[i]) //increases in size if needed
            else
                target.[j] <- source.[i]
            j<-j+1


    let inline private countByImpl (comparer: IEqualityComparer<'SafeKey>) (projection: 'T->'SafeKey) (getKey: 'SafeKey->'Key) (iList: IList<'T>) :Rarr<'Key*int> = 
        let length = iList.Count
        if length = 0 then
            Rarr(0)
        else
            let dict = Dictionary comparer
            // Build the groupings
            for v in iList do
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
    /// <summary>Applies a key-generating function to each element of a IList and returns a IList yielding unique
    /// keys and their number of occurrences in the original IList.</summary>
    /// <param name="projection">A function transforming each item of the input IList into a key to be
    /// compared against the others.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The result IList.</returns>
    let countBy (projection: 'T->'Key) (iList: IList<'T>) : Rarr<'Key * int> = 
        if typeof<'Key>.IsValueType
            // We avoid wrapping a StructBox, because under 64 JIT we get some "hard" tail-calls which affect performance
            then countByImpl HashIdentity.Structural<'Key> projection id iList

            // Wrap a StructBox around all keys in case the key type is itself a type using null as a representation
            else countByImpl StructBox<'Key>.Comparer (fun t -> StructBox (projection t)) (fun sb -> sb.Value) iList


    /// <summary>Creates a IList whose elements are all initially the given value.</summary>
    /// <param name="count">The length of the IList to create.</param>
    /// <param name="value">The value for the elements.</param>
    /// <returns>The created IList.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when count is negative.</exception>
    let create (count: int) (value: 'T) = 
        if count < 0 then  ArgumentException.RaiseBase "FsEx.IList.create: count %d cannot be negative." count
        let iList= Rarr(count)
        for i = 0 to count-1 do
            iList.Add value
        iList


    /// <summary>Returns a IList that contains no duplicate entries according to generic hash and
    /// equality comparisons on the entries.
    /// If an element occurs multiple times in the IList then the later occurrences are discarded.</summary>
    /// <param name="iList">The input IList.</param>
    /// <returns>The result IList.</returns>
    let distinct (iList: IList<'T>) = 
        let temp = Rarr()
        let hashSet = HashSet<'T>(HashIdentity.Structural<'T>)
        let li = iList
        for i=0 to li.Count - 1 do
            let v = li.[i]
            if hashSet.Add(v) then
                temp.Add v
        temp


    /// <summary>Returns a IList that contains no duplicate entries according to the
    /// generic hash and equality comparisons on the keys returned by the given key-generating function.
    /// If an element occurs multiple times in the IList then the later occurrences are discarded.</summary>
    /// <param name="projection">A function transforming the IList items into comparable keys.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The result IList.</returns>
    let distinctBy (projection:'T -> 'Key) (iList: IList<'T>) : Rarr<'T> = 
        if iList.Count < 2 then
            Rarr.ofIList iList // 0 or 1 item, just clone
        else
            let temp = Rarr()
            let hashSet = HashSet<'Key>(HashIdentity.Structural<_>)
            let li = iList
            for i=0 to li.Count - 1 do
                let v = li.[i]
                if hashSet.Add(projection v) then
                    temp.Add v
            temp



    /// <summary>Returns the only element of the IList.</summary>
    /// <param name="iList">The input IList.</param>
    /// <returns>The only element of the IList.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when the input does not have precisely one element.</exception>
    let exactlyOne (iList: IList<'T>) = 
        if iList.Count = 1 then iList.[0]
        else ArgumentException.RaiseBase "FsEx.IList.exactlyOne: IList has %d elements, not one." iList.Count


    /// <summary>Returns a new list with the distinct elements of the input IList which do not appear in the itemsToExclude sequence.
    /// Uses generic hash and equality comparisons to compare values.</summary>
    /// <param name="itemsToExclude">A sequence whose elements that also occur in the input IList will cause those elements to be
    /// removed from the result.</param>
    /// <param name="iList">A IList whose elements that are not also in itemsToExclude will be returned.</param>
    /// <returns>A IList that contains the distinct elements of <c>IList</c> that do not appear in <c>itemsToExclude</c>.</returns>
    let except (itemsToExclude: seq<'T>) (iList: IList<'T>) : Rarr<'T> = 
        if iList.Count = 0 then
            Rarr(0)
        else
            let cached = HashSet(itemsToExclude, HashIdentity.Structural)
            let res = Rarr()
            let li = iList
            for i=0 to li.Count - 1 do
                let e = li.[i]
                if cached.Add e then
                    res.Add e
            res

    /// <summary>Tests if any element of the IList satisfies the given predicate.
    /// The predicate is applied to the elements of the input IList. If any application
    /// returns true then the overall result is <c>true</c> and no further elements are tested.
    /// Otherwise, false is returned.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns><c>true</c> if any result from <c>predicate</c> is <c>true</c>.</returns>
    let exists (predicate: 'T -> bool) (iList: IList<'T>) :bool = 
        let rec search i = i < iList.Count && predicate iList.[i] || search (i+1)
        search 0
        


    /// <summary>Tests if any pair of corresponding elements of the Rarrs satisfies the given predicate.
    /// The predicate is applied to matching elements in the two collections up to the lesser of the
    /// two lengths of the collections. If any application returns true then the overall result is
    /// true and no further elements are tested. Otherwise, if one collections is longer
    /// than the other then the <c>ArgumentException</c> exception is raised.
    /// Otherwise, false is returned.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="iList1">The first input IList.</param>
    /// <param name="iList2">The second input IList.</param>
    /// <returns><c>true</c> if any result from <c>predicate</c> is <c>true</c>.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>
    let exists2 (predicate:'T->'U->bool) (iList1: IList<'T>) (iList2: IList<'U>) :bool = 
        let len1 = iList1.Count
        if len1 <> iList2.Count then ArgumentException.RaiseBase "FsEx.IList.exists2: count of iList1 %d does not match iList2 %d." iList1.Count iList2.Count
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(predicate)
        let rec loop i = i < len1 && (f.Invoke(iList1.[i], iList2.[i]) || loop (i+1))
        loop 0

    /// <summary>Fills a range of elements of the IList with the given value. Extends the IList if needed</summary>
    /// <param name="target">The target IList.</param>
    /// <param name="startIndex">The index of the first element to set.</param>
    /// <param name="count">The number of elements to set.</param>
    /// <param name="value">The value to set.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when either startIndex or count is negative.</exception>
    let fill (target: IList<'T>) (startIndex: int) (count: int) (value: 'T) = 
        if startIndex < 0  then  ArgumentException.RaiseBase "FsEx.IList.fill: startIndex %d cannot be negative." startIndex
        if count < 0  then  ArgumentException.RaiseBase "FsEx.IList.fill: count %d cannot be negative." count
        if target.Count < startIndex then  ArgumentException.RaiseBase "FsEx.IList.fill: target.Count %d is smaller than  startIndex %d."   target.Count  startIndex
        let tlasti = target.Count-1
        for j = startIndex to startIndex + count - 1 do
            if j > tlasti then
                target.Add(value)
            else
                target.[j] <- value

    /// <summary>Returns a new collection containing only the elements of the collection
    /// for which the given predicate returns <c>true</c>.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>A IList containing the elements for which the given predicate returns true.</returns>
    let inline filter (predicate:'T->bool) (iList: IList<'T>) = 
        let r = Rarr()
        for i = 0 to iList.Count - 1 do
            let x = iList.[i]
            if predicate x then 
                r.Add x
        r


    /// <summary>Returns the first element for which the given function returns <c>true</c>.
    /// Raise <see cref="T:System.Collections.Generic.KeyNotFoundException"/> if no such element exists.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="iList">The input IList.</param>
    /// <exception cref="T:System.Collections.Generic.KeyNotFoundException">Thrown if <c>predicate</c> never returns true.</exception>
    /// <returns>The first element for which <c>predicate</c> returns true.</returns>
    let find (predicate : 'T -> bool) (iList: IList<'T>) = 
        let rec search (i)=
            if i = iList.Count       then 
                KeyNotFoundException.Raise "FsEx.IList.find did not find for predicate in IList of %d items." iList.Count
            elif predicate iList.[i] then iList.[i]
            else search(i+1)
        search(0)
        
        
      

    /// <summary>Returns the last element for which the given function returns <c>true</c>.
    /// Raise <see cref="T:System.Collections.Generic.KeyNotFoundException"/> if no such element exists.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="iList">The input IList.</param>
    /// <exception cref="T:System.Collections.Generic.KeyNotFoundException">Thrown if <c>predicate</c> never returns true.</exception>
    /// <returns>The last element for which <c>predicate</c> returns true.</returns>
    let findBack (predicate:'T->bool) (iList: IList<'T>) = 
        let li = iList
        let rec loop i = 
            if i < 0 then
                KeyNotFoundException.Raise "FsEx.IList.findBack: not found in %d items %s" iList.Count iList.ToNiceStringLong
            else
                if predicate li.[i] then li.[i]  else loop (i-1)
        loop (li.Count - 1)


    /// <summary>Returns the index of the first element in the IList that satisfies the given predicate. Raise <see cref="T:System.Collections.Generic.KeyNotFoundException"/> if
    /// none of the elements satisfy the predicate.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="iList">The input IList.</param>
    /// <exception cref="T:System.Collections.Generic.KeyNotFoundException">Thrown if <c>predicate</c> never returns true.</exception>
    /// <returns>The index of the first element in the IList that satisfies the given predicate.</returns>
    let findIndex (predicate:'T->bool) (iList: IList<'T>) = 
        let rec search (i)=
            if i = iList.Count       then 
                KeyNotFoundException.Raise "FsEx.IList.findIndex did not find for predicate in IList of %d items." iList.Count
            elif predicate iList.[i] then i
            else search(i+1)
        search(0)
       


    /// <summary>Returns the index of the last element in the IList
    /// that satisfies the given predicate. Raise <see cref="T:System.Collections.Generic.KeyNotFoundException"/> if
    /// none of the elements satisfy the predicate.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="iList">The input IList.</param>
    /// <exception cref="T:System.Collections.Generic.KeyNotFoundException">Thrown if <c>predicate</c> never returns true.</exception>
    /// <returns>The index of the last element in the IList that satisfies the given predicate.</returns>
    let findIndexBack (predicate:'T->bool) (iList: IList<'T>) = 
        let li = iList
        let rec go n = 
            if n < 0 then
                KeyNotFoundException.Raise "FsEx.IList.findIndexBack: not found in %s" iList.ToNiceStringLong
            elif predicate li.[n] then
                n
            else go (n-1)
        go (li.Count - 1)


    /// <summary>Applies a function to each element of the collection, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes
    /// <c>f (... (f s i0)...) iN</c></summary>
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The final state.</returns>
    let fold<'T, 'State> (folder: 'State -> 'T -> 'State) (state: 'State) (iList: IList<'T>) = 
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(folder)
        let mutable state = state
        let li = iList
        for i = 0 to li.Count - 1 do
            state <- f.Invoke(state, li.[i])
        state


    /// <summary>Applies a function to pairs of elements drawn from the two collections,
    /// left-to-right, threading an accumulator argument
    /// through the computation. The two input
    /// Rarrs must have the same lengths, otherwise an <c>ArgumentException</c> is
    /// raised.</summary>
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="iList1">The first input IList.</param>
    /// <param name="iList2">The second input IList.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>
    /// <returns>The final state.</returns>
    let fold2<'T1, 'T2, 'State>  folder (state: 'State) (iList1: 'T1 IList) (iList2: 'T2  IList) = 
        if iList1.Count <> iList2.Count then ArgumentException.RaiseBase "FsEx.IList.fold2: count of iList1 %d does not match iList2 %d." iList1.Count iList2.Count
        let f = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt(folder)
        let mutable state = state
        for i = 0 to iList1.Count-1 do
            state <- f.Invoke(state, iList1.[i], iList2.[i])
        state


    /// <summary>Applies a function to each element of the IList, starting from the end, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes
    /// <c>f i0 (...(f iN s))</c></summary>
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="iList">The input IList.</param>
    /// <param name="state">The initial state.</param>
    /// <returns>The state object after the folding function is applied to each element of the IList.</returns>
    let foldBack<'T, 'State> (folder: 'T -> 'State -> 'State) (iList: IList<'T>) (state: 'State) = 
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(folder)
        let mutable res = state
        let li = iList
        for i = li.Count - 1 downto 0 do
            res <- f.Invoke(li.[i], res)
        res


    /// <summary>Apply a function to pairs of elements drawn from the two collections, right-to-left,
    /// threading an accumulator argument through the computation. The two input
    /// Rarrs must have the same lengths, otherwise an <c>ArgumentException</c> is
    /// raised.</summary>
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="iList1">The first input IList.</param>
    /// <param name="iList2">The second input IList.</param>
    /// <param name="state">The initial state.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>
    /// <returns>The final state.</returns>
    let foldBack2<'T1, 'T2, 'State>  folder (iList1: 'T1 IList) (iList2: 'T2  IList) (state: 'State) = 
        let len = iList1.Count
        if len <> iList2.Count then ArgumentException.RaiseBase "FsEx.IList.foldBack2: count of iList1 %d does not match iList2 %d." iList1.Count iList2.Count
        let mutable res = state
        let f = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt(folder)
        for i = len-1 downto 0 do
            res <- f.Invoke(iList1.[i], iList2.[i], res)
        res


    /// <summary>Tests if all elements of the IList satisfy the given predicate.
    /// The predicate is applied to the elements of the input collection. If any application
    /// returns false then the overall result is false and no further elements are tested.
    /// Otherwise, true is returned.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns><c>true</c> if all of the IList elements satisfy the predicate.</returns>
    let forall (predicate: 'T -> bool) (iList: IList<'T>) =         
        let rec search (i) = 
            if i = iList.Count             then true
            elif not (predicate iList.[i]) then false
            else search(i+1)
        search(0)


    /// <summary>Tests if all corresponding elements of the IList satisfy the given predicate pairwise.
    /// The predicate is applied to matching elements in the two collections up to the lesser of the
    /// two lengths of the collections. If any application returns false then the overall result is
    /// false and no further elements are tested. Otherwise, if one collection is longer
    /// than the other then the <c>ArgumentException</c> exception is raised.
    /// Otherwise, true is returned.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="iList1">The first input IList.</param>
    /// <param name="iList2">The second input IList.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>
    /// <returns><c>true</c> if all of the IList elements satisfy the predicate.</returns>
    let forall2 (predicate:'T->'U->bool) (iList1: IList<'T>) (iList2: IList<'U>) : bool = 
        let len1 = iList1.Count
        if len1 <> iList2.Count then ArgumentException.RaiseBase "FsEx.IList.forall2: count of iList1 %d does not match iList2 %d." iList1.Count iList2.Count
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(predicate)
        let rec loop i = i >= len1 || (f.Invoke(iList1.[i], iList2.[i]) && loop (i+1))
        loop 0

    /// <summary>Gets an element from an IList. (Use IList.getNeg(i) function if you want to use negative indices too.)</summary>
    /// <param name="iList">The input IList.</param>
    /// <param name="index">The input index.</param>
    /// <returns>The value of the IList at the given index.</returns>
    /// <exception cref="T:System.IndexOutOfRangeException">Thrown when the index is negative or the input IList does not contain enough elements.</exception>
    let inline get (iList: IList<'T>) index = 
        iList.[index]


    /// <summary>Builds a new IList that contains the given sub-range specified by starting index and length.</summary>
    /// <param name="iList">The input IList.</param>
    /// <param name="startIndex">The index of the first element of the sub IList.</param>
    /// <param name="count">The length of the sub IList.</param>
    /// <returns>The created sub IList.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when either startIndex or count is negative, or when there aren't enough elements in the input IList.</exception>
    let sub (iList: IList<'T>) (startIndex: int) (count: int) = 
        let r = new Rarr<'T>(count)
        for i = startIndex to startIndex+count-1 do r.Add iList.[i] 
        r
        

    let inline private groupByImpl (comparer: IEqualityComparer<'SafeKey>) (keyf: 'T->'SafeKey) (getKey: 'SafeKey->'Key) (iList: IList<'T>) :  Rarr<'Key * Rarr<'T>>= 
        let length = iList.Count
        if length = 0 then
            Rarr(0)
        else
            let dict = Dictionary<_, Rarr<_>> comparer
            // Build the groupings
            let li = iList
            for i = 0 to length - 1 do
                let v = li.[i]
                let safeKey = keyf v
                let mutable prev = Unchecked.defaultof<_>
                if dict.TryGetValue(safeKey, &prev) then
                    prev.Add v
                else
                    let prev = Rarr()
                    dict.[safeKey] <- prev
                    prev.Add v
            // Return the iList-of-iLists.
            let result = Rarr(dict.Count)
            for group in dict do
                result.Add( getKey group.Key, group.Value)
            result

    /// <summary>Applies a key-generating function to each element of a IList and yields a IList of
    /// unique keys. Each unique key contains a IList of all elements that match
    /// to this key.</summary>
    /// <param name="projection">A function that transforms an element of the IList into a comparable key. Null or Option.None is allowed as key.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The result IList.</returns>
    let groupBy (projection:'T -> 'Key) (iList:IList<'T>) :  Rarr<'Key * Rarr<'T>> = 
        if typeof<'Key>.IsValueType then
            // We avoid wrapping a StructBox, because under 64 JIT we get some "hard" tail-calls which affect performance
            groupByImpl HashIdentity.Structural<'Key> projection id iList
        else
            // Wrap a StructBox around all keys in case the key type is itself a type using null as a representation
            groupByImpl StructBox<'Key>.Comparer (fun t -> StructBox (projection t)) (fun sb -> sb.Value) iList



    /// <summary>Applies a key-generating function to each element of a IList and yields a Dict of
    /// unique keys and respective elements that match to this key. As opposed to IList.groupBy the key may not be null or Option.None</summary>
    /// <param name="projection">A function that transforms an element of the IList into a comparable key. As opposed to IList.groupBy the key may not be null or Option.None </param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The result IList.</returns>
    let groupByDict (projection:'T -> 'Key) (iList:IList<'T>) :  Dict<'Key,Rarr<'T>> = 
        let dict = Dictionary<'Key, Rarr<'T>>()
        // Build the groupings
        let li = iList
        for i = 0 to li.Count - 1 do
            let v = li.[i]  
            let k = projection v   
            match dict.TryGetValue k with
            |true, r -> r.Add v
            |_ -> 
                let r = Rarr()
                dict.[k] <- r
                r.Add v
        Dict.CreateDirectly dict



    /// <summary>Returns the first element of the IList.</summary>
    /// <param name="iList">The input IList.</param>
    /// <returns>The first element of the IList.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when the input IList is empty.</exception>
    let inline head (iList: IList<'T>) = 
        if iList.Count = 0 then ArgumentException.RaiseBase "FsEx.IList.head: The input IList is empty." else iList.[0]


    /// <summary>Builds a new IList whose elements are the corresponding elements of the input IList
    /// paired with the integer index (from 0) of each element.</summary>
    /// <param name="iList">The input IList.</param>
    /// <returns>The IList of indexed elements.</returns>
    let indexed (iList: IList<'T>) :  Rarr<int * 'T> = 
        let res = Rarr(iList.Count)
        let li = iList
        for i = 0 to li.Count - 1 do
            res.Add (i, li.[i])
        res

    /// <summary>Creates a IList given the dimension and a generator function to compute the elements.</summary>
    /// <param name="count">The number of elements to initialize.</param>
    /// <param name="initializer">The function to generate the initial values for each index.</param>
    /// <returns>The created IList.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when count is negative.</exception>
    let inline init (count:int) (initializer:int->'T) = 
        if count < 0 then ArgumentException.RaiseBase "FsEx.IList.init: count %d is negative." count
        let res = Rarr(count)
        for i = 0 to count-1 do
            res.Add (initializer i)
        res

    /// <summary>Return a new IList with a new item inserted before the given index.(does NOT modify in place !)</summary>
    /// <param name="index">The index where the item should be inserted.</param>
    /// <param name="value">The value to insert.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The result IList.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when index is below not within iList.Count.</exception>
    let insertAt (index: int) (value: 'T) (iList: IList<'T>) : Rarr<'T> = 
        if index < 0 || index > iList.Count then ArgumentException.RaiseBase "FsEx.IList.insertAt: index %d not within iList.Count %d." index iList.Count
        let r = Rarr.ofIList iList //TODO can be optimized by using a loop
        r.Insert(index,value)
        r


    /// <summary>Return a new IList with new items inserted before the given index.(does NOT modify in place !) If required increases the count of the IList.</summary>
    /// <param name="index">The index where the items should be inserted.</param>
    /// <param name="values">The values to insert.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The result IList.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when index is below not within iList.Count.</exception>
    let insertManyAt (index: int) (values: seq<'T>) (iList: IList<'T>) : Rarr<'T> = 
        if index < 0 || index > iList.Count  then ArgumentException.RaiseBase "FsEx.IList.insertManyAt: index %d and  not within iList.Count %d." index iList.Count
        let r = Rarr.ofIList iList //TODO can be optimized by using a loop
        r.InsertRange(index,values)
        r

    /// <summary>Returns true if the given IList is empty, otherwise false.</summary>
    /// <param name="iList">The input IList.</param>
    /// <returns><c>true</c> if the IList is empty.</returns>
    let inline isEmpty (iList: IList<'T>) = 
        iList.Count = 0

    /// <summary>Gets an element from a IList.</summary>
    /// <param name="index">The input index.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The value of the IList at the given index.</returns>
    /// <exception cref="T:System.IndexOutOfRangeException">Thrown when the index is negative or the input IList does not contain enough elements.</exception>
    let inline item index (iList: IList<'T>) = 
        iList.[index]

    /// <summary>Applies the given function to each element of the IList.</summary>
    /// <param name="action">The function to apply.</param>
    /// <param name="iList">The input IList.</param>
    let inline iter ((*[<InlineIfLambda>]*) action) (iList: IList<'T>) = // TODO activate InlineIfLambda
        let li = iList
        for i = 0 to li.Count - 1 do
            action li.[i]


    /// <summary>Applies the given function to pair of elements drawn from matching indices in two Rarrs. The
    /// two Rarrs must have the same lengths, otherwise an <c>ArgumentException</c> is
    /// raised.</summary>
    /// <param name="action">The function to apply.</param>
    /// <param name="iList1">The first input IList.</param>
    /// <param name="iList2">The second input IList.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>
    let inline iter2 (action : 'T -> 'U -> unit) (iList1: IList<'T>) (iList2: IList<'U>) = 
        if iList1.Count <> iList2.Count then ArgumentException.RaiseBase "FsEx.IList.iter2: count of iList1 %d does not match iList2 %d." iList1.Count iList2.Count
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(action)
        for i = 0 to iList1.Count-1 do
            f.Invoke(iList1.[i], iList2.[i])


    /// <summary>Applies the given function to each element of the IList. The integer passed to the
    /// function indicates the index of element.</summary>
    /// <param name="action">The function to apply to each index and element.</param>
    /// <param name="iList">The input IList.</param>
    let inline iteri (action : int -> 'T -> unit)  (iList: IList<'T>) = 
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(action)
        let li = iList
        for i = 0 to li.Count - 1 do
            f.Invoke(i, li.[i])


    /// <summary>Applies the given function to pair of elements drawn from matching indices in two Rarrs,
    /// also passing the index of the elements. The two Rarrs must have the same lengths,
    /// otherwise an <c>ArgumentException</c> is raised.</summary>
    /// <param name="action">The function to apply to each index and pair of elements.</param>
    /// <param name="iList1">The first input IList.</param>
    /// <param name="iList2">The second input IList.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>
    let inline iteri2 (action : int -> 'T -> 'U -> unit)  (iList1: IList<'T>) (iList2: IList<'U>) = 
        if iList1.Count <> iList2.Count then ArgumentException.RaiseBase "FsEx.IList.iteri2: count of iList1 %d does not match iList2 %d." iList1.Count iList2.Count
        let f = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt(action)
        for i = 0 to iList1.Count-1 do
            f.Invoke(i, iList1.[i], iList2.[i])


    /// <summary>Returns the last element of the IList.</summary>
    /// <param name="iList">The input IList.</param>
    /// <returns>The last element of the IList.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when the input does not have any elements.</exception>
    let inline last (iList: IList<'T>) = 
        if iList.Count = 0 then ArgumentException.RaiseBase "FsEx.IList.last: The input IList is empty."
        iList.[iList.Count-1]


    /// <summary>Returns the length of a IList. You can also use property iList.Count.</summary>
    /// <param name="iList">The input IList.</param>
    /// <returns>The length or count of the IList.</returns>
    let inline length (iList: IList<'T>) = 
        iList.Count

    /// <summary>Builds a new IList whose elements are the results of applying the given function
    /// to each of the elements of the IList.</summary>
    /// <param name="mapping">The function to transform elements of the IList.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The IList of transformed elements.</returns>
    let inline map ( mapping: 'T -> 'U) (iList: IList<'T>) : Rarr<'U> = 
        // TODO use [<InlineIfLambda>] for performance?? 
        let r = new Rarr<'U>(iList.Count)
        for i = 0 to iList.Count-1 do r.Add (mapping iList.[i] )
        r        
       

    /// <summary>Builds a new IList whose elements are the results of applying the given function
    /// to each of the elements of the Array.</summary>
    /// <param name="mapping">The function to transform elements of the IList.</param>
    /// <param name="arr">The input Array.</param>
    /// <returns>The IList of transformed elements.</returns>
    let inline mapFromArray ( mapping: 'T -> 'U) (arr: array<'T>) : Rarr<'U> =         
        let res = Rarr<'U>(arr.Length)
        for i = 0 to arr.Length-1 do
            res.Add(mapping arr.[i])
        res
   
    /// <summary>Builds a new IList whose elements are the results of applying the given function
    /// to each of the elements of a collection with IList interface.</summary>
    /// <param name="mapping">The function to transform elements of the IList.</param>
    /// <param name="list">The input collection with IList interface.</param>
    /// <returns>The IList of transformed elements.</returns>
    let inline mapFromIList ( mapping: 'T -> 'U) (list: IList<'T>) : Rarr<'U> =         
        let res = Rarr<'U>(list.Count)
        for i = 0 to list.Count-1 do
            res.Add(mapping list.[i])
        res

    /// <summary>Builds a new IList whose elements are the results of applying the given function
    /// to each of the elements of an IEnumerable.</summary>
    /// <param name="mapping">The function to transform elements of the IList.</param>
    /// <param name="sequence">The input IEnumerable.</param>
    /// <returns>The IList of transformed elements.</returns>
    let inline mapFromSeq ( mapping: 'T -> 'U) (sequence: seq<'T>) : Rarr<'U> =         
        let res = Rarr<'U>()
        use e = sequence.GetEnumerator()        
        while e.MoveNext() do
            res.Add(mapping e.Current)            
        res

    /// <summary>Builds a new collection whose elements are the results of applying the given function
    /// to the corresponding elements of the two collections pairwise. The two input
    /// Rarrs must have the same lengths, otherwise an <c>ArgumentException</c> is
    /// raised.</summary>
    /// <param name="mapping">The function to transform the pairs of the input elements.</param>
    /// <param name="iList1">The first input IList.</param>
    /// <param name="iList2">The second input IList.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>
    /// <returns>The IList of transformed elements.</returns>
    let map2 (mapping: 'T1 ->'T2 ->'U) (iList1: IList<'T1>) (iList2: IList<'T2>) : Rarr<'U> = 
        if iList1.Count <> iList2.Count then ArgumentException.RaiseBase "FsEx.IList.map2: count of iList1 %d does not match iList2 %d." iList1.Count iList2.Count
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(mapping)
        let res = Rarr<'U>(iList1.Count)
        for i = 0 to iList1.Count-1 do
            res.Add(f.Invoke(iList1.[i], iList2.[i]) )
        res

    /// <summary>Builds a new collection whose elements are the results of applying the given function
    /// to the corresponding triples from the three collections. The three input
    /// Rarrs must have the same length, otherwise an <c>ArgumentException</c> is
    /// raised.</summary>
    /// <param name="mapping">The function to transform the pairs of the input elements.</param>
    /// <param name="iList1">The first input IList.</param>
    /// <param name="iList2">The second input IList.</param>
    /// <param name="iList3">The third input IList.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>
    /// <returns>The IList of transformed elements.</returns>
    let map3 (mapping: 'T1 ->'T2 ->'T3 ->'U)  (iList1: IList<'T1> ) (iList2: IList<'T2> ) (iList3: IList<'T3> ) : Rarr<'U> = 
        let len1 = iList1.Count
        if len1 <> iList2.Count || len1 <> iList3.Count then ArgumentException.RaiseBase "FsEx.IList.map3: count of iList1 %d does not match iList2 %d or iList3 %d" iList1.Count iList2.Count iList3.Count
        let f = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt(mapping)
        let res = Rarr(len1)
        for i = 0 to iList1.Count-1 do
            res.Add <|  f.Invoke(iList1.[i], iList2.[i], iList3.[i])
        res


    /// <summary>Combines map and fold. Builds a new IList whose elements are the results of applying the given function
    /// to each of the elements of the input IList. The function is also used to accumulate a final value.</summary>
    /// <param name="mapping">The function to transform elements from the input IList and accumulate the final value.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The IList of transformed elements, and the final accumulated value.</returns>
    let mapFold<'T, 'State, 'Result> (mapping: 'State -> 'T -> 'Result * 'State) state (iList:IList<'T>) : IList<'Result> * 'State = 
        match iList.Count with
        | 0   -> Rarr(0), state
        | len ->
            let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(mapping)
            let mutable acc = state
            let res = Rarr(len)
            let li = iList
            for i = 0 to len-1 do
                let h, s = f.Invoke(acc, li.[i])
                res.Add h
                acc <- s
            res, acc

    /// <summary>Combines map and foldBack. Builds a new IList whose elements are the results of applying the given function
    /// to each of the elements of the input IList. The function is also used to accumulate a final value.</summary>
    /// <param name="mapping">The function to transform elements from the input IList and accumulate the final value.</param>
    /// <param name="iList">The input IList.</param>
    /// <param name="state">The initial state.</param>
    /// <returns>The IList of transformed elements, and the final accumulated value.</returns>
    let mapFoldBack<'T, 'State, 'Result> (mapping: 'T -> 'State -> 'Result * 'State) (iList:IList<'T>) state : IList<'Result> * 'State= 
        match iList.Count with
        | 0 -> Rarr(0), state
        | len ->
            let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(mapping)
            let mutable acc = state
            let res = Rarr(len)
            for i = 0 to len-1 do
                res.Add Unchecked.defaultof<'Result> // needs to be filled already because of 'downto' loop
            let li = iList
            for i = len - 1 downto 0 do
                let h, s = f.Invoke(li.[i], acc)
                res.[i] <- h
                acc <- s
            res, acc

    /// <summary>Builds a new IList whose elements are the results of applying the given function
    /// to each of the elements of the IList. The integer index passed to the
    /// function indicates the index of element being transformed.</summary>
    /// <param name="mapping">The function to transform elements and their indices.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The IList of transformed elements.</returns>
    let mapi (mapping: int -> 'T -> 'U) (iList: IList<'T>) : Rarr<'U> = 
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(mapping)
        let res = Rarr(iList.Count)
        let li = iList
        for i = 0 to li.Count - 1 do
            res.Add <|  f.Invoke(i, li.[i])
        res

    /// <summary>Builds a new collection whose elements are the results of applying the given function
    /// to the corresponding elements of the two collections pairwise, also passing the index of
    /// the elements. The two input Rarrs must have the same lengths, otherwise an <c>ArgumentException</c> is
    /// raised.</summary>
    /// <param name="mapping">The function to transform pairs of input elements and their indices.</param>
    /// <param name="iList1">The first input IList.</param>
    /// <param name="iList2">The second input IList.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>
    /// <returns>The IList of transformed elements.</returns>
    let mapi2 (mapping: int -> 'T1 -> 'T2-> 'U) (iList1: IList<'T1>) (iList2: IList<'T2>) : IList<'U>  = 
        let f = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt(mapping)
        if iList1.Count <> iList2.Count then ArgumentException.RaiseBase "FsEx.IList.mapi2: count of iList1 %d does not match iList2 %d." iList1.Count iList2.Count
        let res = Rarr(iList1.Count)
        for i = 0 to iList1.Count-1 do
            res.Add <|  f.Invoke(i, iList1.[i], iList2.[i])
        res

    /// <summary>Returns the greatest of all elements of the IList, compared via Operators.max on the function result.
    /// Throws ArgumentException for empty Rarrs.</summary>
    /// <param name="iList">The input IList.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input IList is empty.</exception>
    /// <returns>The maximum element.</returns>
    let inline max (iList: IList<'T>) = 
        if iList.Count = 0 then ArgumentException.RaiseBase "FsEx.IList.max: Count must be at least one: %s"  iList.ToNiceStringLong
        let mutable acc = iList.[0]        
        let li = iList
        for i = 1 to li.Count - 1 do
            let curr = li.[i]
            if curr > acc then
                acc <- curr
        acc

    /// <summary>Returns the greatest of all elements of the IList, compared via Operators.max on the function result.</summary>
    /// <param name="projection">The function to transform the elements into a type supporting comparison.</param>
    /// <param name="iList">The input IList.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input IList is empty.</exception>
    /// <returns>The maximum element.</returns>
    let inline maxBy (projection : 'T -> 'Key) (iList: IList<'T>) : 'T = 
        if iList.Count = 0 then ArgumentException.RaiseBase "FsEx.IList.maxBy: Count must be at least one: %s"  iList.ToNiceStringLong
        let mutable accv = iList.[0]
        if iList.Count =  1 then 
            accv // if len = 1 then don't call the projection not even once !
        else 
            let mutable acc = projection accv
            let li = iList
            for i = 1 to li.Count - 1 do
                let currv = li.[i]
                let curr = projection currv
                if curr > acc then
                    acc <- curr
                    accv <- currv
            accv

    /// <summary>Returns the lowest of all elements of the IList, compared via Operators.min.</summary>
    /// <param name="iList">The input IList.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input IList is empty.</exception>
    /// <returns>The minimum element.</returns>
    let inline min (iList: IList<'T>) : 'T= 
        if iList.Count = 0 then ArgumentException.RaiseBase "FsEx.IList.min: Count must be at least one: %s"  iList.ToNiceStringLong
        let mutable acc = iList.[0]
        let li = iList
        for i = 1 to li.Count - 1 do
            let curr = li.[i]
            if curr < acc then
                acc <- curr
        acc

    /// <summary>Returns the lowest of all elements of the IList, compared via Operators.min on the function result.</summary>
    /// <param name="projection">The function to transform the elements into a type supporting comparison.</param>
    /// <param name="iList">The input IList.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input IList is empty.</exception>
    /// <returns>The minimum element.</returns>
    let inline minBy ((*[<InlineIfLambda>]*) projection : 'T -> 'Key) (iList: IList<'T>) : 'T = 
        if iList.Count = 0 then ArgumentException.RaiseBase "FsEx.IList.minBy: Count must be at least one: %s"  iList.ToNiceStringLong
        let mutable accv = iList.[0]
        if iList.Count =  1 then 
            accv // if len = 1 then don't call the projection not even once !
        else
            let mutable acc = projection accv
            let li = iList
            for i = 1 to li.Count - 1 do
                let currv = li.[i]
                let curr = projection currv
                if curr < acc then
                    acc <- curr
                    accv <- currv
            accv


    /// <summary>Returns a IList of each element in the input IList and its predecessor, with the
    /// exception of the first element which is only returned as the predecessor of the second element.</summary>
    /// <param name="iList">The input IList.</param>
    /// <returns>The result IList.</returns>
    let pairwise (iList: IList<'T>) = 
        if iList.Count < 2 then
            Rarr(0)
        else
            init (iList.Count-1) (fun i -> iList.[i], iList.[i+1])


    /// <summary>Splits the collection into two collections, containing the
    /// elements for which the given predicate returns <c>true</c> and <c>false</c>
    /// respectively.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>A pair of Rarrs. The first containing the elements the predicate evaluated to <c>true</c> ,
    /// and the second containing those evaluated to <c>false</c>.</returns>
    let partition (predicate:'T->bool) (iList : IList<'T>) : Rarr<'T> * Rarr<'T> = 
        let trueResults  = Rarr()
        let falseResults = Rarr()
        let len = iList.Count
        let li = iList
        for i = 0 to len - 1 do
            let el = li.[i]
            if predicate el then
                trueResults.Add el
            else
                falseResults.Add el
        trueResults, falseResults


    /// <summary>Returns a IList with all elements permuted according to the specified permutation.</summary>
    /// <param name="indexMap">The function that maps input indices to output indices.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The output IList.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when indexMap does not produce a valid permutation.</exception>
    let permute (indexMap:int -> int) (iList: IList<'T>) : Rarr<'T>= 
        let res  = Rarr.ofIList iList
        let inv = Array.zeroCreate iList.Count
        let li = iList
        for i = 0 to li.Count - 1 do
            let j = indexMap i
            if j < 0 || j >= iList.Count then ArgumentException.RaiseBase "FsEx.IList.permute: the indexMap generated %d from %d but only 0 to %d is allowed" j i (li.Count - 1)
            res.[j] <- li.[i]
            inv.[j] <- 1uy
        for i = 0 to li.Count - 1 do
            if inv.[i] <> 1uy then ArgumentException.RaiseBase "FsEx.IList.permute: the indexMap function did not generated %d a new value for " i
        res


    /// <summary>Applies the given function to successive elements, returning the first
    /// result where function returns <c>Some(x)</c> for some <c>x</c>. If the function
    /// never returns <c>Some(x)</c> then <see cref="T:System.Collections.Generic.KeyNotFoundException"/> is raised.</summary>
    /// <param name="chooser">The function to generate options from the elements.</param>
    /// <param name="iList">The input IList.</param>
    /// <exception cref="T:System.Collections.Generic.KeyNotFoundException">Thrown if every result from
    /// <c>chooser</c> is <c>None</c>.</exception>
    /// <returns>The first result.</returns>
    let pick (chooser:'T -> 'U option) (iList: IList<'T>) = 
        let li = iList
        let rec loop i = 
            if i >= iList.Count then
                KeyNotFoundException.Raise "FsEx.IList.pick: Key not found in %d elements" iList.Count
            else
                match chooser li.[i] with
                | None -> loop(i+1)
                | Some res -> res
        loop 0


    /// <summary>Applies a function to each element of the IList, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c>
    /// then computes <c>f (... (f i0 i1)...) iN</c>.
    /// Raises ArgumentException if the IList has size zero.</summary>
    /// <param name="reduction">The function to reduce a pair of elements to a single element.</param>
    /// <param name="iList">The input IList.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input IList is empty.</exception>
    /// <returns>The final result of the reductions.</returns>
    let reduce (reduction:'T -> 'T -> 'T) (iList: IList<'T>) = 
        if iList.Count = 0 then ArgumentException.RaiseBase "FsEx.IList.reduce: Count must be at least one: %s"  iList.ToNiceStringLong
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(reduction)
        let li = iList
        let mutable res = li.[0]
        for i = 1 to li.Count - 1 do
            res <- f.Invoke(res, li.[i])
        res


    /// <summary>Applies a function to each element of the IList, starting from the end, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c>
    /// then computes <c>f i0 (...(f iN-1 iN))</c>.</summary>
    /// <param name="reduction">A function that takes in the next-to-last element of the list and the
    /// current accumulated result to produce the next accumulated result.</param>
    /// <param name="iList">The input IList.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input IList is empty.</exception>
    /// <returns>The final result of the reductions.</returns>
    let reduceBack (reduction:'T -> 'T -> 'T) (iList: IList<'T>) = 
        if iList.Count = 0 then ArgumentException.RaiseBase "FsEx.IList.reduceBack: Count must be at least one: %s"  iList.ToNiceStringLong
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(reduction)
        let mutable res = iList.Last
        let li = iList
        for i = iList.Count - 2 downto 0 do
            res <- f.Invoke(li.[i], res)
        res



    /// <summary>Return a new IList with the item at a given index removed. (does NOT modify in place !)</summary>
    /// <param name="index">The index of the item to be removed.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The result IList.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when index is outside 0..iList.Length - 1</exception>
    let removeAt (index: int) (iList: IList<'T>) : Rarr<'T> = 
        if index < 0 || index >= iList.Count then ArgumentException.RaiseBase "FsEx.IList.removeAt: index %d not within iList.Count %d." index iList.Count
        let r = Rarr(iList.Count-1)
        for i = 0 to iList.Count-1 do 
            if i<> index then 
                r.Add iList.[i]
        r

    /// <summary>Return a new IList with the number of items starting at a given index removed. (does NOT modify in place !)</summary>
    /// <param name="index">The index of the first item to be removed.</param>
    /// <param name="count">The number of items to remove.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The result IList.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when index is outside 0..iList.Length - count</exception>
    let removeManyAt (index: int) (count: int) (iList: IList<'T>) : Rarr<'T> = 
        if index < 0 || index > iList.Count  - count then ArgumentException.RaiseBase "FsEx.IList.removeManyAt: index %d and count %d not within iList.Count %d." index count iList.Count
        let r = Rarr(iList.Count-count)
        let ei = index+count-1
        for i = 0 to iList.Count-1 do 
            if i<index || i > ei then 
                r.Add iList.[i]
        r

    /// <summary>Creates a IList by replicating the given initial value.</summary>
    /// <param name="count">The number of elements to replicate.</param>
    /// <param name="initial">The value to replicate</param>
    /// <returns>The generated IList.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when count is negative.</exception>
    let replicate count (initial:'T) = 
        if count < 0 then  ArgumentException.RaiseBase "FsEx.IList.replicate: count %d cannot be negative "count
        let arr = Rarr(count)
        for _ = 0 to count-1 do
            arr.Add ( initial)
        arr

    /// <summary>Returns a new IList with the elements in reverse order.</summary>
    /// <param name="iList">The input IList.</param>
    /// <returns>The reversed IList.</returns>
    let rev (iList: IList<'T>) = 
        let len = iList.Count
        let result = Rarr(len)
        let li = iList
        for i = len - 1 downto 0 do
            result.Add li.[i]
        result

    /// <summary>Like <c>fold</c>, but return the intermediary and final results.</summary>
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="stateInit">The initial state.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The IList of state values. This IList has the initial state and the state for all elements in the input IList, so one more item than the input.</returns>
    let scan<'T, 'State> (folder:'State -> 'T -> 'State) (stateInit: 'State) (iList: IList<'T>) :Rarr<'State> = 
        let folder = OptimizedClosures.FSharpFunc<_,_,_>.Adapt folder
        // Holds the initial and intermediate state values.
        let results = Rarr(iList.Count+1)
        results.Add stateInit
        // Fold over the specified range of items.
        let mutable state = stateInit
        let li = iList
        for i = 0 to li.Count - 1 do
            state <- folder.Invoke (state, li.[i])
            results.Add state
        results

    /// <summary>Like <c>foldBack</c>, but return both the intermediary and final results.</summary>
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="iList">The input IList.</param>
    /// <param name="stateInit">The initial state.</param>
    /// <returns>The IList of state values. Count = input count + 1 </returns>
    let scanBack<'T, 'State> (folder:'T -> 'State -> 'State) (iList: IList<'T>) (stateInit: 'State) : Rarr<'State> = 
        let folder = OptimizedClosures.FSharpFunc<_,_,_>.Adapt folder
        let results = Rarr(iList.Count+1)// Holds the initial and intermediate state values.
        for _ = 0 to iList.Count do // to fill up list +1 with default values
            results.Add stateInit
        // Fold over the specified range of items.
        let mutable state = stateInit
        let li = iList
        for i = li.Count - 1 downto 0 do
            state <- folder.Invoke (li.[i],state)
            results.[i] <- state
        results


    /// <summary>Sets an element of a IList. (use IList.setNeg(i) function if you want to use negative indices too)</summary>
    /// <param name="iList">The input IList.</param>
    /// <param name="index">The input index.</param>
    /// <param name="value">The input value.</param>
    /// <exception cref="T:System.IndexOutOfRangeException">Thrown when the index is negative or the input IList does not contain enough elements.</exception>
    let inline set (iList: IList<'T>) index value = 
        iList.[index] <- value



    /// <summary>Builds a new IList that contains the elements of the given IList, excluding the first N elements.</summary>
    /// <param name="count">The number of elements to skip.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>A copy of the input IList, after removing the first N elements.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when count is negative or exceeds the number of
    /// elements in the IList.</exception>
    let skip count (iList: IList<'T>) = 
        if count < 0 || count > iList.Count then ArgumentException.RaiseBase "FsEx.IList.skip: count %d is not in range of 0 to iList.Count %d " count iList.Count
        if count = iList.Count then
            Rarr()
        else
            sub iList count (iList.Count-count)
            


    /// <summary>Bypasses elements in a IList while the given predicate returns <c>true</c>, and then returns
    /// the remaining elements in a new IList.</summary>
    /// <param name="predicate">A function that evaluates an element of the IList to a boolean value.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The created sub IList.</returns>
    let skipWhile (predicate:'T->bool) (iList: IList<'T>) = 
        let mutable i = 0
        let li = iList
        while i < iList.Count && predicate li.[i] do
            i <- i + 1
        if i = iList.Count then
            Rarr()
        else
            sub iList i (iList.Count-i)


    /// <summary>Sorts the elements of a IList, returning a new IList. Elements are compared using <see cref="M:Microsoft.FSharp.Core.Operators.compare"/>.
    /// This means "Z" is before "a". This is different from Collections.Generic.Sort() where "a" is before "Z" using IComparable interface.
    /// This is NOT a stable sort, i.e. the original order of equal elements is not necessarily preserved.
    /// For a stable sort, consider using Seq.Sort</summary>
    /// <param name="iList">The input IList.</param>
    /// <returns>A new sorted IList.</returns>
    let sort<'T when 'T : comparison> (iList : IList<'T>) : Rarr<'T> = 
        let r = sub iList 0 iList.Count
        r.Sort(Operators.compare)
        r


    /// <summary>Sorts the elements of a IList, using the given projection for the keys and returning a new IList.
    /// Elements are compared using <see cref="M:Microsoft.FSharp.Core.Operators.compare"/>.
    /// This means "Z" is before "a". This is different from Collections.Generic.Sort() where "a" is before "Z" using IComparable interface.
    /// This is NOT a stable sort, i.e. the original order of equal elements is not necessarily preserved.
    /// For a stable sort, consider using Seq.sort.</summary>
    /// <param name="projection">The function to transform IList elements into the type that is compared.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The sorted IList.</returns>
    let sortBy<'T, 'Key when 'Key : comparison> (projection : 'T -> 'Key) (iList : IList<'T>) : Rarr<'T> = 
        let r = sub iList 0 iList.Count
        r.Sort (fun x y -> Operators.compare (projection x) (projection y))
        r


    /// <summary>Sorts the elements of a IList, in descending order, using the given projection for the keys and returning a new IList.
    /// Elements are compared using <see cref="M:Microsoft.FSharp.Core.Operators.compare"/>.
    /// This means in ascending sorting "Z" is before "a". This is different from Collections.Generic.Sort() where "a" is before "Z" using IComparable interface.
    /// This is NOT a stable sort, i.e. the original order of equal elements is not necessarily preserved.
    /// For a stable sort, consider using Seq.sort.</summary>
    /// <param name="projection">The function to transform IList elements into the type that is compared.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The sorted IList.</returns>
    let inline sortByDescending<'T, 'Key when 'Key : comparison> (projection : 'T -> 'Key) (iList : IList<'T>) : Rarr<'T> = 
        let r = sub iList 0 iList.Count
        r.Sort (fun x y -> Operators.compare (projection y) (projection x)) // x and y are swapped for descending order
        r


    /// <summary>Sorts the elements of a IList, in descending order, returning a new IList. Elements are compared using <see cref="M:Microsoft.FSharp.Core.Operators.compare"/>.
    /// This means in ascending sorting "Z" is before "a". This is different from Collections.Generic.Sort() where "a" is before "Z" using IComparable interface.
    /// This is NOT a stable sort, i.e. the original order of equal elements is not necessarily preserved.
    /// For a stable sort, consider using Seq.sort.</summary>
    /// <param name="iList">The input IList.</param>
    /// <returns>The sorted IList.</returns>
    let sortDescending<'T when 'T : comparison> (iList : IList<'T>) : Rarr<'T> = 
        let r = sub iList 0 iList.Count // fastest way to create a shallow copy
        r.Sort(Operators.compare) // Operators.compare is need to match sorting of Array.sort
        r.Reverse()
        r

    /// <summary>Sorts the elements of a IList, using the given comparison function as the order, returning a new IList.
    /// This is NOT a stable sort, i.e. the original order of equal elements is not necessarily preserved.
    /// For a stable sort, consider using Seq.sort.</summary>
    /// <param name="comparer">The function to compare pairs of IList elements.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The sorted IList.</returns>
    let sortWith (comparer : 'T -> 'T -> int) (iList : IList<'T>) : Rarr<'T> = 
        let r = sub iList 0 iList.Count // fastest way to create a shallow copy
        r.Sort (comparer)
        r

    /// <summary>Splits a IList into two Rarrs, at the given index.</summary>
    /// <param name="index">The index at which the IList is split.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The two split Rarrs.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when split index exceeds the number of elements in the IList.</exception>
    let splitAt index (iList: IList<'T>) = 
        if index < 0 || index > iList.Count then ArgumentException.RaiseBase "FsEx.IList.splitAt: index %d is not in range  0 to iList.Count-1 (%d)" index (iList.Count-1)
        sub iList 0  index, sub iList index (iList.Count-index)


    /// <summary>Splits the input IList into at most <c>chunkCount</c> chunks.
    /// If the list can not be split evenly the initial elements will be one bigger than the later elements. Just like with Array.splitInto.</summary>
    /// <param name="chunkCount">The maximum number of chunks.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The IList split into chunks.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when <c>count</c> is not positive.</exception>
    let splitInto (chunkCount:int) (iList: IList<'T>) : Rarr<Rarr<'T>> = 
        if chunkCount < 1  then ArgumentException.RaiseBase "FsEx.IList.splitInto: count %d is less than 1" chunkCount
        let len = iList.Count
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
                //printfn "i %d v: %d chunksize %d" i iList.[i] chunksize
                if k = chunksize  then
                    sub <- Rarr(chunksize)
                    res.Add(sub)
                    k <- 0
                    if res.Count = oneBiggerFor + 1  then // reduce list size once
                        chunksize <- chunksize - 1
                sub.Add(iList.[i])
                k <- k+1
            res


    /// <summary>Returns the sum of the elements in the IList.</summary>
    /// <param name="iList">The input IList.</param>
    /// <returns>The resulting sum.</returns>
    let inline sum (iList: ^T IList ) : ^T = 
        let mutable acc = LanguagePrimitives.GenericZero< ^T>
        let li = iList
        for i = 0 to li.Count - 1 do
            acc <- Checked.(+) acc li.[i]
        acc


    /// <summary>Returns the sum of the results generated by applying the function to each element of the IList.</summary>
    /// <param name="projection">The function to transform the IList elements into the type to be summed.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The resulting sum.</returns>
    let inline sumBy (projection: 'T -> ^Key) (iList: IList<'T>) : ^Key = 
        let mutable acc = LanguagePrimitives.GenericZero< ^Key>
        let li = iList
        for i = 0 to li.Count - 1 do
            acc <- Checked.(+) acc (projection li.[i])
        acc


    /// <summary>Returns a new IList containing the elements of the original except the first element.</summary>
    /// <param name="iList">The input IList.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the IList is empty.</exception>
    /// <returns>A new IList containing the elements of the original except the first element.</returns>
    let tail (iList: IList<'T>) = 
        if iList.Count = 0  then ArgumentException.RaiseBase "FsEx.IList.tail: input IList is empty"
        sub iList 1 (iList.Count-1)


    /// <summary>Returns the first N elements of the IList.
    /// Throws <c>ArgumentException</c> if the count exceeds the number of elements in the IList.
    /// Use <c>IList.truncate</c> to returns as many items as the IList contains instead of throwing an exception.</summary>
    /// <param name="count">The number of items to take.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The result IList.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when the input IList is empty or count exceeds the number of elements in the list.</exception>
    let take count (iList: IList<'T>) = 
        if count < 0 then  ArgumentException.RaiseBase "FsEx.IList.take: count %d cannot be negative." count
        if count = 0 then
            Rarr(0)
        else
            if count > iList.Count then
                ArgumentException.RaiseBase "FsEx.IList.take: count %d > iList.Count %d." count iList.Count
            sub iList 0 count



    /// <summary>Returns a IList that contains all elements of the original IList while the
    /// given predicate returns <c>true</c>, and then returns no further elements.</summary>
    /// <param name="predicate">A function that evaluates to false when no more items should be returned.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The result IList.</returns>
    let takeWhile (predicate:'T->bool) (iList: IList<'T>) = 
        let li = iList
        if iList.Count = 0 then
            Rarr(0)
        else
            let mutable count = 0
            while count < iList.Count && predicate li.[count] do
                count <- count + 1
            sub iList 0 count



    /// <summary>Builds a list from the given IList.</summary>
    /// <param name="iList">The input IList.</param>
    /// <returns>The list of IList elements.</returns>
    let toList (iList:IList<'T>) : list<'T> = 
        let li = iList
        let mutable res = []
        for i = li.Count - 1 downto 0 do
            res <- li.[i] :: res
        res



    /// <summary>Returns the transpose of the given sequence of Rarrs.</summary>
    /// <param name="iLists">The input sequence of Rarrs.</param>
    /// <returns>The transposed IList.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>
    let transpose (iLists: IList<IList<'T>>) : Rarr<Rarr<'T>> = 
        // originally let transpose (iLists: seq<IList<'T>>) : IList<IList<'T>> = 
        let len = iLists.Count
        if len = 0 then
            Rarr(0)
        else
            let lenInner = iLists.[0].Count
            for j in 1..len-1 do
                if lenInner <> iLists.[j].Count then
                    ArgumentException.RaiseBase "FsEx.IList.transpose: the count %d in sub IList %d does not match the count of the first IList %d." iLists.[j].Count j lenInner
            let result = Rarr(lenInner)
            for i in 0..lenInner-1 do
                let sub = Rarr(len)
                result.Add(sub)
                for j in 0..len-1 do
                    sub.Add(iLists.[j].[i])
            result


    /// <summary>Returns at most N elements in a new IList. When count is negative return empty IList.</summary>
    /// <param name="count">The maximum number of items to return.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The result IList.</returns>
    let truncate count (iList: IList<'T>) : Rarr<'T> = 
        if count <= 0 then Rarr(0)
        else
            let c = Operators.min count iList.Count
            sub iList 0 c



    /// <summary>Returns the only element of the IList or <c>None</c> if IList is empty or contains more than one element.</summary>
    /// <param name="iList">The input IList.</param>
    /// <returns>The only element of the IList or <c>None</c>.</returns>
    let tryExactlyOne (iList: IList<'T>) :option<'T> = 
        if iList.Count = 1 then Some iList.[0]
        else None


    /// <summary>Returns the first element for which the given function returns <c>true</c>.
    /// Return None if no such element exists.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The first element that satisfies the predicate, or <c>None</c>.</returns>
    let tryFind (predicate:'T->bool) (iList: IList<'T>) :option<'T> = 
        let elementIndex = iList|> findIndex predicate
        match elementIndex with
        | -1 ->
            None
        | index ->
            Some iList.[index]


    /// <summary>Returns the last element for which the given function returns <c>true</c>.
    /// Return None if no such element exists.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The last element that satisfies the predicate, or <c>None</c>.</returns>
    let tryFindBack (predicate:'T->bool) (iList: IList<'T>) :option<'T> = 
        let li = iList
        let rec loop i = 
            if i < 0 then None else
            if predicate li.[i] then Some li.[i]  else loop (i-1)
        loop ((li.Count - 1))


    /// <summary>Returns the index of the first element in the IList
    /// that satisfies the given predicate.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The index of the first element that satisfies the predicate, or <c>None</c>.</returns>
    let tryFindIndex (predicate:'T->bool) (iList: IList<'T>) : option<int>= 
        let elementIndex = iList|> findIndex predicate
        match elementIndex with
        | -1 ->
            None
        | index ->
            Some index


    /// <summary>Returns the index of the last element in the IList
    /// that satisfies the given predicate.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The index of the last element that satisfies the predicate, or <c>None</c>.</returns>
    let tryFindIndexBack (predicate:'T->bool) (iList: IList<'T>) : option<int>= 
        let li = iList
        let rec loop i = 
            if i < 0 then None else
            if predicate li.[i] then Some i  else loop (i-1)
        loop ((li.Count - 1))


    /// <summary>Returns the first element of the IList, or
    /// <c>None</c> if the IList is empty.</summary>
    /// <param name="iList">The input IList.</param>
    /// <returns>The first element of the IList or <c>None</c>.</returns>
    let tryHead (iList: IList<'T>) = 
        if iList.Count = 0 then None
        else Some iList.[0]


    /// <summary>Tries to find the nth element in the IList.
    /// Returns <c>None</c> if index is negative or the input IList does not contain enough elements.</summary>
    /// <param name="index">The index of element to retrieve.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The nth element of the IList or <c>None</c>.</returns>
    let tryItem index (iList: IList<'T>) = 
        if index < 0 || index >= iList.Count then None
        else Some(iList.[index])


    /// <summary>Returns the last element of the IList.
    /// Return <c>None</c> if no such element exists.</summary>
    /// <param name="iList">The input IList.</param>
    /// <returns>The last element of the IList or <c>None</c>.</returns>
    let tryLast (iList: IList<'T>) = 
        if iList.Count = 0 then None
        else Some iList.[iList.Count-1]


    /// <summary>Applies the given function to successive elements, returning the first
    /// result where function returns <c>Some(x)</c> for some <c>x</c>. If the function
    /// never returns <c>Some(x)</c> then <c>None</c> is returned.</summary>
    /// <param name="chooser">The function to transform the IList elements into options.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The first transformed element that is <c>Some(x)</c>.</returns>
    let tryPick chooser (iList: IList<'T>) = 
        let li = iList
        let rec loop i = 
            if i >= iList.Count then
                None
            else
                match chooser li.[i] with
                | None -> loop(i+1)
                | res -> res
        loop 0


    /// <summary>Returns a IList that contains the elements generated by the given computation.
    /// The given initial <c>state</c> argument is passed to the element generator.</summary>
    /// <param name="generator">A function that takes in the current state and returns an option tuple of the next
    /// element of the IList and the next state value.</param>
    /// <param name="state">The initial state value.</param>
    /// <returns>The result IList.</returns>
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


    /// <summary>Splits a IList of pairs into two Rarrs.</summary>
    /// <param name="iList">The input IList.</param>
    /// <returns>The two Rarrs.</returns>
    let unzip (iList: IList<'T*'U>) : Rarr<'T> * Rarr<'U>= 
        let len = iList.Count
        let res1 = Rarr(len)
        let res2 = Rarr(len)
        let li = iList
        for i = 0 to li.Count - 1 do
            let x, y = li.[i]
            res1.Add <|  x
            res2.Add <|  y
        res1, res2


    /// <summary>Splits a IList of triples into three Rarrs.</summary>
    /// <param name="iList">The input IList.</param>
    /// <returns>The tuple of three Rarrs.</returns>
    let unzip3 (iList: IList<'T*'U*'V>) = 
        let len = iList.Count
        let res1 = Rarr(len)
        let res2 = Rarr(len)
        let res3 = Rarr(len)
        let li = iList
        for i = 0 to li.Count - 1 do
            let x, y, z = li.[i]
            res1.Add <|  x
            res2.Add <|  y
            res3.Add <|  z
        res1, res2, res3

    /// <summary>Return a new IList with the item at a given index set to the new value.(does NOT modify in place !)</summary>
    /// <param name="index">The index of the item to be replaced.</param>
    /// <param name="value">The new value.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The result IList.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when index is not within  iList.Count </exception>
    let updateAt (index: int) (value: 'T) (iList: IList<'T>) : Rarr<'T> = 
        if index < 0 || index >= iList.Count  then ArgumentException.RaiseBase "FsEx.IList.updateAt: index %d  not within iList.Count %d." index  iList.Count
        let r = Rarr.ofIList iList
        r.[index] <- value
        r


    /// <summary>Returns a new IList containing only the elements of the IList
    /// for which the given predicate returns <c>true</c>.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>a IList containing the elements for which the given predicate returns true.</returns>
    let where (predicate:'T->bool) (iList: IList<'T>) = 
        filter predicate iList


    /// <summary>Returns a IList of sliding windows containing elements drawn from the input IList. Each window is returned as a fresh IList.</summary>
    /// <param name="windowSize">The number of elements in each window.</param>
    /// <param name="iList">The input IList.</param>
    /// <returns>The result IList.</returns>
    /// <exception cref="T:System.ArgumentException">Thrown when windowSize is not positive.</exception>
    let windowed windowSize (iList: IList<'T>) = 
        if windowSize <= 0 then  ArgumentException.RaiseBase "FsEx.IList.windowed: windowSize %d cannot be negative or 0." windowSize
        let len = iList.Count
        if windowSize > len then
            Rarr(0)
        else
            let res=Rarr(len - windowSize + 1)
            for i = 0 to len - windowSize do
                res.Add <|  sub iList i windowSize
            res


    /// <summary>Combines the two Rarrs into a IList of pairs. The two Rarrs must have equal lengths, otherwise an <c>ArgumentException</c> is raised.</summary>
    /// <param name="iList1">The first input IList.</param>
    /// <param name="iList2">The second input IList.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>
    /// <returns>The IList of tupled elements.</returns>
    let zip (iList1: IList<'T>) (iList2: IList<'U>) = 
        let len1 = iList1.Count
        if len1 <> iList2.Count then ArgumentException.RaiseBase "FsEx.IList.zip: count of iList1 %d does not match iList2 %d." iList1.Count iList2.Count
        let res = Rarr(len1)
        for i = 0 to iList1.Count-1 do
            res.Add (iList1.[i], iList2.[i])
        res


    /// <summary>Combines three Rarrs into a IList of pairs. The three Rarrs must have equal lengths, otherwise an <c>ArgumentException</c> is raised.</summary>
    /// <param name="iList1">The first input IList.</param>
    /// <param name="iList2">The second input IList.</param>
    /// <param name="iList3">The third input IList.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>
    /// <returns>The IList of tupled elements.</returns>
    let zip3 (iList1: IList<'T>) (iList2: IList<'U>) (iList3: IList<'V>) = 
        let len1 = iList1.Count
        if len1 <> iList2.Count || len1 <> iList3.Count then ArgumentException.RaiseBase "FsEx.IList.zip3: count of iList1 %d does not match iList2 %d or iList3 %d." iList1.Count iList2.Count iList3.Count
        let res = Rarr(len1)
        for i = 0 to iList1.Count-1 do
            res.Add (iList1.[i], iList2.[i], iList3.[i])
        res

    /// Parallel operations on IList using Threading.Tasks.Parallel.For
    module Parallel = 

        open System.Threading.Tasks

        /// <summary>Apply the given function to each element of the IList. Return
        /// the IList comprised of the results "x" for each element where
        /// the function returns Some(x).
        /// Performs the operation in parallel using <see cref="M:System.Threading.Tasks.Parallel.For" />.
        /// The order in which the given function is applied to elements of the input IList is not specified.</summary>
        /// <param name="chooser">The function to generate options from the elements.</param>
        /// <param name="iList">The input IList.</param>
        /// <returns>The IList of results.</returns>
        let choose (chooser: 'T -> option<'U> ) (iList: IList<'T>) : Rarr<'U>= 
            let inputLength = iList.Count
            let isChosen: bool[] = Array.zeroCreate inputLength
            let results: 'U []   = Array.zeroCreate inputLength
            let mutable outputLength = 0
            let li = iList
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



        /// <summary>For each element of the IList, apply the given function. Concatenate all the results and return the combined IList.
        /// Performs the operation in parallel using <see cref="M:System.Threading.Tasks.Parallel.For" />.
        /// The order in which the given function is applied to elements of the input IList is not specified.</summary>
        /// <param name="mapping"></param>
        /// <param name="iList">The input IList.</param>
        /// <returns>'U[]</returns>
        let collect (mapping: 'T -> IList<'U>)  (iList: IList<'T>) : Rarr<'U>= 
            let inputLength = iList.Count
            let result = create inputLength Unchecked.defaultof<_>
            let li = iList
            Parallel.For(0, inputLength,
                (fun i -> result.[i] <- mapping li.[i])) |> ignore
            concat result


        /// <summary>Create a IList given the dimension and a generator function to compute the elements.
        /// Performs the operation in parallel using <see cref="M:System.Threading.Tasks.Parallel.For" />.
        /// The order in which the given function is applied to indices is not specified.</summary>
        /// <param name="count"></param>
        /// <param name="initializer"></param>
        /// <returns>The IList of results.</returns>
        let init count (initializer:int->'T) : Rarr<'T> =
            let result = create count Unchecked.defaultof<_>
            Parallel.For (0, count, fun i -> result.[i] <- initializer i) |> ignore
            result


        /// <summary>Apply the given function to each element of the IList.
        /// Performs the operation in parallel using <see cref="M:System.Threading.Tasks.Parallel.For" />.
        /// The order in which the given function is applied to elements of the input IList is not specified.</summary>
        /// <param name="action"></param>
        /// <param name="iList">The input IList.</param>
        let iter (action:'T->unit) (iList: IList<'T>) = 
            let li = iList
            Parallel.For (0, iList.Count, fun i -> action li.[i]) |> ignore


        /// <summary>Apply the given function to each element of the IList. The integer passed to the
        /// function indicates the index of element.
        /// Performs the operation in parallel using <see cref="M:System.Threading.Tasks.Parallel.For" />.
        /// The order in which the given function is applied to elements of the input IList is not specified.</summary>
        /// <param name="action"></param>
        /// <param name="iList">The input IList.</param>
        let iteri (action:int->'T->unit) (iList: IList<'T>) = 
            let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(action)
            let li = iList
            Parallel.For (0, iList.Count, fun i -> f.Invoke(i, li.[i])) |> ignore


        /// <summary>Build a new IList whose elements are the results of applying the given function
        /// to each of the elements of the IList.
        /// Performs the operation in parallel using <see cref="M:System.Threading.Tasks.Parallel.For" />.
        /// The order in which the given function is applied to elements of the input IList is not specified.</summary>
        /// <param name="mapping"></param>
        /// <param name="iList">The input IList.</param>
        /// <returns>The IList of results.</returns>
        let map (mapping: 'T -> 'U) (iList: IList<'T>) : Rarr<'U>= 
            let inputLength = iList.Count
            let result = create inputLength Unchecked.defaultof<_>
            let li = iList
            Parallel.For(0, inputLength, fun i ->
                result.[i] <- mapping li.[i]) |> ignore
            result


        /// <summary>Build a new IList whose elements are the results of applying the given function
        /// to each of the elements of the IList. The integer index passed to the
        /// function indicates the index of element being transformed.
        /// Performs the operation in parallel using <see cref="M:System.Threading.Tasks.Parallel.For" />.
        /// The order in which the given function is applied to elements of the input IList is not specified.</summary>
        /// <param name="mapping"></param>
        /// <param name="iList">The input IList.</param>
        /// <returns>The IList of results.</returns>
        let mapi (mapping:int-> 'T -> 'U) (iList: IList<'T>) = 
            let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(mapping)
            let inputLength = iList.Count
            let result = create inputLength  Unchecked.defaultof<_>
            let li = iList
            Parallel.For(0, inputLength, fun i ->
                result.[i] <- f.Invoke (i, li.[i])) |> ignore
            result


        /// <summary>Split the collection into two collections, containing the
        /// elements for which the given predicate returns <c>true</c> and <c>false</c>
        /// respectively
        /// Performs the operation in parallel using <see cref="M:System.Threading.Tasks.Parallel.For" />.
        /// The order in which the given function is applied to indices is not specified.</summary>
        /// <param name="predicate">The function to test the input elements.</param>
        /// <param name="iList">The input IList.</param>
        /// <returns>The two Rarrs of results.</returns>
        let partition (predicate:'T->bool) (iList: IList<'T>) = 
            let inputLength = iList.Count
            let isTrue = Array.zeroCreate inputLength
            let mutable trueLength = 0
            let li = iList
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




