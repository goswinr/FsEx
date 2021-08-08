namespace FsEx

open System

#nowarn "44" // to disable the obsolete warning on accessing Rarr.List

/// Generic operations on Rarr which is like a System.Collections.Generic.List<'T> but with nicer error messages.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] //need this so doesn't hide Rarr class in C# assemblies (should consider for other extension modules as well)
module Rarr =    
    
    //---------------------------------------------------
    // extensions added only in FsEx:
    //----------------------------------------------------
    
    /// Applies a function to List
    /// If resulting List meets the resultPredicate it is returned , otherwise  orinal input is returned.
    let inline applyIfResult (resultPredicate:Rarr<'T> -> bool) (transform:Rarr<'T> -> Rarr<'T>)  (rarr: Rarr<'T>) : Rarr<'T> =
        let r = transform rarr
        if resultPredicate r then r
        else rarr

    /// Applies a function to List if it meets the inputPredicate, otherwise just returns input.
    /// If resulting List meets the resultPredicate it is returned , otherwise orinal input is returned.
    let inline applyIfInputAndResult (inputPredicate:Rarr<'T> -> bool) (resultPredicate:Rarr<'T> -> bool) (transform:Rarr<'T> -> Rarr<'T>)  (rarr: Rarr<'T>) : Rarr<'T> =
        if inputPredicate rarr then
            let r = transform rarr
            if resultPredicate r then r
            else rarr
        else
            rarr  

    /// Creates a shallow copy by calling 
    /// rarr.GetRange(0,rarr.Count) 
    let inline shallowCopy (rarr: Rarr<'T>) = rarr.GetRange(0,rarr.Count) // fastest way to create a shallow copy

    /// Gets an item at index 
    /// (use Rarr.GetNeg(i) member if you want to use negative indices too)
    let inline get index (rarr: Rarr<'T>) = rarr.[index]
        
    /// Sets an item at index 
    /// (use Rarr.SetNeg(i) member if you want to use negative indices too)
    let inline set index value  (rarr: Rarr<'T>) = rarr.[index] <- value

    /// Gets an item in the Rarr by index.
    /// Allows for negtive index too ( -1 is last item,  like Python)
    /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
    let inline getNeg index  (rarr: Rarr<'T>)= 
        let len = rarr.Count
        let ii =  if index < 0 then len + index else index
        if ii<0 || ii >= len then ArgumentOutOfRangeException.Raise "Rarr.GetNeg: Failed to get index %d from Rarr of %d items: %A" index rarr.Count rarr
        rarr.List.[ii] // access List directly to not check index twice       

    /// Sets an item in the Rarr by index.
    /// Allows for negtive index too ( -1 is last item,  like Python)
    /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
    let inline setNeg index value  (rarr: Rarr<'T>)= 
        let len = rarr.Count
        let ii =  if index < 0 then len + index else index
        if ii<0 || ii >= len then ArgumentOutOfRangeException.Raise "Rarr.SetNeg: Failed to set index %d to %A rom Rarr of %d items: %A" index value rarr.Count rarr
        rarr.List.[ii] <- value     // access List directly to not check index twice      
   
    /// Any index will return a value.
    /// Rarr is treated as an endless loop in positive and negative direction   
    let inline getLooped index  (rarr: Rarr<'T>)= 
        let len = rarr.Count
        if len=0 then ArgumentOutOfRangeException.Raise "Rarr.GetLooped: Failed to get index %d from Rarr of 0 items" index
        let t = index % len
        let ii = if t >= 0 then t  else t + len 
        rarr.List.[ii]   // access List directly to not check index twice             

    /// Any index will set a value.
    /// Rarr is treated as an endless loop in positive and negative direction   
    let inline setLooped index value  (rarr: Rarr<'T>) = 
        let len = rarr.Count
        if len=0 then ArgumentOutOfRangeException.Raise "Rarr.SetLooped: Failed to Set index %d to %A in Rarr of 0 items" index value
        let t = index % len
        let ii = if t >= 0 then t  else t + len 
        rarr.List.[ii] <- value // access List directly to not check index twice  

    /// Get and remove last item from Rarr
    let inline pop  (rarr: Rarr<'T>)  =
        if rarr.Count=0 then ArgumentOutOfRangeException.Raise "Failed to pop from empty Rarr"
        let i = rarr.Count - 1        
        let v = rarr.List.[i] // access List directly to not check index twice  
        rarr.List.RemoveAt(i)
        v

    /// Gets the last item in the Rarr.
    /// equal to this.[this.Count - 1]
    let inline last  (rarr: Rarr<'T>) = 
        if rarr.Count = 0 then IndexOutOfRangeException.Raise "Rarr.last: Failed to get last item of empty List"
        rarr.List.[rarr.Count - 1]
    
    /// Gets the second last item in the Rarr.
    /// equal to this.[this.Count - 2]
    let inline secondLast  (rarr: Rarr<'T>)= 
        if rarr.Count < 2 then  IndexOutOfRangeException.Raise "Rarr.secondLast: Failed to get second last item of %s"   (NiceString.toNiceStringFull rarr)
        rarr.List.[rarr.Count - 2]

    /// Gets the third last item in the Rarr.
    /// equal to this.[this.Count - 3]
    let inline thirdLast  (rarr: Rarr<'T>)= 
        if rarr.Count < 3 then  IndexOutOfRangeException.Raise "Rarr.thirdLast: Failed to get third last item of %s"  (NiceString.toNiceStringFull rarr)
        rarr.List.[rarr.Count - 3]
                
    /// Gets the first item in the Rarr.
    /// equal to this.[0]
    let inline first  (rarr: Rarr<'T>)= 
        if rarr.Count = 0 then IndexOutOfRangeException.Raise "Rarr.first: Failed to get first item of empty Rarr List"
        rarr.List.[0]

    /// Gets the second item in the Rarr.
    /// equal to this.[1]
    let inline second  (rarr: Rarr<'T>)= 
        if rarr.Count < 2 then IndexOutOfRangeException.Raise  "Rarr.second: Failed to get second item of %s"   (NiceString.toNiceStringFull rarr)
        rarr.List.[1]

    /// Gets the third item in the Rarr.
    /// equal to this.[2]
    let inline third  (rarr: Rarr<'T>)= 
        if rarr.Count < 3 then IndexOutOfRangeException.Raise "Rarr.third: Failed to get third item of %s"  (NiceString.toNiceStringFull rarr)
        rarr.List.[2]

    /// Allows for negative indices too. ( -1 is last item, like Python) 
    /// The resulting Rarr includes the end index.
    /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
    let inline slice startIdx endIdx (rarr: Rarr<_>)  = rarr.GetSlice(startIdx, endIdx)
    
    /// internal, to ensure a minumum count
    let inline private checkCount c txt (a:Rarr<_>) = 
        if a.Count < c then ArgumentException.RaiseBase "Rarr %A has %d elements but needs  %d for Rarr.%s" a a.Count c txt
    
    (*

            implemet those with cashing to avoid repeated index lookup, see Seq module

            /// Yields Seq from (first, second)  upto (second-last, last)  
            /// not looped
            /// the resulting seq is one element shorter than the input Rarr
            let windowed2 (a:_ Rarr) = 
                checkCount 2 "windowed2" a  
                seq {   for i = 0 to a.Count-2 do  yield a.[i], a.[i+1] }
    
            /// Yields looped Seq from (first, second)  upto (last, first) 
            /// the resulting seq has the same element count as the input Rarr
            let thisNext (a:_ Rarr) = 
                checkCount 2 "thisNext" a 
                seq {   for i = 0 to a.Count-2 do yield a.[i], a.[i+1] 
                        yield a.[a.Count-1], a.[0] }
    
            /// Yields looped Seq from (last,first)  upto (second-last, last) 
            /// the resulting seq has the same element count as the input Rarr
            let prevThis (a:_ Rarr) = 
                checkCount 2 "prevThis" a 
                seq {   yield a.[a.Count-1], a.[0]
                        for i = 0 to a.Count-2 do yield a.[i], a.[i+1] }
    
            /// Yields Seq from (first, second, third)  upto (third-last, second-last, last) 
            /// not looped
            /// the resulting seq is two elements shorter than the input Rarr
            let windowed3 (a:_ Rarr) = 
                checkCount 3 "windowed3" a 
                seq {   for i = 0 to a.Count-3 do yield a.[i], a.[i+1], a.[i+2] } //TODO keep prev in mutabe value to avoid accessing the same item 3 times
    
            /// Yields looped Seq of  from (last, first, second)  upto (second-last, last, first)
            /// the resulting seq has the same element count as the input Rarr
            let prevThisNext (a:_ Rarr) =  
                checkCount 3 "prevThisNext" a 
                seq{    yield  a.[a.Count-1], a.[0], a.[1] 
                        for i = 0 to a.Count-3 do yield a.[i], a.[i+1], a.[i+2]
                        yield  a.[a.Count-2],a.[a.Count-1], a.[0] }
    
            /// Yields Seq from (0,first, second)  upto (lastIndex-1 , second-last, last) 
            /// not looped
            /// the resulting seq is one element shorter than the input Rarr
            let windowed2i (a:_ Rarr) =     
                checkCount 2 "windowed2i" a 
                seq {   for i = 0 to a.Count-2 do yield i, a.[i], a.[i+1] } 
    
            /// Yields looped Seq  from (0,first, second)  upto (lastIndex, last, first)
            /// the resulting seq has the same element count as the input Rarr
            let iThisNext (a:_ Rarr) = 
                checkCount 2 "iThisNext" a  
                seq {   for i = 0 to a.Count-2 do yield i, a.[i], a.[i+1] 
                        yield  a.Count-1, a.[a.Count-1], a.[0] }
    
            /// Yields Seq from (1, first, second, third)  upto (lastIndex-1 , third-last, second-last, last) 
            /// not looped
            /// the resulting seq is two elements shorter than the input Rarr
            let windowed3i (a:_ Rarr) =     
                checkCount 3 "windowed3i" a 
                seq {   for i = 0 to a.Count-3 do yield i+1, a.[i], a.[i+1], a.[i+2] }
    
            /// Yields looped Seq from (1, last, first, second)  upto (lastIndex, second-last, last, first)
            /// the resulting seq has the same element count as the input Rarr
            let iPrevThisNext (a:_ Rarr) = 
                checkCount 3 "iPrevThisNext" a 
                seq {   yield  0, a.[a.Count-1], a.[0], a.[1]
                        for i = 0 to a.Count-3 do yield i+1, a.[i], a.[i+1], a.[i+2] 
                        yield  a.Count-1, a.[a.Count-2],a.[a.Count-1], a.[0] }
    
            *)

    /// Returns an empty Rarr
    let inline empty() = Rarr<'T>()

    /// Returns a Rarr that has at most k items, can be less too.
    let inline truncate k (rarr: Rarr<'T>) =
        if rarr.Count >= k then rarr.GetRange(0,k)
        else                    rarr.GetRange(0,rarr.Count)

    /// Create a Rarr by calling the given generator on each index.

    let inline init count initializer : Rarr<'T> =
        if count < 0 then ArgumentException.RaiseBase "Rarr.init: The count can't be %d" count   
        let rarr = Rarr (count)
        for i = 0 to count - 1 do rarr.Add ( initializer i)
        rarr

    /// Considers List cirular and move elements up or down
    /// e.g.: rotate +1 [ a, b, c, d] = [ d, a, b, c]
    /// e.g.: rotate -1 [ a, b, c, d] = [ b, c, d, a]
    let inline rotate k (rarr: Rarr<'T>)  =  init rarr.Count (fun i -> rarr.[negIdxLooped (i-k) rarr.Count])

    /// Returns a Rarr of the index and the item. (like enumerate in Python)
    let inline indexed (rarr: Rarr<'T>)  =  init rarr.Count (fun i -> i,rarr.[i])

    /// Splits a Rarr in two, like Rarr.filter but returning two Lists
    /// The first Rarr has all elements where the filter function returned 'true'
    /// renamed to  Rarr.partition
    [<Obsolete("renamed to  Rarr.partition")>] 
    let inline splitBy filter (rarr:Rarr<'T>) =  
        let t=Rarr()
        let f=Rarr()
        for x in rarr do
            if filter x then t.Add(x)
            else             f.Add(x)
        t,f

    /// Structural equality
    /// compares each element in both lists for eqality . Rarrs must also be of same Count
    let inline equals (rarr1: Rarr<'T>) (rarr2: Rarr<'T>) =
        if rarr1.Count <> rarr2.Count then false 
        else
            let rec eq i = 
                if i < rarr1.Count then 
                    if rarr1.[i] = rarr2.[i] then eq (i+1)
                    else false
                else
                    true
            eq 0
    
    /// Returns a Rarr with just one item.
    let inline singelton (x :'T)  =
        let r = Rarr(1)
        r.Add x
        r
    
    /// Returns true if the given Rarr has just one item.
    /// same as Rarr.hasOne
    let inline isSingelton (rarr : Rarr<'T>) : bool =
        rarr.Count = 1

    /// Returns true if the given Rarr has just one item.
    /// same as Rarr.isSingelton
    let inline hasOne (rarr : Rarr<'T>) : bool =
        rarr.Count = 1

    /// Returns true if the given Rarr has count items.
    let inline hasItems count (rarr : Rarr<'T>) : bool =
        rarr.Count = count

    /// Returns true if the given Rarr has equal or more than count items.
    let inline hasMinimumItems count (rarr : Rarr<'T>) : bool =
        rarr.Count >= count

    /// Returns true if the given Rarr has equal or less than count items.
    let inline hasMaximumItems count (rarr : Rarr<'T>) : bool =
        rarr.Count <= count
    
    /// swap the values of two given indices in Rarr
    let inline swap i j (xs:Rarr<'T>) : unit = 
        if i < 0 then IndexOutOfRangeException.Raise "Rarr.swap: index i can't be less than 0: %d (j: %d)" i j
        if i >= xs.Count then IndexOutOfRangeException.Raise "Rarr.swap: index i can't be bigger than %d but is %d (j: %d)" xs.LastIndex i j 
        if i<>j then  
            if j < 0 then IndexOutOfRangeException.Raise "Rarr.swap: index j can't be less than 0: %d (i: %d)" j i 
            if j >= xs.Count then IndexOutOfRangeException.Raise "Rarr.swap: index j can't be bigger than %d but is %d (i: %d)" xs.LastIndex j i
            // operate on underlaying list since indixes are checked
            let ti = xs.List.[i]
            xs.List.[i] <- xs.List.[j]
            xs.List.[j] <- ti
    

    /// internal only for finding 
    module internal MinMax =
        //TODO test keeping of order if equal !
        
        let inline simple cmpF (xs:Rarr<'T>) =
            if xs.Count < 1 then ArgumentException.RaiseBase "Empty %A in Rarr max / min" xs
            let mutable m = xs.[0]
            for i=1 to xs.Count-1 do
                if cmpF xs.List.[i] m then m <- xs.List.[i]
            m 
            
        let inline simple2 cmpF (xs:Rarr<'T>) =
            if xs.Count < 2 then ArgumentException.RaiseBase "Only %d elements in %A, for Rarr first+second max / min" xs.Count xs
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
            if xs.Count < 3 then ArgumentException.RaiseBase "Only %d elements in %A, for Rarr first+second+third max / min" xs.Count xs
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
            if xs.Count < 1 then ArgumentException.RaiseBase "Empty %A, Rarr  max / min IndexByFun" xs
            let mutable f = func xs.List.[0]
            let mutable mf = f
            let mutable ii = 0
            for i=1 to xs.Count-1 do
                f <- func xs.List.[i] 
                if cmpF f mf then 
                    ii <- i
                    mf <- f
            ii

        let inline index2ByFun cmpF func (xs:Rarr<'T>) =
            if xs.Count < 2 then ArgumentException.RaiseBase "Only %d elements in %A, for Rarr index2ByFun max / min" xs.Count xs            
            let mutable i1 = 0
            let mutable i2 = 1 
            let mutable mf1 = func xs.List.[i1]
            let mutable mf2 = func xs.List.[i2]
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
            if xs.Count < 3 then ArgumentException.RaiseBase "Only %d elements in %A, for Rarr index3ByFun max / min" xs.Count xs 
            // sort first 3
            let mutable i1,i2,i3 =  indexOfSort3By byFun cmpOp xs.[0] xs.[1] xs.[2] // otherwise would fail on sorting first 3, test on Rarr([5;6;3;1;2;0])|> Rarr.max3 
            let mutable e1 =  byFun xs.List.[i1]
            let mutable e2 =  byFun xs.List.[i2]
            let mutable e3 =  byFun xs.List.[i3] 
            let mutable f = e1 // placeholder
            for i=3 to xs.Count-1 do
                f <- byFun xs.List.[i] 
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
        let min rarr =     rarr |> MinMax.simple (<)  

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
    let inline minIndBy f rarr = rarr |> MinMax.indexByFun (<) f

    /// Returns the Index of the biggest element of the Rarr.
    /// Elements are compared by applying the predicate function first.
    let inline maxIndBy f rarr = rarr |> MinMax.indexByFun (>) f

    /// Returns the smallest two elements of the Rarr.
    /// If they are equal then the the order is kept
    let inline min2 rarr =     rarr |> MinMax.simple2 (<)

    /// Returns the biggest two elements of the Rarr.
    /// If they are equal then the the order is kept
    let inline max2 rarr =     rarr |> MinMax.simple2 (>)
        
    /// Returns the smallest two elements of the Rarr.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    let inline min2By f rarr = 
        let i,ii = rarr |> MinMax.index2ByFun (<) f 
        rarr.List.[i],rarr.List.[ii]
        
    /// Returns the biggest two elements of the Rarr.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    let inline max2By f rarr = 
        let i,ii = rarr |> MinMax.index2ByFun (>) f 
        rarr.List.[i],
        rarr.List.[ii]

    /// Returns the indices of the two smallest elements of the Rarr.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    let inline min2IndBy f rarr = rarr |> MinMax.index2ByFun (<) f

    /// Returns the indices of the two biggest elements of the Rarr.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    let inline max2IndBy f rarr = rarr |> MinMax.index2ByFun (>) f

    /// Returns the smallest three elements of the Rarr.
    /// If they are equal then the the order is kept
    let inline min3 rarr =  rarr |> MinMax.simple3 (<)

    /// Returns the biggest three elements of the Rarr.
    /// If they are equal then the the order is kept
    let inline max3 rarr =  rarr |> MinMax.simple3 (>)

    /// Returns the smallest three elements of the Rarr.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    let inline min3By f rarr = 
        let i,ii,iii = rarr |> MinMax.index3ByFun (<) f 
        rarr.List.[i],
        rarr.List.[ii],
        rarr.List.[iii]

    /// Returns the biggest three elements of the Rarr.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    let inline max3By f rarr = 
        let i,ii,iii = rarr |> MinMax.index3ByFun (>) f 
        rarr.List.[i],
        rarr.List.[ii],
        rarr.List.[iii]

    /// Returns the indices of the three smallest elements of the Rarr.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    let inline min3IndBy f rarr = rarr |> MinMax.index3ByFun(<) f

    /// Returns the indices of the three biggest elements of the Rarr.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    let inline max3IndBy f rarr = rarr |> MinMax.index3ByFun (>) f
   
    /// Sorts the elements and returns a new shallow copy of Rarr.
    /// Elements are compared using Operators.compare.
    let inline  sort<'T when 'T : comparison> (rarr : Rarr<'T>) : Rarr<'T> =       
        let r = rarr.GetRange(0,rarr.Count) // fastest way to create a shallow copy
        r.Sort ()
        r
        
    /// Sort the elements and returns a new shallow copy of Rarr.
    ///using the key extractor and generic comparison on the keys.
    let inline sortBy<'T, 'Key when 'Key : comparison>     (projection : 'T -> 'Key) (rarr : Rarr<'T>) : Rarr<'T>=
        let r = rarr.GetRange(0,rarr.Count) // fastest way to create a shallow copy
        r.Sort (fun x y ->compare (projection x) (projection y))
        r
    
    /// Sort the elements and returns new shallow copy of Rarr.
    /// using the given comparison function.
    let inline sortWith (comparer : 'T -> 'T -> int) (rarr : Rarr<'T>) : Rarr<'T> =
       let r = rarr.GetRange(0,rarr.Count) // fastest way to create a shallow copy
       r.Sort (comparer)
       r

    /// Return the length or count of the collection.
    /// same as Rarr.length
    let inline count (rarr : Rarr<'T>) : int =
        rarr.Count
    
    /// Counts for how many items of the collection the predicate returns true.    
    /// same as Rarr.filter and then Rarr.length 
    let inline countIf (predicate : 'T -> bool) (rarr : Rarr<'T>) : int = //countBy is something else !!
        let mutable k = 0
        for i=0 to rarr.Count - 1 do
            if predicate rarr.List.[i] then 
                k <- k + 1
        k

    //-------------------------------------------------------------------------------------------------------------------------------
    // taken and adapded from https://github.com/jack-pappas/ExtCore/blob/master/ExtCore/Collections.Rarr.fs 
    // previously used https://github.com/dotnet/fsharp/tree/master/src/utils   
    //-------------------------------------------------------------------------------------------------------------------------------
    // alternative: https://github.com/fsprojects/FSharpx.Collections/blob/master/src/FSharpx.Collections/ResizeArray.fs
    
    // TODO get doc text from Array module

    open Microsoft.FSharp.Core
    open Microsoft.FSharp.Core.OptimizedClosures
    open System.Collections.Generic
    open LanguagePrimitives

    /// Return the length or count of the collection.
    /// same as Rarr.count
    let inline length (rarr : Rarr<'T>) : int =
        rarr.Count

    /// Return true if the given Rarr is empty, otherwise false.
    let inline isEmpty (rarr : Rarr<'T>) : bool =
        rarr.Count = 0

    /// Create a Rarr whose elements are all initially the given value.
    let inline create count value : Rarr<'T> =
        if count < 0 then ArgumentException.RaiseBase "Rarr.create: The count can't be %d" count        
        let rarr = Rarr (count)
        for i = 0 to count - 1 do
            rarr.Add value
        rarr

    /// Adds an object to the end of the Rarr.
    let inline add item (rarr : Rarr<'T>) : unit =
        rarr.Add item

    /// Determines whether an element is in the Rarr.
    let inline contains (value : 'T) (rarr : Rarr<'T>) : bool =        
        rarr.Contains value

    /// Build a Rarr from the given sequence.
    /// if input is a Rarr or Generic.List it is returned directly
    let inline ofSeq (sequence : seq<'T>) : Rarr<'T> =
        match sequence with
        | :? Rarr<'T> as r -> r
        | :? List<'T> as l -> Rarr.CreateDirectly(l)
        | _ -> Rarr (sequence)

    /// Build a Rarr from the given list.
    let inline ofList (list : 'T list) : Rarr<'T> =
        //let len = list.Length //edit by Goswin, could be costly
        let res = Rarr<_>() //(len) 
        let rec add = function
            | [] -> ()
            | e::l -> res.Add(e); add l
        add list
        res

    /// Build a Rarr from the given Rarr.
    let inline ofArray (arr : 'T[]) : Rarr<'T> =
        Rarr (arr)

    /// Return a view of the Rarr as an enumerable object.
    let inline toSeq (rarr : Rarr<'T>) : seq<'T> =        
        Seq.readonly rarr

    /// Build a list from the given Rarr.
    let inline toList (rarr : Rarr<'T>) : 'T list =        
        let mutable res = []
        for i = rarr.Count - 1 downto 0 do
            res <- rarr.List.[i] :: res
        res

    /// Return a fixed-length Rarr containing the elements of the input Rarr.
    let inline toArray (rarr : Rarr<'T>) : 'T[] =
        rarr.ToArray ()

   
    /// Sorts the elements of the Rarr by mutating the Rarr in-place.
    /// Elements are compared using Operators.compare.
    let inline sortInPlace<'T when 'T : comparison> (rarr : Rarr<'T>) : unit =
        rarr.Sort ()
        
    /// Sort the elements of the Rarr by mutating the Rarr in-place.
    /// Using the key extractor and generic comparison on the keys.
    let inline sortInPlaceBy<'T, 'Key when 'Key : comparison>  (projection : 'T -> 'Key) (rarr : Rarr<'T>) : unit=
        rarr.Sort (fun x y -> compare (projection x) (projection y))

    /// Sort the elements of the Rarr by mutating the Rarr in-place.
    /// Using the given comparison function.
    let inline sortInPlaceWith (comparer : 'T -> 'T -> int) (rarr : Rarr<'T>) : unit =
        rarr.Sort (comparer)

    /// Build a new Rarr that contains the elements of the given Rarr.
    let inline copy (rarr : Rarr<'T>) : Rarr<'T> =
        Rarr (rarr)

    /// Return an Rarr containing the given element.
    let inline singleton value : Rarr<'T> =
        let rarr = Rarr ()
        rarr.Add value
        rarr

    /// Build a new Rarr that contains the elements of each of the given sequence of Rarrs.
    let inline concat (rarrs : seq<#seq<'T>>) : Rarr<'T> =
        let flattened = Rarr ()
        for rarr in rarrs do
            flattened.AddRange rarr
        flattened
    
    /// Build a new Rarr that contains the elements of the first Rarr followed by
    /// the elements of the second Rarr.
    let inline append (rarr1 : Rarr<'T>) (rarr2 : Rarr<'T>) : Rarr<'T> =
        let combined = Rarr (rarr1.Count + rarr2.Count)
        combined.AddRange rarr1
        combined.AddRange rarr2
        combined

    /// Build a new Rarr that contains the given subrange specified by
    /// starting index and length.
    let inline sub (rarr : Rarr<'T>) start count : Rarr<'T> =  
        rarr.GetRange (start, count) // no erro checking needed here 
        

    /// Fill a range of the collection with the given element.
    let inline fill (rarr : Rarr<'T>) start count value : unit =        
        if start < 0 then
            invalidArg "start" "The start index cannot be less than zero (0)." //TODO: improve error message with actual values 
        elif count < 0 then
            invalidArg "count" "The number of elements to copy cannot be less than zero (0)."
        elif start + count > rarr.Count then
            invalidArg "count" "There are fewer than 'count' elements between the 'start' index and the end of the collection."
    
        // Overwrite the items within the range using the specified value.
        for i = start to start + count - 1 do
            rarr.List.[i] <- value

    /// Return a new Rarr with the elements in reverse order.
    let inline rev (rarr : Rarr<'T>) : Rarr<'T> =        
        let len = rarr.Count
        let result = Rarr (len)
        for i = len - 1 downto 0 do
            result.Add rarr.List.[i]
        result

    /// Read a range of elements from the first Rarr and write them into the second.
    let inline blit (source : Rarr<'T>) sourceIndex (target : Rarr<'T>) targetIndex count : unit =
        if sourceIndex < 0 then
            invalidArg "sourceIndex" "The source index cannot be negative."//TODO: improve error message with actual values 
        elif targetIndex < 0 then
            invalidArg "targetIndex" "The target index cannot be negative."
        elif count < 0 then
            invalidArg "count" "Failed to copy a negative number of items."
        elif sourceIndex + count > length source then
            invalidArg "sourceIndex" "There are fewer than 'count' elements between 'sourceIndex' and the end of the source Rarr."
        elif targetIndex + count > length target then
            invalidArg "sourceIndex" "There are fewer than 'count' elements between 'sourceIndex' and the end of the target Rarr."

        for i = 0 to count - 1 do
            target.List.[targetIndex + i] <- source.List.[sourceIndex + i]

    /// Combine the two Rarrs into a Rarr of pairs.
    /// The two Rarrs must have equal lengths, otherwise an <c>ArgumentException</c> is raised.
    let inline zip (rarr1 : Rarr<'T1>) (rarr2 : Rarr<'T2>) : Rarr<'T1 * 'T2> =
        let len = length rarr1
        if len <> length rarr2 then
            ArgumentException.RaiseBase "Rarr.zip: The Rarrs have different lengths. %d and %d" rarr1.Count rarr2.Count              
        let results = Rarr (len)
        for i = 0 to len - 1 do
            results.Add (rarr1.List.[i], rarr2.List.[i])
        results

    /// Split a Rarr of pairs into two Rarrs.
    let inline unzip (rarr : Rarr<'T1 * 'T2>) : Rarr<'T1> * Rarr<'T2> =        
        let len = rarr.Count
        let results1 = Rarr (len)
        let results2 = Rarr (len)
        for i = 0 to len - 1 do
            let x, y = rarr.List.[i]
            results1.Add x
            results2.Add y
        results1, results2

    /// Test if any element of the Rarr satisfies the given predicate.
    /// If the input function is <c>f</c> and the elements are <c>i0...iN</c> 
    /// then computes <c>p i0 or ... or p iN</c>.
    let inline exists (predicate : 'T -> bool) (rarr : Rarr<'T>) : bool =        
        rarr.Exists (System.Predicate predicate)
        

    /// Test elements of the two Rarrs pairwise to see if any pair of element satisfies the given predicate.
    /// Raise ArgumentException if the Rarrs have different lengths.
    let inline exists2 predicate (rarr1 : Rarr<'T1>) (rarr2 : Rarr<'T2>) : bool =
        let len = length rarr1
        if len <> length rarr2 then
            ArgumentException.RaiseBase "Rarr.exists2: The Rarrs have different lengths. %d and %d" rarr1.Count rarr2.Count 
        let predicate = FSharpFunc<_,_,_>.Adapt predicate    
        let mutable index = 0
        let mutable foundMatch = false
        while index < len && not foundMatch do
            foundMatch <- predicate.Invoke (rarr1.List.[index], rarr2.List.[index])
            index <- index + 1
        foundMatch

    /// Test if all elements of the Rarr satisfy the given predicate.
    /// If the input function is <c>f</c> and the elements are <c>i0...iN</c> and "j0...jN"
    /// then computes <c>p i0 && ... && p iN</c>.
    let inline forall (predicate : 'T -> bool) (rarr : Rarr<'T>) : bool =        
        rarr.TrueForAll (System.Predicate predicate)
       

    /// Test elements of the two Rarrs pairwise to see if all pairs of elements satisfy the given predicate.
    /// Raise ArgumentException if the Rarrs have different lengths.
    let inline forall2 predicate (rarr1 : Rarr<'T1>) (rarr2 : Rarr<'T2>) : bool =
        let len = length rarr1
        if len <> length rarr2 then
            ArgumentException.RaiseBase "Rarr.forall2: The Rarrs have different lengths. %d and %d" rarr1.Count rarr2.Count 
        let predicate = FSharpFunc<_,_,_>.Adapt predicate    
        let mutable index = 0
        let mutable allMatch = true
        while index < len && allMatch do
            allMatch <- predicate.Invoke (rarr1.List.[index], rarr2.List.[index])
            index <- index + 1
        allMatch

    /// Return a new collection containing only the elements of the collection
    /// for which the given predicate returns <c>true</c>.
    let inline filter (predicate : 'T -> bool) (rarr : Rarr<'T>) : Rarr<'T> =        
        rarr.FindAll (System.Predicate predicate)       

    
    /// Apply the given function to each element of the Rarr. Return
    /// the Rarr comprised of the results "x" for each element where
    /// the function returns Some(x)
    let inline choose (chooser : 'T -> 'U option) (rarr : Rarr<'T>) : Rarr<'U> =                
        let result = Rarr ()
        let count = rarr.Count
        for i = 0 to count - 1 do
            match chooser rarr.List.[i] with
            | None -> ()
            | Some value ->
                result.Add value
        result

    /// <summary>
    /// Return the first element for which the given function returns <c>true</c>.
    /// Return <c>None</c> if no such element exists.</summary>
    let inline tryFind (predicate : 'T -> bool) (rarr : Rarr<'T>) : 'T option =        
        let elementIndex =            
            rarr.FindIndex (System.Predicate predicate)
        match elementIndex with
        | -1 ->
            None
        | index ->
            Some rarr.[index]

    /// <summary>
    /// Return the first element for which the given function returns <c>true</c>.
    /// Raise <c>KeyNotFoundException</c> if no such element exists.</summary>
    let inline find (predicate : 'T -> bool) (rarr : Rarr<'T>) : 'T =        
        let elementIndex =  rarr.FindIndex (System.Predicate predicate) 
        match elementIndex with
        | -1 ->
            KeyNotFoundException.Raise "Rarr.find did not find for predicate  %A in Rarr of %d items %A" predicate rarr.Count rarr            
        | index ->
            rarr.List.[index]

    /// Return the index of the first element in the Rarr
    /// that satisfies the given predicate.
    let inline tryFindIndex (predicate : 'T -> bool) (rarr : Rarr<'T>) : int option =        
        let elementIndex =   rarr.FindIndex (System.Predicate predicate) 
        match elementIndex with
        | -1 ->
            None
        | index ->
            Some index
        
    /// <summary>
    /// Return the index of the first element in the Rarr
    /// that satisfies the given predicate. Raise <c>KeyNotFoundException</c> if 
    /// none of the elements satisfy the predicate. </summary>
    let inline findIndex (predicate : 'T -> bool) (rarr : Rarr<'T>) : int =        
        let elementIndex =     rarr.FindIndex (System.Predicate predicate)
        match elementIndex with
        | -1 ->
            KeyNotFoundException.Raise "Rarr.findIndex did not find for predicate %A in Rarr of %d items %A" predicate rarr.Count rarr   
        | index ->
            index

    /// Return the index of the first element in the Rarr
    /// that satisfies the given predicate.
    let inline tryFindIndexi predicate (rarr : Rarr<'T>) : int option =        
        let predicate = FSharpFunc<_,_,_>.Adapt predicate
        let lastIndex = rarr.Count - 1
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
    /// Return the index of the first element in the Rarr
    /// that satisfies the given predicate. Raise <c>KeyNotFoundException</c> if 
    /// none of the elements satisfy the predicate.</summary>
    let inline findIndexi predicate (rarr : Rarr<'T>) : int =        
        match tryFindIndexi predicate rarr with
        | Some index ->
            index
        | None ->
            KeyNotFoundException.Raise "Rarr.findIndexi did not find for predicate %A in Rarr of %d items %A" predicate rarr.Count rarr   

    /// <summary>
    /// Applies the given function to successive elements, returning the first
    /// result where function returns <c>Some(x)</c> for some x. If the function
    /// never returns <c>Some(x)</c>, returns <c>None</c>.</summary>
    let inline tryPick (picker : 'T -> 'U option) (rarr : Rarr<'T>) : 'U option =        
        let count = rarr.Count
        let mutable result = None
        let mutable index = 0
        while index < count && Option.isNone result do
            result <- picker rarr.List.[index]
            index <- index + 1
        result

    /// <summary>
    /// Applies the given function to successive elements, returning the first
    /// result where function returns <c>Some(x)</c> for some x. If the function
    /// never returns <c>Some(x)</c>, raises KeyNotFoundException.</summary>
    let inline pick (picker : 'T -> 'U option) (rarr : Rarr<'T>) : 'U =        
        let count = rarr.Count
        let mutable result = None
        let mutable index = 0
        while index < count && Option.isNone result do
            result <- picker rarr.List.[index]
            index <- index + 1
        match result with
        | Some result ->
            result
        | None ->
            KeyNotFoundException.Raise "Rarr.pick did not find for picker %A in Rarr of %d items %A" picker rarr.Count rarr   
            

    /// Apply the given function to each element of the Rarr.
    let inline iter (action : 'T -> unit) (rarr : Rarr<'T>) : unit =  
        let count = rarr.Count
        for i = 0 to count - 1 do
            action rarr.List.[i]

    /// Apply the given function to each element of the Rarr. The integer passed to the
    /// function indicates the index of element.
    let inline iteri (action : int -> 'T -> unit) (rarr : Rarr<'T>) : unit =        
        let action = FSharpFunc<_,_,_>.Adapt action
        let count = rarr.Count
        for i = 0 to count - 1 do
            action.Invoke (i, rarr.List.[i])

    /// <summary>
    /// Apply the given function to two Rarrs simultaneously. The two Rarrs
    /// must have the same lengths, otherwise an <c>ArgumentException</c> is raised.</summary>
    let inline iter2 action (rarr1 : Rarr<'T1>) (rarr2 : Rarr<'T1>) : unit =
        let len = length rarr1
        if len <> length rarr2 then
            ArgumentException.RaiseBase "Rarr.iter2: The Rarrs have different lengths. %d and %d" rarr1.Count rarr2.Count 
        let action = FSharpFunc<_,_,_>.Adapt action
        for i = 0 to len - 1 do
            action.Invoke (rarr1.List.[i], rarr2.List.[i])

    /// <summary>
    /// Apply the given function to pair of elements drawn from matching indices in two Rarrs,
    /// also passing the index of the elements. The two Rarrs must have the same lengths, 
    /// otherwise an <c>ArgumentException</c> is raised.</summary>
    let inline iteri2 action (rarr1 : Rarr<'T1>) (rarr2 : Rarr<'T2>) : unit =
        let len = length rarr1
        if len <> length rarr2 then
            ArgumentException.RaiseBase "Rarr.iteri2: The Rarrs have different lengths. %d and %d" rarr1.Count rarr2.Count 
        let action = FSharpFunc<_,_,_,_>.Adapt action
        for i = 0 to len - 1 do
            action.Invoke (i, rarr1.List.[i], rarr2.List.[i])

    /// <summary>
    /// Build a new Rarr whose elements are the results of applying the given function
    /// to each of the elements of the Rarr.</summary>
    let inline map (mapping : 'T -> 'U) (rarr : Rarr<'T>) : Rarr<'U> =
        rarr.ConvertAll (System.Converter mapping)
        

    /// <summary>
    /// Build a new Rarr whose elements are the results of applying the given function
    /// to each of the elements of the Rarr. The integer index passed to the
    /// function indicates the index of element being transformed. </summary>
    let inline mapi (mapping : int -> 'T -> 'U) (rarr : Rarr<'T>) : Rarr<'U> =        
        let mapping = FSharpFunc<_,_,_>.Adapt mapping
        let count = rarr.Count
        let result = Rarr (count)
        for i = 0 to count - 1 do
            result.Add <| mapping.Invoke (i, rarr.List.[i])
        result

    /// <summary>
    /// Build a new collection whose elements are the results of applying the given function
    /// to the corresponding elements of the two collections pairwise. The two input
    /// Rarrs must have the same lengths. </summary>
    let inline map2 mapping (rarr1 : Rarr<'T1>) (rarr2 : Rarr<'T2>) : Rarr<'U> =
        let len = length rarr1
        if len <> length rarr2 then
            ArgumentException.RaiseBase "Rarr.map2: The Rarrs have different lengths. %d and %d" rarr1.Count rarr2.Count 
        let mapping = FSharpFunc<_,_,_>.Adapt mapping
        let results = Rarr (len)
        for i = 0 to len - 1 do
            mapping.Invoke (rarr1.List.[i], rarr2.List.[i])
            |> results.Add
        results

    /// <summary>
    /// Build a new collection whose elements are the results of applying the given function
    /// to the corresponding elements of the two collections pairwise. The two input
    /// Rarrs must have the same lengths, otherwise an <c>ArgumentException</c> is raised.</summary>
    let inline mapi2 mapping (rarr1 : Rarr<'T1>) (rarr2 : Rarr<'T2>) : Rarr<'U> =
        let len = length rarr1
        if len <> length rarr2 then
            ArgumentException.RaiseBase "Rarr.mapi2: The Rarrs have different lengths. %d and %d" rarr1.Count rarr2.Count
        let mapping = FSharpFunc<_,_,_,_>.Adapt mapping
        let results = Rarr (len)
        for i = 0 to len - 1 do
            mapping.Invoke (i, rarr1.List.[i], rarr2.List.[i])
            |> results.Add
        results

    /// <summary>
    /// Apply a function to each element of the collection, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> 
    /// then computes <c>f (... (f s i0)...) iN</c>.</summary>
    let inline fold (folder : 'State -> 'T -> 'State) state (rarr : Rarr<'T>) : 'State =        
        let folder = FSharpFunc<_,_,_>.Adapt folder
        let mutable state = state
        let count = rarr.Count
        for i = 0 to count - 1 do
            state <- folder.Invoke (state, rarr.List.[i])
        state

    /// <summary>foldSub on just part of teh Rarr</summary>
    let inline foldSub folder (state : 'State) (rarr : Rarr<'T>) startIndex endIndex : 'State =        
        if startIndex < 0 then           IndexOutOfRangeException.Raise "Rarr.foldSub: The starting index cannot be negative. but is %d" startIndex
        elif endIndex > 0 then           IndexOutOfRangeException.Raise "Rarr.foldSub: The ending index cannot be negative. but is %d" endIndex    
        let len = rarr.Count
        if startIndex >= len then            IndexOutOfRangeException.Raise  "Rarr.foldSub: The starting index is outside the bounds of the Rarr: %d of %d" startIndex len
        elif endIndex >= len then            IndexOutOfRangeException.Raise  "Rarr.foldSub: The ending index is outside the bounds of the Rarr: %d of %d" endIndex len
        let folder = FSharpFunc<_,_,_>.Adapt folder
        // Fold over the specified range of items.
        let mutable state = state
        for i = startIndex to endIndex do
            state <- folder.Invoke (state, rarr.List.[i])
        state

    /// <summary>
    /// Applies a function to each element of the collection, threading an accumulator argument
    /// through the computation. The integer index passed to the function indicates the
    /// index of the element within the collection.</summary>
    let inline foldi (folder : 'State -> int -> 'T -> 'State) state (rarr : Rarr<'T>) : 'State =        
        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        let mutable state = state
        let count = rarr.Count
        for i = 0 to count - 1 do
            state <- folder.Invoke (state, i, rarr.List.[i])
        state

    /// <summary>
    /// Apply a function to pairs of elements drawn from the two collections, left-to-right,
    /// threading an accumulator argument through the computation.  The two input Rarrs must
    /// have the same lengths, otherwise an <c>ArgumentException</c> is raised. </summary>
    let inline fold2 folder (state : 'State) (rarr1 : Rarr<'T1>) (rarr2 : Rarr<'T2>) : 'State =
        let len = length rarr1
        if len <> length rarr2 then
            ArgumentException.RaiseBase "Rarr.fold2: The Rarrs have different lengths. %d and %d" rarr1.Count rarr2.Count 
        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        let mutable state = state
        for i = 0 to len - 1 do
            state <- folder.Invoke (state, rarr1.List.[i], rarr2.List.[i])
        state

    /// <summary>
    /// Apply a function to each element of the Rarr, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are
    /// <c>i0...iN</c> then computes <c>f i0 (...(f iN s))</c>. </summary>
    let inline foldBack (folder : 'T -> 'State -> 'State) (rarr : Rarr<'T>) state : 'State =        
        let folder = FSharpFunc<_,_,_>.Adapt folder
        let mutable state = state
        for i = rarr.Count - 1 downto 0 do
            state <- folder.Invoke (rarr.List.[i], state)
        state

    /// <summary>foldBackSub</summary>
    let inline foldBackSub folder (rarr : Rarr<'T>) startIndex endIndex (state : 'State) : 'State =        
        if startIndex < 0 then       IndexOutOfRangeException.Raise "Rarr.foldBackSub: The starting index cannot be negative. but is %d" startIndex
        elif endIndex > 0 then       IndexOutOfRangeException.Raise "Rarr.foldBackSub: The ending index cannot be negative. but is %d" endIndex    
        let len = rarr.Count
        if startIndex >= len then    IndexOutOfRangeException.Raise  "Rarr.foldBackSub: The starting index is outside the bounds of the Rarr: %d of %d" startIndex len
        elif endIndex >= len then    IndexOutOfRangeException.Raise  "Rarr.foldBackSub: The ending index is outside the bounds of the Rarr: %d of %d" endIndex len
        let folder = FSharpFunc<_,_,_>.Adapt folder
        // Fold over the specified range of items.
        let mutable state = state
        for i = endIndex downto startIndex do
            state <- folder.Invoke (rarr.List.[i], state)
        state

    /// <summary>foldiBack</summary>
    let inline foldiBack (folder : int -> 'T -> 'State -> 'State) (rarr : Rarr<'T>) state : 'State =        
        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        let mutable state = state
        for i = rarr.Count - 1 downto 0 do
            state <- folder.Invoke (i, rarr.List.[i], state)
        state

    /// <summary>
    /// Apply a function to pairs of elements drawn from the two collections, right-to-left, 
    /// threading an accumulator argument through the computation.  The two input
    /// Rarrs must have the same lengths, otherwise an <c>ArgumentException</c> is raised.
    /// </summary>
    let inline foldBack2 folder (rarr1 : Rarr<'T1>) (rarr2 : Rarr<'T2>) (state : 'State) : 'State =
        let len = length rarr1
        if len <> length rarr2 then    ArgumentException.RaiseBase "Rarr.foldBack2: The Rarrs have different lengths. %d and %d" rarr1.Count rarr2.Count 
        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        let mutable state = state
        for i = len - 1 downto 0 do
            state <- folder.Invoke (rarr1.List.[i], rarr2.List.[i], state)
        state

    /// <summary>
    /// Apply a function to each element of the Rarr, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> 
    /// then computes <c>f (... (f i0 i1)...) iN</c>.
    /// Raises <c>ArgumentException</c> if the Rarr is empty.</summary> 
    let inline reduce (reduction : 'T -> 'T -> 'T) (rarr : Rarr<'T>) =        
        if isEmpty rarr then ArgumentException.RaiseBase "Rarr.reduce: The given Rarr is empty"
        let reduction = FSharpFunc<_,_,_>.Adapt reduction
        let mutable state = rarr.List.[0]
        let count = rarr.Count
        for i = 1 to count - 1 do
            state <- reduction.Invoke (state, rarr.List.[i])
        state

    /// <summary>
    /// Apply a function to each element of the Rarr, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> then 
    /// computes <c>f i0 (...(f iN-1 iN))</c>.
    /// Raises <c>ArgumentException</c> if the Rarr has size zero.</summary>
    let inline reduceBack (reduction : 'T -> 'T -> 'T) (rarr : Rarr<'T>) : 'T =        
        if isEmpty rarr then ArgumentException.RaiseBase "Rarr.reduceBack: The given Rarr is empty"
        let reduction = FSharpFunc<_,_,_>.Adapt reduction
        let count = rarr.Count
        let mutable state = rarr.List.[count - 1]
        for i = count - 2 downto 0 do
            state <- reduction.Invoke (rarr.List.[i], state)
        state

    /// <summary>scanSub</summary>
    let inline scanSub folder (state : 'State) (rarr : Rarr<'T>) startIndex endIndex : Rarr<'State> =        
        if startIndex < 0 then       IndexOutOfRangeException.Raise "Rarr.scanSub: The starting index cannot be negative. but is %d" startIndex
        elif endIndex > 0 then       IndexOutOfRangeException.Raise "Rarr.scanSub: The ending index cannot be negative. but is %d" endIndex    
        let len = rarr.Count
        if startIndex >= len then    IndexOutOfRangeException.Raise  "Rarr.scanSub: The starting index is outside the bounds of the Rarr: %d of %d" startIndex len
        elif endIndex >= len then    IndexOutOfRangeException.Raise  "Rarr.scanSub: The ending index is outside the bounds of the Rarr: %d of %d" endIndex len
        let folder = FSharpFunc<_,_,_>.Adapt folder
        // Holds the initial and intermediate state values.
        let results = Rarr (endIndex - startIndex + 2)
        results.Add state
        // Fold over the specified range of items.
        let mutable state = state
        for i = startIndex to endIndex do
            state <- folder.Invoke (state, rarr.List.[i])
            results.Add state
        results

    /// <summary> Like <c>fold</c>, but return the intermediary and final results. </summary>
    let inline scan folder (state : 'State) (rarr : Rarr<'T>) : Rarr<'State> =        
        scanSub folder state rarr 0 (rarr.Count - 1)

    /// <summary>scanBackSub</summary>
    let inline scanBackSub folder (rarr : Rarr<'T>) startIndex endIndex (state : 'State) : Rarr<'State> =        
        if startIndex < 0 then            IndexOutOfRangeException.Raise "Rarr.scanBackSub: The starting index cannot be negative. but is %d" startIndex
        elif endIndex > 0 then            IndexOutOfRangeException.Raise "Rarr.scanBackSub: The ending index cannot be negative. but is %d" endIndex    
        let len = rarr.Count
        if startIndex >= len then            IndexOutOfRangeException.Raise  "Rarr.scanBackSub: The starting index is outside the bounds of the Rarr: %d of %d" startIndex len
        elif endIndex >= len then            IndexOutOfRangeException.Raise  "Rarr.scanBackSub: The ending index is outside the bounds of the Rarr: %d of %d" endIndex len
        let folder = FSharpFunc<_,_,_>.Adapt folder
        // Holds the initial and intermediate state values.
        let results = Rarr (endIndex - startIndex + 2)
        results.Add state
        // Fold over the specified range of items.
        let mutable state = state
        for i = endIndex downto startIndex do
            state <- folder.Invoke (rarr.List.[i], state)
            results.Insert (0, state)
        results

    /// <summary> Like <c>foldBack</c>, but return both the intermediary and final results. </summary>
    let inline scanBack folder (rarr : Rarr<'T>) (state : 'State) : Rarr<'State> =        
        scanBackSub folder rarr 0 (rarr.Count - 1) state

    /// <summary>
    /// Split the collection into two collections, containing the elements for which
    /// the given predicate returns <c>true</c> and <c>false</c> respectively.</summary>
    let inline partition predicate (rarr : Rarr<'T>) : Rarr<'T> * Rarr<'T> =        
        let trueResults = Rarr ()
        let falseResults = Rarr ()
        let len = rarr.Count
        for i = 0 to len - 1 do
            let el = rarr.List.[i]
            if predicate el then
                trueResults.Add el
            else
                falseResults.Add el
        trueResults, falseResults

    /// <summary>
    /// Splits the collection into two (2) collections, containing the elements for which the
    /// given function returns <c>Choice1Of2</c> or <c>Choice2Of2</c>, respectively. This function is similar to
    /// <c>Rarr.partition</c>, but it allows the returned collections to have different element types.</summary>
    let inline mapPartition partitioner (rarr : Rarr<'T>) : Rarr<'U1> * Rarr<'U2> =        
        let results1 = Rarr ()
        let results2 = Rarr ()
        let len = rarr.Count
        for i = 0 to len - 1 do
            match partitioner rarr.List.[i] with
            | Choice1Of2 value ->
                results1.Add value
            | Choice2Of2 value ->
                results2.Add value
        results1, results2

    /// <summary>Returns the sum of the elements in the Rarr.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The resulting sum.</returns>
    let inline sum (rarr : Rarr< ^T>) : ^T =         
        if rarr.Count = 0 then ArgumentException.RaiseBase "Rarr.sum: The input Rarr is empty."
        let mutable acc = LanguagePrimitives.GenericZero< (^T) >  
        for i = 0 to rarr.Count - 1 do
            acc <- Checked.(+) acc rarr.List.[i]
        acc

    /// <summary>Returns the sum of the results generated by applying the function to each element of the Rarr.</summary>
    /// <param name="projection">The function to transform the Rarr elements into the type to be summed.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The resulting sum.</returns>
    let inline sumBy (projection : 'T -> ^U) (rarr : Rarr<'T>) : ^U =        
        if rarr.Count = 0 then ArgumentException.RaiseBase "Rarr.sumBy: The input Rarr is empty."
        let mutable acc = LanguagePrimitives.GenericZero< (^U) >    
        for i = 0 to rarr.Count - 1 do
            acc <- Checked.(+) acc (projection rarr.List.[i])
        acc

    /// <summary>Returns the lowest of all elements of the Rarr, compared via Operators.min.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The minimum element.</returns>
    /// <exception cref="System.ArgumentException">Thrown when <paramref name="rarr"/> is empty.</exception>
    let inline min (rarr : Rarr<'T>) =        
        if rarr.Count = 0 then ArgumentException.RaiseBase "Rarr.min: The input Rarr is empty."
        let mutable acc = rarr.List.[0]
        for i = 1 to rarr.Count - 1 do
            let curr = rarr.List.[i]
            if curr < acc then
                acc <- curr
        acc

    /// <summary>Returns the lowest of all elements of the Rarr, compared via Operators.min on the function result.</summary>
    /// <param name="projection">The function to transform the elements into a type supporting comparison.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The minimum element.</returns>
    /// <exception cref="System.ArgumentException">Thrown when <paramref name="rarr"/> is empty.</exception>
    let inline minBy (projection : 'T -> 'U) (rarr : Rarr<'T>) =        
        if rarr.Count = 0 then ArgumentException.RaiseBase "Rarr.minBy: The input Rarr is empty."
        let mutable accv = rarr.List.[0]
        let mutable acc = projection accv
        for i = 1 to rarr.Count - 1 do
            let currv = rarr.List.[i]
            let curr = projection currv
            if curr < acc then
                acc <- curr
                accv <- currv
        accv

    /// <summary>Returns the greatest of all elements of the Rarr, compared via Operators.max on the function result.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The maximum element.</returns>
    /// <exception cref="System.ArgumentException">Thrown when <paramref name="rarr"/> is empty.</exception>
    let inline max (rarr : Rarr<'T>) : ^T=        
        if rarr.Count = 0 then ArgumentException.RaiseBase "Rarr.max: The input Rarr is empty."
        let mutable acc = rarr.List.[0]
        for i = 1 to rarr.Count - 1 do
            let curr = rarr.List.[i]
            if curr > acc then
                acc <- curr
        acc

    /// <summary>Returns the greatest of all elements of the Rarr, compared via Operators.max on the function result.</summary>
    /// <param name="projection">The function to transform the elements into a type supporting comparison.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The maximum element.</returns>
    /// <exception cref="System.ArgumentException">Thrown when <paramref name="rarr"/> is empty.</exception>
    let inline  maxBy (projection : 'T -> 'U) (rarr : Rarr<'T>): ^T =        
        if rarr.Count = 0 then ArgumentException.RaiseBase "Rarr.maxBy: The input Rarr is empty."
        let mutable accv = rarr.List.[0]
        let mutable acc = projection accv
        for i = 1 to rarr.Count - 1 do
            let currv = rarr.List.[i]
            let curr = projection currv
            if curr > acc then
                acc <- curr
                accv <- currv
        accv

    /// <summary>Returns the average of the elements in the Rarr.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The average of the elements in the Rarr.</returns>
    /// <exception cref="System.ArgumentException">Thrown when <paramref name="rarr"/> is empty.</exception>
    let inline average (rarr : Rarr<'T>) : ^T =        
        if rarr.Count = 0 then ArgumentException.RaiseBase "Rarr.average: The input Rarr is empty."
        Seq.average rarr

    /// <summary>Returns the average of the elements generated by applying the function to each element of the Rarr.</summary>
    /// <param name="projection">The function to transform the Rarr elements before averaging.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The computed average.</returns>
    /// <exception cref="System.ArgumentException">Thrown when <paramref name="rarr"/> is empty.</exception>
    let inline averageBy (projection : 'T -> ^U) (rarr : Rarr<'T>) : ^U =        
        if rarr.Count = 0 then ArgumentException.RaiseBase "Rarr.averageBy: The input Rarr is empty."
        Seq.averageBy projection rarr
