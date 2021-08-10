namespace FsEx

open System
open System
open System.Diagnostics
open System.Collections.Generic
open Microsoft.FSharp.Core
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core.Operators
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators

#nowarn "44" // to disable the obsolete warning on accessing Rarr.List

/// Generic operations on Rarr which is like a System.Collections.Generic.List<'T> but with nicer error messages.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] //need this so doesn't hide Rarr class in C# assemblies (should consider for other extension modules as well)
module Rarr =    
    
    //---------------------------------------------------
    //        extensions added only in FsEx:
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
                seq {   for i = 0 to a.Count-3 do yield a.[i], a.[i+1], a.[i+2] } 

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
    let inline rotate k (rarr: Rarr<'T>)  =  
        init rarr.Count (fun i -> rarr.[negIdxLooped (i-k) rarr.Count])

    /// Returns a Rarr of the index and the item. (like enumerate in Python)
    let inline indexed (rarr: Rarr<'T>)  =  
        init rarr.Count (fun i -> i,rarr.[i])


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
    let inline createTEMPOFF count value : Rarr<'T> =
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

    //----------------------------------------------------------------------------------------
    // ------------- implementation adaped form FSharp.Core Array module: ------------------
    //----------------------------------------------------------------------------------------

    /// <summary>Returns a new Rarr that contains all pairings (or combinations)  of elements from the first and second Rarrs.</summary>
    /// <param name="rarr1">The first input Rarr.</param>
    /// <param name="rarr2">The second input Rarr.</param>    
    /// <returns>The resulting Rarr of pairs of length: rarr1.Count * rarr2.Count.</returns>
    let allPairs (rarr1: Rarr<'T>) (rarr2: Rarr<'U>) :Rarr<'T*'U> =
        let res = Rarr(rarr1.Count * rarr2.Count) 
        for i = 0 to rarr1.Count-1 do
            for j = 0 to rarr2.Count-1 do
                res.Add (rarr1.[i], rarr2.[j])
        res
        
                
    /// <summary>Builds a new Rarr that contains the elements of the first Rarr followed by the elements of the second Rarr.</summary>
    /// <param name="rarr1">The first input Rarr.</param>
    /// <param name="rarr2">The second input Rarr.</param>
    /// <returns>The resulting Rarr of length: rarr1.Count + rarr2.Count..</returns>
    let append (rarr1: Rarr<'T>) (rarr2: Rarr<'T>) =
        let res = rarr1.Clone()
        res.AddRange(rarr2)
        res   
        
                
    /// <summary>Returns the average of the elements in the Rarr.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when <c>Rarr</c> is empty.</exception>    
    /// <returns>The average of the elements in the Rarr.</returns>
    let inline average (rarr: Rarr<'T>) = 
        if rarr.Count = 0 then ArgumentException.RaiseBase "Rarr.average: The input Rarr is empty."
        let mutable acc = LanguagePrimitives.GenericZero< ^T>
        for i = 0 to rarr.Count - 1 do
            acc <- Checked.(+) acc rarr.[i]
        LanguagePrimitives.DivideByInt< ^T> acc rarr.Count
        
                
    /// <summary>Returns the average of the elements generated by applying the function to each element of the Rarr.</summary>
    /// <param name="projection">The function to transform the Rarr elements before averaging.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when <c>Rarr</c> is empty.</exception>
    /// <returns>The computed average.</returns>    
    let inline averageBy (projection: 'T -> ^U) (rarr: Rarr<'T>) : ^U = 
        if rarr.Count = 0 then ArgumentException.RaiseBase "Rarr.averageBy: The input Rarr is empty."
        let mutable acc = LanguagePrimitives.GenericZero< ^U>
        for i = 0 to rarr.Count - 1 do
            acc <- Checked.(+) acc (projection rarr.[i])
        LanguagePrimitives.DivideByInt< ^U> acc rarr.Count
        
                
    /// <summary>Applies the given function to each element of the Rarr. Returns
    /// the Rarr comprised of the results "x" for each element where
    /// the function returns Some(x)</summary>
    /// <param name="chooser">The function to generate options from the elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The Rarr of results.</returns>
    let inline choose (chooser : 'T -> 'U option) (rarr : Rarr<'T>) : Rarr<'U> =                
        let result = Rarr ()
        let count = rarr.Count
        for i = 0 to count - 1 do
            match chooser rarr.List.[i] with
            | None -> ()
            | Some value ->
                result.Add value
        result
    

                
    /// <summary>Divides the input Rarr into chunks of size at most <c>chunkSize</c>.</summary>
    /// <param name="chunkSize">The maximum size of each chunk.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The Rarr divided into chunks.</returns>    
    /// <exception cref="T:System.ArgumentOutOfRangeException">Thrown when <c>chunkSize</c> is not positive.</exception>
    let chunkBySize chunkSize (rarr: Rarr<'T>) : Rarr<Rarr<'T>> =
        if chunkSize <= 0 then ArgumentOutOfRangeException.Raise "Rarr.chunkBySize: chunkSize %d must be bigger than 0" chunkSize
        let len = rarr.Count
        if len = 0 then
            Rarr(0)
        elif chunkSize > len then
            Rarr([rarr.Clone()])
        else
            let chunkCount = (len - 1) / chunkSize + 1
            let res = Rarr(chunkCount)
            let mutable sub = Rarr(0)
            for i=0 to rarr.Count-1 do                //TODO Test
                if i % chunkCount = 0 then 
                    sub <- Rarr(chunkSize)
                    res.Add(sub)
                sub.Add rarr.[i]
            res
        
                
    /// <summary>For each element of the Rarr, applies the given function. Concatenates all the results and return the combined Rarr.</summary>
    /// <param name="mapping">The function to create sub-Rarrs from the input Rarr elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The concatenation of the sub-Rarrs.</returns>    
    let collect (mapping: 'T -> seq<'U>)  (rarr: Rarr<'T>) : Rarr<'U>=
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
                result <- comparer rarr1.[i] rarr2.[i]
                i <- i + 1
        else
            while i < rarr2.Count && result = 0 do
                result <- comparer rarr1.[i] rarr2.[i]
                i <- i + 1        
        if result <> 0 then result
        elif length1 = length2 then 0
        elif length1 < length2 then -1
        else 1
        
                
    /// <summary>Builds a new Rarr that contains the elements of each of the given sequence of Rarrs.</summary>
    /// <param name="rarrs">The input sequence of Rarrs.</param>
    /// <returns>The concatenation of the sequence of input Rarrs.</returns>    
    let concat (rarrs: seq<Rarr<'T>>) = 
        let res = Rarr()
        for r in rarrs do
            res.AddRange(r)
        res  
                
    /// <summary>Tests if the Rarr contains the specified element.</summary>
    /// <param name="value">The value to locate in the input Rarr.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>True if the input Rarr contains the specified element; false otherwise.</returns>    
    let inline contains value (rarr: Rarr<'T>) =
        let mutable state = false
        let mutable i = 0
        while not state && i < rarr.Count do
            state <- value = rarr.[i]
            i <- i + 1
        state

    /// <summary>Builds a new Rarr that contains the elements of the given Rarr. 
    /// A shallow copy by calling rarr.GetRange(0,rarr.Count) </summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>A copy of the input Rarr.</returns>    
    let inline copy (rarr: Rarr<'T>) =
        rarr.GetRange(0,rarr.Count) // fastest way to create a shallow copy
        
        
                
    /// <summary>Reads a range of elements from the first Rarr and write them into the second. The target Rarr increases in size if needed</summary>
    /// <param name="source">The source Rarr.</param>
    /// <param name="sourceIndex">The starting index of the source Rarr.</param>
    /// <param name="target">The target Rarr.</param>
    /// <param name="targetIndex">The starting index of the target Rarr.</param>
    /// <param name="count">The number of elements to copy.</param>     
    /// <exception cref="T:System.ArgumentOutOfRangeException">Thrown when any of sourceIndex, targetIndex or count are negative, or when there aren't enough elements in source.</exception>
    let inline blit (source: Rarr<'T>) (sourceIndex: int) (target: Rarr<'T>) (targetIndex: int) (count: int) :unit= 
        if sourceIndex < 0  then  ArgumentOutOfRangeException.Raise "Rarr.blit: sourceIndex %d cannot be negative "sourceIndex
        if targetIndex < 0  then  ArgumentOutOfRangeException.Raise "Rarr.blit: targetIndex %d cannot be negative "targetIndex
        if count < 0  then  ArgumentOutOfRangeException.Raise "Rarr.blit: count %d cannot be negative "count
        if source.LastIndex < sourceIndex + count then  ArgumentOutOfRangeException.Raise "Rarr.blit: source.LastIndex %d is smaller than  sourceIndex %d + count %d"   source.LastIndex  sourceIndex  count
        if target.Count < targetIndex then  ArgumentOutOfRangeException.Raise "Rarr.blit: target.Count %d is smaller than  targetIndex %d"  target.Count  targetIndex      
        let mutable j = targetIndex
        let tlasti = target.Count-1
        for i = sourceIndex to sourceIndex + count - 1 do
            if j > tlasti then 
                target.Add(source.[i])
            else
                target.[j] <- source.[i]
            j<-j+1
    
    
    module private HashingHelp = 
         
         /// a StructBox for keys in case the key type is itself a type using null as a representation
         [<Struct; NoComparison; NoEquality>]
         type StructBox<'T when 'T:equality>(value:'T) = // from fsharp/FSharp.Core/seqcore.fs
             member x.Value = value
             static member Comparer =
                 let gcomparer = HashIdentity.Structural<'T>
                 { new IEqualityComparer<StructBox<'T>> with
                        member _.GetHashCode(v) = gcomparer.GetHashCode(v.Value)
                        member _.Equals(a,b)    = gcomparer.Equals(a.Value, b.Value) }
         
         //------- Count by:

         let inline countByImpl (comparer: IEqualityComparer<'SafeKey>) (projection: 'T->'SafeKey) (getKey: 'SafeKey->'Key) (rarr: Rarr<'T>) =
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
             
         // We avoid wrapping a StructBox, because under 64 JIT we get some "hard" tailcalls which affect performance
         let countByValueType (projection: 'T -> 'Key) (rarr: Rarr<'T>) =
             countByImpl HashIdentity.Structural<'Key> projection id rarr
             
         // Wrap a StructBox around all keys in case the key type is itself a type using null as a representation
         let countByRefType (projection: 'T -> 'Key) (rarr: Rarr<'T>) =
             countByImpl StructBox<'Key>.Comparer (fun t -> StructBox (projection t)) (fun sb -> sb.Value) rarr
         
         //------- Group by:
         
         let inline groupByImpl (comparer: IEqualityComparer<'SafeKey>) (keyf: 'T->'SafeKey) (getKey: 'SafeKey->'Key) (rarr: Rarr<'T>) =
             let length = rarr.Count
             if length = 0 then 
                Rarr(0)
             else
                 let dict = Dictionary<_, Rarr<_>> comparer             
                 // Build the groupings
                 for i = 0 to length - 1 do
                     let v = rarr.[i]
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
             
         // We avoid wrapping a StructBox, because under 64 JIT we get some "hard" tailcalls which affect performance
         let groupByValueType (keyf: 'T->'Key) (rarr: Rarr<'T>) = groupByImpl HashIdentity.Structural<'Key> keyf id rarr
             
         // Wrap a StructBox around all keys in case the key type is itself a type using null as a representation
         let groupByRefType   (keyf: 'T->'Key) (rarr: Rarr<'T>) = groupByImpl StructBox<'Key>.Comparer (fun t -> StructBox (keyf t)) (fun sb -> sb.Value) rarr            
      
             
                
    /// <summary>Applies a key-generating function to each element of an Rarr and returns an Rarr yielding unique
    /// keys and their number of occurrences in the original Rarr.</summary>
    /// <param name="projection">A function transforming each item of the input Rarr into a key to be
    /// compared against the others.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The result Rarr.</returns>    
    let countBy (projection: 'T->'Key) (rarr: Rarr<'T>) : Rarr<'Key * int>=
        if typeof<'Key>.IsValueType
            then HashingHelp.countByValueType projection rarr
            else HashingHelp.countByRefType   projection rarr
        
                
    /// <summary>Creates an Rarr whose elements are all initially the given value.</summary>
    /// <param name="count">The length of the Rarr to create.</param>
    /// <param name="value">The value for the elements.</param>
    /// <returns>The created Rarr.</returns>
    /// <exception cref="T:System.ArgumentOutOfRangeException">Thrown when count is negative.</exception>
    let createTEMPOFF (count: int) (value: 'T) =
        if count < 0 then  ArgumentOutOfRangeException.Raise "Rarr.create: count %d cannot be negative "count
        let rarr= Rarr(count)
        for i = 0 to rarr.Count-1 do // use checked arithmetic here to satisfy FxCop
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
        for v in rarr do 
            if hashSet.Add(v) then
                temp.Add v
        temp
        
                
    /// <summary>Returns an Rarr that contains no duplicate entries according to the 
    /// generic hash and equality comparisons on the keys returned by the given key-generating function.
    /// If an element occurs multiple times in the Rarr then the later occurrences are discarded.</summary>
    /// <param name="projection">A function transforming the Rarr items into comparable keys.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The result Rarr.</returns>    
    let distinctBy projection (rarr: Rarr<'T>) =
        let temp = Rarr()
        let hashSet = HashSet<'T>(HashIdentity.Structural<_>)
        for v in rarr do 
            if hashSet.Add(projection v) then
                temp.Add v
        temp
        
                
    /// <summary>Returns an empty Rarr of the given type.</summary>
    /// <returns>The empty Rarr.</returns>
    [<GeneralizableValue>]
    let empty<'T> : 'T  Rarr = Rarr<'T>(0)
  
                
    /// <summary>Returns the only element of the Rarr.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The only element of the Rarr.</returns>    
    /// <exception cref="T:System.ArgumentException">Thrown when the input does not have precisely one element.</exception>
    let exactlyOne (rarr: Rarr<'T>) =
        if rarr.Count = 1 then rarr.[0]
        else ArgumentException.RaiseBase "Rarr.exactlyOne: Rarr has %d elements, not one." rarr.Count        
        
                
    /// <summary>Returns a new list with the distinct elements of the input Rarr which do not appear in the itemsToExclude sequence,
    /// using generic hash and equality comparisons to compare values.</summary>
    /// <param name="itemsToExclude">A sequence whose elements that also occur in the input Rarr will cause those elements to be
    /// removed from the result.</param>
    /// <param name="rarr">An Rarr whose elements that are not also in itemsToExclude will be returned.</param>
    /// <returns>An Rarr that contains the distinct elements of <c>Rarr</c> that do not appear in <c>itemsToExclude</c>.</returns>    
    let except (itemsToExclude: seq<_>) (rarr: _ Rarr) =        
        if rarr.Count = 0 then
            rarr
        else
            let cached = HashSet(itemsToExclude, HashIdentity.Structural)
            let res = Rarr()
            for i=0 to rarr.LastIndex do 
                let e = rarr.[i]
                if not<| cached.Contains e then 
                    res.Add e
            res
        
                
    /// <summary>Tests if any element of the Rarr satisfies the given predicate.</summary>
    /// <remarks>The predicate is applied to the elements of the input Rarr. If any application 
    /// returns true then the overall result is true and no further elements are tested. 
    /// Otherwise, false is returned.</remarks>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>True if any result from <c>predicate</c> is true.</returns>    
    let exists (predicate: 'T -> bool) (rarr: Rarr<'T>) =
        let len = rarr.Count
        let rec loop i = i < len && (predicate rarr.[i] || loop (i+1))
        len > 0 && loop 0
        
                
    /// <summary>Tests if any pair of corresponding elements of the Rarrs satisfies the given predicate.</summary>
    /// <remarks>The predicate is applied to matching elements in the two collections up to the lesser of the 
    /// two lengths of the collections. If any application returns true then the overall result is 
    /// true and no further elements are tested. Otherwise, if one collections is longer 
    /// than the other then the <c>ArgumentException</c> exception is raised. 
    /// Otherwise, false is returned.</remarks>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="rarr1">The first input Rarr.</param>
    /// <param name="rarr2">The second input Rarr.</param>
    /// <returns>True if any result from <c>predicate</c> is true.</returns>    
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>
    let exists2 predicate (rarr1: _ Rarr) (rarr2: _ Rarr) = 
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(predicate)
        let len1 = rarr1.Count
        if len1 <> rarr2.Count then ArgumentException.RaiseBase "Rarr.exists2: count of rarr1 %d does not match rarr2 %d" rarr2.Count rarr1.Count
        let rec loop i = i < len1 && (f.Invoke(rarr1.[i], rarr2.[i]) || loop (i+1))
        loop 0
        
                
    /// <summary>Fills a range of elements of the Rarr with the given value. Extends the Rarr if needed</summary>
    /// <param name="target">The target Rarr.</param>
    /// <param name="targetIndex">The index of the first element to set.</param>
    /// <param name="count">The number of elements to set.</param>
    /// <param name="value">The value to set.</param>    
    /// <exception cref="T:System.ArgumentOutOfRangeException">Thrown when either targetIndex or count is negative.</exception>
    let fill (target: Rarr<'T>) (targetIndex: int) (count: int) (value: 'T) =        
        if targetIndex < 0  then  ArgumentOutOfRangeException.Raise "Rarr.fill: targetIndex %d cannot be negative "targetIndex
        if count < 0  then  ArgumentOutOfRangeException.Raise "Rarr.fill: count %d cannot be negative "count
        if target.Count < targetIndex then  ArgumentOutOfRangeException.Raise "Rarr.fill: target.Count %d is smaller than  targetIndex %d"   target.Count  targetIndex 
        let tlasti = target.Count-1
        for j = targetIndex to targetIndex + count - 1 do 
            if j > tlasti then 
                target.Add(value)
            else
                target.[j] <- value           
        
        
                
    /// <summary>Returns a new collection containing only the elements of the collection
    /// for which the given predicate returns "true".</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>An Rarr containing the elements for which the given predicate returns true.</returns>    
    let filter predicate (rarr: _ Rarr) =         
        rarr.FindAll (System.Predicate predicate)   
                    
                
    /// <summary>Returns the first element for which the given function returns 'true'.
    /// Raise <see cref="T:System.Collections.Generic.KeyNotFoundException"/> if no such element exists.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="rarr">The input Rarr.</param>    
    /// <exception cref="T:System.Collections.Generic.KeyNotFoundException">Thrown if <c>predicate</c> never returns true.</exception>
    /// <returns>The first element for which <c>predicate</c> returns true.</returns>
    let find predicate (rarr: _ Rarr) = 
        let rec loop i =             
            if i >= rarr.Count then  
                KeyNotFoundException.Raise "Rarr.find: not found in %d items %A" rarr.Count rarr
            else
                if predicate rarr.[i] then rarr.[i]  else loop (i+1)
        loop 0 
        
                
    /// <summary>Returns the last element for which the given function returns 'true'.
    /// Raise <see cref="T:System.Collections.Generic.KeyNotFoundException"/> if no such element exists.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <exception cref="T:System.Collections.Generic.KeyNotFoundException">Thrown if <c>predicate</c> never returns true.</exception>    
    /// <returns>The last element for which <c>predicate</c> returns true.</returns>
    let findBack predicate (rarr: _ Rarr) =
        let rec loop i =             
            if i < 0 then  
                KeyNotFoundException.Raise "Rarr.findBack: not found in %d items %A" rarr.Count rarr
            else
                if predicate rarr.[i] then rarr.[i]  else loop (i-1)
        loop rarr.LastIndex
        
                
    /// <summary>Returns the index of the first element in the Rarr that satisfies the given predicate. Raise <see cref="T:System.Collections.Generic.KeyNotFoundException"/> if 
    /// none of the elements satisfy the predicate.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <exception cref="T:System.Collections.Generic.KeyNotFoundException">Thrown if <c>predicate</c> never returns true.</exception>    
    /// <returns>The index of the first element in the Rarr that satisfies the given predicate.</returns>
    let findIndex predicate (rarr: _ Rarr) = 
        let len = rarr.Count 
        let rec go n = 
            if n >= len then 
                KeyNotFoundException.Raise "Rarr.findIndex: not found in %d items %A" rarr.Count rarr
            elif predicate rarr.[n] then
                n 
            else go (n+1)
        go 0
        
                
    /// <summary>Returns the index of the last element in the Rarr
    /// that satisfies the given predicate. Raise <see cref="T:System.Collections.Generic.KeyNotFoundException"/> if
    /// none of the elements satisfy the predicate.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <exception cref="T:System.Collections.Generic.KeyNotFoundException">Thrown if <c>predicate</c> never returns true.</exception>    
    /// <returns>The index of the last element in the Rarr that satisfies the given predicate.</returns>
    let findIndexBack predicate (rarr: _ Rarr) =         
        let rec go n = 
            if n < 0 then 
                KeyNotFoundException.Raise "Rarr.findIndexBack: not found in %d items %A" rarr.Count rarr
            elif predicate rarr.[n] then
                n 
            else go (n-1)
        go rarr.LastIndex
        
                
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
        for i = 0 to rarr.Count-1 do 
            state <- f.Invoke(state, rarr.[i])
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
        let f = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt(folder)
        let mutable state = state 
        if rarr1.Count <> rarr2.Count then ArgumentException.RaiseBase "Rarr.fold2: count of rarr1 %d does not match rarr2 %d" rarr2.Count rarr1.Count
        for i = 0 to rarr1.Count-1 do 
            state <- f.Invoke(state, rarr1.[i], rarr2.[i])
        state
        
    //let foldSubRight f (rarr: _ Rarr) start fin acc = 
    //    let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(f)
    //    let mutable res = acc 
    //    for i = fin downto start do
    //        res <- f.Invoke(rarr.[i], res)
    //    res
    //    
    //let scanSubLeft f initState (rarr: _ Rarr) start fin = 
    //    let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(f)
    //    let mutable state = initState 
    //    let res = create (2+fin-start) initState 
    //    for i = start to fin do
    //        state <- f.Invoke(state, rarr.[i])
    //        res.[i - start+1] <- state
    //    res
        
                
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
        for i = rarr.Count-1 downto 0 do 
            res <- f.Invoke(rarr.[i], res)
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
        let f = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt(folder)
        let mutable res = state 
        let len = rarr1.Count
        if len <> rarr2.Count then ArgumentException.RaiseBase "Rarr.foldBack2: count of rarr1 %d does not match rarr2 %d" rarr2.Count rarr1.Count
        for i = len-1 downto 0 do 
            res <- f.Invoke(rarr1.[i], rarr2.[i], res)
        res
        
                
    /// <summary>Tests if all elements of the Rarr satisfy the given predicate.</summary>
    /// <remarks>The predicate is applied to the elements of the input collection. If any application 
    /// returns false then the overall result is false and no further elements are tested. 
    /// Otherwise, true is returned.</remarks>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>True if all of the Rarr elements satisfy the predicate.</returns>
    
    let forall (predicate: 'T -> bool) (rarr: Rarr<'T>) =
        let len = rarr.Count
        let rec loop i = i >= len || (predicate rarr.[i] && loop (i+1))
        loop 0
        
                
    /// <summary>Tests if all corresponding elements of the Rarr satisfy the given predicate pairwise.</summary>
    /// <remarks>The predicate is applied to matching elements in the two collections up to the lesser of the 
    /// two lengths of the collections. If any application returns false then the overall result is 
    /// false and no further elements are tested. Otherwise, if one collection is longer 
    /// than the other then the <c>ArgumentException</c> exception is raised. 
    /// Otherwise, true is returned.</remarks>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="rarr1">The first input Rarr.</param>
    /// <param name="rarr2">The second input Rarr.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>
    /// <returns>True if all of the Rarr elements satisfy the predicate.</returns>
    let forall2 predicate (rarr1: _ Rarr) (rarr2: _ Rarr) = 
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(predicate)
        let len1 = rarr1.Count
        if len1 <> rarr2.Count then ArgumentException.RaiseBase "Rarr.forall2: count of rarr1 %d does not match rarr2 %d" rarr2.Count rarr1.Count
        let rec loop i = i >= len1 || (f.Invoke(rarr1.[i], rarr2.[i]) && loop (i+1))
        loop 0
        
    
        
                
    /// <summary>Gets an element from an Rarr.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <param name="index">The input index.</param>
    /// <returns>The value of the Rarr at the given index.</returns>    
    /// <exception cref="T:System.IndexOutOfRangeException">Thrown when the index is negative or the input Rarr does not contain enough elements.</exception>
    let get (rarr: _ Rarr) index = 
        rarr.[index]
        
                
    /// <summary>Builds a new Rarr that contains the given subrange specified by
    /// starting index and length.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <param name="startIndex">The index of the first element of the sub Rarr.</param>
    /// <param name="count">The length of the sub Rarr.</param>
    /// <returns>The created sub Rarr.</returns>    
    /// <exception cref="T:System.ArgumentOutOfRangeException">Thrown when either startIndex or count is negative,
    /// or when there aren't enough elements in the input Rarr.</exception>
    let sub (rarr: Rarr<'T>) (startIndex: int) (count: int) =
        // nice ArgumentOutOfRangeException will be thrown by this 
        rarr.GetRange(startIndex,count)
        
                
    /// <summary>Applies a key-generating function to each element of an Rarr and yields an Rarr of 
    /// unique keys. Each unique key contains an Rarr of all elements that match 
    /// to this key.</summary>
    /// <param name="projection">A function that transforms an element of the Rarr into a comparable key.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The result Rarr.</returns>
    
    let groupBy (projection: 'T->'Key) (rarr: Rarr<'T>) =
        if typeof<'Key>.IsValueType
            then groupByValueType projection rarr
            else groupByRefType   projection rarr
        
                
    /// <summary>Returns the first element of the Rarr.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The first element of the Rarr.</returns>    
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarr is empty.</exception>
    let head (rarr: Rarr<'T>) =
        if rarr.Count = 0 then ArgumentException.RaiseBase "Rarr.XXXXXXXXXXXXXXX: The input Rarr is empty." else rarr.[0]
        
                
    /// <summary>Builds a new Rarr whose elements are the corresponding elements of the input Rarr
    /// paired with the integer index (from 0) of each element.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The Rarr of indexed elements.</returns>
    
    let indexed (rarr: Rarr<'T>) =
        let res = Rarr.create rarr.Count Unchecked.defaultof<_>
        for i = 0 to res.Count-1 do
            res.[i] <- (i, rarr.[i])
        res
        
                
    /// <summary>Creates an Rarr given the dimension and a generator function to compute the elements.</summary>
    /// <param name="count">The number of elements to initialize.</param>
    /// <param name="initializer">The function to generate the initial values for each index.</param>
    /// <returns>The created Rarr.</returns>
    /// <exception cref="T:System.ArgumentOutOfRangeException">Thrown when count is negative.</exception>
    let inline init count initializer = Microsoft.FSharp.Primitives.Basics.Rarr.init count initializer
        
                
    /// <summary>Returns true if the given Rarr is empty, otherwise false.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>True if the Rarr is empty.</returns>
    
    let isEmpty (rarr: Rarr<'T>) = 
        rarr.Count = 0
        
                
    /// <summary>Gets an element from an Rarr.</summary>
    /// <param name="index">The input index.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The value of the Rarr at the given index.</returns>
    
    /// <exception cref="T:System.IndexOutOfRangeException">Thrown when the index is negative or the input Rarr does not contain enough elements.</exception>
    let item index (rarr: _ Rarr) =
        rarr.[index]
        
                
    /// <summary>Applies the given function to each element of the Rarr.</summary>
    /// <param name="action">The function to apply.</param>
    /// <param name="rarr">The input Rarr.</param>
    
    let inline iter ((*[<InlineIfLambda>]*) action) (rarr: Rarr<'T>) = 
        for i = 0 to rarr.Count-1 do 
            action rarr.[i]
        
                
    /// <summary>Applies the given function to pair of elements drawn from matching indices in two Rarrs. The
    /// two Rarrs must have the same lengths, otherwise an <c>ArgumentException</c> is
    /// raised.</summary>
    /// <param name="action">The function to apply.</param>
    /// <param name="rarr1">The first input Rarr.</param>
    /// <param name="rarr2">The second input Rarr.</param>

    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>
    let iter2 action (rarr1: Rarr<'T>) (rarr2: Rarr<'U>) = 
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(action)
        if rarr1.Count <> rarr2.Count then invalidArgDifferentRarrLength "rarr1" rarr1.Count "rarr2" rarr2.Count
        for i = 0 to rarr1.Count-1 do 
            f.Invoke(rarr1.[i], rarr2.[i])
        
                
    /// <summary>Applies the given function to each element of the Rarr. The integer passed to the
    /// function indicates the index of element.</summary>
    /// <param name="action">The function to apply to each index and element.</param>
    /// <param name="rarr">The input Rarr.</param>
    
    let iteri action (rarr: Rarr<'T>) =
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(action)            
        for i = 0 to rarr.Count-1 do 
            f.Invoke(i, rarr.[i])
        
                
    /// <summary>Applies the given function to pair of elements drawn from matching indices in two Rarrs,
    /// also passing the index of the elements. The two Rarrs must have the same lengths, 
    /// otherwise an <c>ArgumentException</c> is raised.</summary>
    /// <param name="action">The function to apply to each index and pair of elements.</param>
    /// <param name="rarr1">The first input Rarr.</param>
    /// <param name="rarr2">The second input Rarr.</param>

    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>
    let iteri2 action (rarr1: Rarr<'T>) (rarr2: Rarr<'U>) = 
        let f = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt(action)
        if rarr1.Count <> rarr2.Count then invalidArgDifferentRarrLength "rarr1" rarr1.Count "rarr2" rarr2.Count
        for i = 0 to rarr1.Count-1 do 
            f.Invoke(i, rarr1.[i], rarr2.[i])
        
                
    /// <summary>Returns the last element of the Rarr.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The last element of the Rarr.</returns>
    
    /// <exception cref="T:System.ArgumentException">Thrown when the input does not have any elements.</exception>
    let inline last (rarr: Rarr<'T>) =
        if rarr.Count = 0 then ArgumentException.RaiseBase "Rarr.XXXXXXXXXXXXXXX: The input Rarr is empty."
        rarr.[rarr.Count-1]
        
                
    /// <summary>Returns the length of an Rarr. You can also use property arr.Length.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The length of the Rarr.</returns>
    
    let length (rarr: _ Rarr)    = 
        rarr.Count
                
                
    /// <summary>Builds a new Rarr whose elements are the results of applying the given function
    /// to each of the elements of the Rarr.</summary>
    /// <param name="mapping">The function to transform elements of the Rarr.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The Rarr of transformed elements.</returns>
    
    let inline map ((*[<InlineIfLambda>]*) mapping: 'T -> 'U) (rarr: Rarr<'T>) =
        let res: Rarr<'U> = Rarr.create rarr.Count Unchecked.defaultof<_>
        for i = 0 to res.Count-1 do 
            res.[i] <- mapping rarr.[i]
        res
        
                
    /// <summary>Builds a new collection whose elements are the results of applying the given function
    /// to the corresponding elements of the two collections pairwise. The two input
    /// Rarrs must have the same lengths, otherwise an <c>ArgumentException</c> is
    /// raised.</summary>
    /// <param name="mapping">The function to transform the pairs of the input elements.</param>
    /// <param name="rarr1">The first input Rarr.</param>
    /// <param name="rarr2">The second input Rarr.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>

    /// <returns>The Rarr of transformed elements.</returns>
    let map2 mapping (rarr1: Rarr<'T>) (rarr2: Rarr<'U>) = 
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(mapping)
        if rarr1.Count <> rarr2.Count then invalidArgDifferentRarrLength "rarr1" rarr1.Count "rarr2" rarr2.Count
        let res = Rarr.create rarr1.Count Unchecked.defaultof<_>
        for i = 0 to res.Count-1 do 
            res.[i] <- f.Invoke(rarr1.[i], rarr2.[i])
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
    DELTE LINEThrown when any of the input Rarrs is null.</exception>
    /// <returns>The Rarr of transformed elements.</returns>
    let map3 mapping (rarr1: 'T1 Rarr) (rarr2: 'T2 Rarr) (rarr3: 'T3 Rarr) = 
        let f = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt(mapping)
        let len1 = rarr1.Count
        if len1 <> rarr2.Count || len1 <> rarr3.Count then invalidArg3RarrsDifferent "rarr1" "rarr2" "rarr3" len1 rarr2.Count rarr3.Count
                    
        let res = Rarr.create len1 Unchecked.defaultof<_>
        for i = 0 to res.Count-1 do
            res.[i] <- f.Invoke(rarr1.[i], rarr2.[i], rarr3.[i])
        res
        
                
    /// <summary>Combines map and fold. Builds a new Rarr whose elements are the results of applying the given function
    /// to each of the elements of the input Rarr. The function is also used to accumulate a final value.</summary>
    /// <param name="mapping">The function to transform elements from the input Rarr and accumulate the final value.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="rarr">The input Rarr.</param>
    
    /// <returns>The Rarr of transformed elements, and the final accumulated value.</returns>
    let mapFold<'T, 'State, 'Result> (mapping: 'State -> 'T -> 'Result * 'State) state rarr =
        Microsoft.FSharp.Primitives.Basics.Rarr.mapFold mapping state rarr
        
                
    /// <summary>Combines map and foldBack. Builds a new Rarr whose elements are the results of applying the given function
    /// to each of the elements of the input Rarr. The function is also used to accumulate a final value.</summary>
    /// <param name="mapping">The function to transform elements from the input Rarr and accumulate the final value.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <param name="state">The initial state.</param>
    
    /// <returns>The Rarr of transformed elements, and the final accumulated value.</returns>
    let mapFoldBack<'T, 'State, 'Result> (mapping: 'T -> 'State -> 'Result * 'State) rarr state =
        Microsoft.FSharp.Primitives.Basics.Rarr.mapFoldBack mapping rarr state
        
                
    /// <summary>Builds a new Rarr whose elements are the results of applying the given function
    /// to each of the elements of the Rarr. The integer index passed to the
    /// function indicates the index of element being transformed.</summary>
    /// <param name="mapping">The function to transform elements and their indices.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The Rarr of transformed elements.</returns>
    
    let mapi (mapping: int -> 'T -> 'U) (rarr: Rarr<'T>) =
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(mapping)            
        let res = Rarr.create rarr.Count Unchecked.defaultof<_>
        for i = 0 to rarr.Count-1 do 
            res.[i] <- f.Invoke(i, rarr.[i])
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
    let mapi2 mapping (rarr1: Rarr<'T>) (rarr2: Rarr<'U>) = 
        let f = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt(mapping)
        if rarr1.Count <> rarr2.Count then invalidArgDifferentRarrLength "rarr1" rarr1.Count "rarr2" rarr2.Count
        let res = Rarr.create rarr1.Count  Unchecked.defaultof<_>
        for i = 0 to res.Count-1 do 
            res.[i] <- f.Invoke(i, rarr1.[i], rarr2.[i])
        res
        
                
    /// <summary>Returns the greatest of all elements of the Rarr, compared via Operators.max on the function result.</summary>
    /// <remarks>Throws ArgumentException for empty Rarrs.</remarks>
    /// <param name="rarr">The input Rarr.</param>
    
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarr is empty.</exception>
    /// <returns>The maximum element.</returns>
    let inline max (rarr: _ Rarr) = 
        if rarr.Count = 0 then ArgumentException.RaiseBase "Rarr.XXXXXXXXXXXXXXX: The input Rarr is empty."
        let mutable acc = rarr.[0]
        for i = 1 to rarr.Count - 1 do
            let curr = rarr.[i]
            if curr > acc then 
                acc <- curr
        acc
        
                
    /// <summary>Returns the greatest of all elements of the Rarr, compared via Operators.max on the function result.</summary>
    /// <remarks>Throws ArgumentException for empty Rarrs.</remarks>
    /// <param name="projection">The function to transform the elements into a type supporting comparison.</param>
    /// <param name="rarr">The input Rarr.</param>
    
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarr is empty.</exception>
    /// <returns>The maximum element.</returns>
    let inline maxBy projection (rarr: _ Rarr) = 
        if rarr.Count = 0 then ArgumentException.RaiseBase "Rarr.XXXXXXXXXXXXXXX: The input Rarr is empty."
        let mutable accv = rarr.[0]
        let mutable acc = projection accv
        for i = 1 to rarr.Count - 1 do
            let currv = rarr.[i]
            let curr = projection currv
            if curr > acc then
                acc <- curr
                accv <- currv
        accv
        
                
    /// <summary>Returns the lowest of all elements of the Rarr, compared via Operators.min.</summary>
    /// <remarks>Throws ArgumentException for empty Rarrs</remarks>
    /// <param name="rarr">The input Rarr.</param>
    
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarr is empty.</exception>
    /// <returns>The minimum element.</returns>
    let inline min (rarr: _ Rarr) = 
        if rarr.Count = 0 then ArgumentException.RaiseBase "Rarr.XXXXXXXXXXXXXXX: The input Rarr is empty."
        let mutable acc = rarr.[0]
        for i = 1 to rarr.Count - 1 do
            let curr = rarr.[i]
            if curr < acc then 
                acc <- curr
        acc
        
                
    /// <summary>Returns the lowest of all elements of the Rarr, compared via Operators.min on the function result.</summary>
    /// <remarks>Throws ArgumentException for empty Rarrs.</remarks>
    /// <param name="projection">The function to transform the elements into a type supporting comparison.</param>
    /// <param name="rarr">The input Rarr.</param>
    
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarr is empty.</exception>
    /// <returns>The minimum element.</returns>
    let inline minBy ((*[<InlineIfLambda>]*) projection) (rarr: _ Rarr) = 
        if rarr.Count = 0 then ArgumentException.RaiseBase "Rarr.XXXXXXXXXXXXXXX: The input Rarr is empty."
        let mutable accv = rarr.[0]
        let mutable acc = projection accv
        for i = 1 to rarr.Count - 1 do
            let currv = rarr.[i]
            let curr = projection currv
            if curr < acc then
                acc <- curr
                accv <- currv
        accv
        
                
    /// <summary>Builds an Rarr from the given list.</summary>
    /// <param name="list">The input list.</param>
    /// <returns>The Rarr of elements from the list.</returns>
    let ofList list =
        List.toRarr list
        
                
    /// <summary>Builds a new Rarr from the given enumerable object.</summary>
    /// <param name="source">The input sequence.</param>
    /// <returns>The Rarr of elements from the sequence.</returns>
    
    let ofSeq source = 
        Seq.toRarr source
        
                
    /// <summary>Returns an Rarr of each element in the input Rarr and its predecessor, with the
    /// exception of the first element which is only returned as the predecessor of the second element.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The result Rarr.</returns>
    
    let pairwise (rarr: Rarr<'T>) =
        if rarr.Count < 2 then empty else
        init (rarr.Count-1) (fun i -> rarr.[i], rarr.[i+1])
        
                
    /// <summary>Splits the collection into two collections, containing the 
    /// elements for which the given predicate returns "true" and "false"
    /// respectively.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>A pair of Rarrs. The first containing the elements the predicate evaluated to true,
    /// and the second containing those evaluated to false.</returns>
    
    let partition predicate (rarr: _ Rarr) = 
        let res = Rarr.create rarr.Count         Unchecked.defaultof<_>
        let mutable upCount = 0
        let mutable downCount = rarr.Count-1    
        for x in rarr do                
            if predicate x then 
                res.[upCount] <- x
                upCount <- upCount + 1
            else
                res.[downCount] <- x
                downCount <- downCount - 1
                        
        let res1 = Microsoft.FSharp.Primitives.Basics.Rarr.subUnchecked 0 upCount res
        let res2 = Rarr.create (rarr.Count - upCount)     Unchecked.defaultof<_>
                
        downCount <- rarr.Count-1
        for i = 0 to res2.Count-1 do
            res2.[i] <- res.[downCount]
            downCount <- downCount - 1
                
        res1, res2
        
                
    /// <summary>Returns an Rarr with all elements permuted according to the
    /// specified permutation.</summary>
    /// <param name="indexMap">The function that maps input indices to output indices.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The output Rarr.</returns>
    
    /// <exception cref="T:System.ArgumentException">Thrown when indexMap does not produce a valid permutation.</exception>
    let permute indexMap (rarr: _ Rarr) =  
        Microsoft.FSharp.Primitives.Basics.Rarr.permute indexMap rarr
        
                
    /// <summary>Applies the given function to successive elements, returning the first
    /// result where function returns <c>Some(x)</c> for some <c>x</c>. If the function 
    /// never returns <c>Some(x)</c> then <see cref="T:System.Collections.Generic.KeyNotFoundException"/> is raised.</summary>
    /// <param name="chooser">The function to generate options from the elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    
    /// <exception cref="T:System.Collections.Generic.KeyNotFoundException">Thrown if every result from
    /// <c>chooser</c> is <c>None</c>.</exception>
    /// <returns>The first result.</returns>
    let pick chooser (rarr: _ Rarr) = 
        let rec loop i = 
            if i >= rarr.Count then 
                indexNotFound()
            else 
                match chooser rarr.[i] with 
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
    let reduce reduction (rarr: _ Rarr) = 
        let len = rarr.Count
        if len = 0 then 
            invalidArg "rarr" LanguagePrimitives.ErrorStrings.InputRarrEmptyString
        else 
            let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(reduction)
            let mutable res = rarr.[0]
            for i = 1 to rarr.Count-1 do
                res <- f.Invoke(res, rarr.[i])
            res
        
                
    /// <summary>Applies a function to each element of the Rarr, starting from the end, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> 
    /// then computes <c>f i0 (...(f iN-1 iN))</c>.</summary>
    /// <param name="reduction">A function that takes in the next-to-last element of the list and the
    /// current accumulated result to produce the next accumulated result.</param>
    /// <param name="rarr">The input Rarr.</param>
    
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarr is empty.</exception>
    /// <returns>The final result of the reductions.</returns>
    let reduceBack reduction (rarr: _ Rarr) = 
        let len = rarr.Count
        if len = 0 then invalidArg "rarr" LanguagePrimitives.ErrorStrings.InputRarrEmptyString
        else foldSubRight reduction rarr 0 (len - 2) rarr.[len - 1]
        
                
    /// <summary>Creates an Rarr by replicating the given initial value.</summary>
    /// <param name="count">The number of elements to replicate.</param>
    /// <param name="initial">The value to replicate</param>
    /// <returns>The generated Rarr.</returns>
    /// <exception cref="T:System.ArgumentOutOfRangeException">Thrown when count is negative.</exception>
    let replicate count initial = 
        if count < 0 then  ArgumentOutOfRangeException.Raise "Rarr.XXXX: count %d cannot be negative "count
        let arr: 'T rarr = Rarr.create count Unchecked.defaultof<_>
        for i = 0 to arr.Count-1 do 
            arr.[i] <- initial
        arr
        
                
    /// <summary>Returns a new Rarr with the elements in reverse order.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The reversed Rarr.</returns>
    
    let rev (rarr: _ Rarr) = 
        let res = Rarr.create rarr.Count Unchecked.defaultof<_>
        let mutable j = rarr.Count-1
        for i = 0 to rarr.Count-1 do 
            res.[j] <- rarr.[i]
            j <- j - 1
        res
        
                
    /// <summary>Like <c>fold</c>, but return the intermediary and final results.</summary>
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The Rarr of state values.</returns>
    
    let scan<'T, 'State> folder (state: 'State) (rarr: Rarr<'T>) = 
        let len = rarr.Count
        scanSubLeft folder state rarr 0 (len - 1)
        
                
    /// <summary>Like <c>foldBack</c>, but return both the intermediary and final results.</summary>
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <param name="state">The initial state.</param>
    /// <returns>The Rarr of state values.</returns>
    
    let scanBack<'T, 'State> folder (rarr: Rarr<'T>) (state: 'State) = 
        Microsoft.FSharp.Primitives.Basics.Rarr.scanSubRight folder rarr 0 (rarr.Count - 1) state
        
                
    /// <summary>Sets an element of an Rarr.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <param name="index">The input index.</param>
    /// <param name="value">The input value.</param>
    
    /// <exception cref="T:System.IndexOutOfRangeException">Thrown when the index is negative or the input Rarr does not contain enough elements.</exception>
    let set (rarr: _ Rarr) index value = 
        rarr.[index] <- value
        
                
    /// <summary>Returns an Rarr that contains one item only.</summary>
    /// <param name="value">The input item.</param>
    /// <returns>The result Rarr of one item.</returns>
    let inline singleton value = [|value|]
        
                
    /// <summary>Builds a new Rarr that contains the elements of the given Rarr, excluding the first N elements.</summary>
    /// <param name="count">The number of elements to skip.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>A copy of the input Rarr, after removing the first N elements.</returns>
    
    /// <exception cref="T:System.ArgumentExcepion">Thrown when count is negative or exceeds the number of 
    /// elements in the Rarr.</exception>
    let skip count (rarr: Rarr<'T>) =
        if count > rarr.Count then invalidArgOutOfRange "count" count "rarr.Count" rarr.Count
        if count = rarr.Count then
            empty
        else
            let count = max count 0
            Microsoft.FSharp.Primitives.Basics.Rarr.subUnchecked count (rarr.Count - count) rarr
        
                
    /// <summary>Bypasses elements in an Rarr while the given predicate returns True, and then returns
    /// the remaining elements in a new Rarr.</summary>
    /// <param name="predicate">A function that evaluates an element of the Rarr to a boolean value.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The created sub Rarr.</returns>
    
    let skipWhile predicate (rarr: Rarr<'T>) =        
        let mutable i = 0            
        while i < rarr.Count && predicate rarr.[i] do i <- i + 1
        
        match rarr.Count - i with
        | 0 -> empty
        | resLen -> Microsoft.FSharp.Primitives.Basics.Rarr.subUnchecked i resLen rarr
        
                
    /// <summary>Sorts the elements of an Rarr, returning a new Rarr. Elements are compared using <see cref="M:Microsoft.FSharp.Core.Operators.compare"/>. </summary>
    /// <remarks>This is not a stable sort, i.e. the original order of equal elements is not necessarily preserved. 
    /// For a stable sort, consider using <see cref="M:Microsoft.FSharp.Collections.SeqModule.Sort"/>.</remarks>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The sorted Rarr.</returns>
    
    let sort rarr = 
        let result = copy rarr
        sortInPlace result
        result
        
                
    /// <summary>Sorts the elements of an Rarr, using the given projection for the keys and returning a new Rarr. 
    /// Elements are compared using <see cref="M:Microsoft.FSharp.Core.Operators.compare"/>.</summary>
    /// <remarks>This is not a stable sort, i.e. the original order of equal elements is not necessarily preserved. 
    /// For a stable sort, consider using <see cref="M:Microsoft.FSharp.Collections.SeqModule.Sort"/>.</remarks>
    /// <param name="projection">The function to transform Rarr elements into the type that is compared.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The sorted Rarr.</returns>
    
    let sortBy projection rarr =
        let result = copy rarr
        sortInPlaceBy projection result
        result
        
                
    /// <summary>Sorts the elements of an Rarr, in descending order, using the given projection for the keys and returning a new Rarr. 
    /// Elements are compared using <see cref="M:Microsoft.FSharp.Core.Operators.compare"/>.</summary>
    /// <remarks>This is not a stable sort, i.e. the original order of equal elements is not necessarily preserved. 
    /// For a stable sort, consider using <see cref="M:Microsoft.FSharp.Collections.SeqModule.Sort"/>.</remarks>
    /// <param name="projection">The function to transform Rarr elements into the type that is compared.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The sorted Rarr.</returns>
    let inline sortByDescending projection rarr =
        let inline compareDescending a b = compare (projection b) (projection a)
        sortWith compareDescending rarr
        
                
    /// <summary>Sorts the elements of an Rarr, in descending order, returning a new Rarr. Elements are compared using <see cref="M:Microsoft.FSharp.Core.Operators.compare"/>. </summary>
    /// <remarks>This is not a stable sort, i.e. the original order of equal elements is not necessarily preserved. 
    /// For a stable sort, consider using <see cref="M:Microsoft.FSharp.Collections.SeqModule.Sort"/>.</remarks>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The sorted Rarr.</returns>
    let inline sortDescending rarr =
        let inline compareDescending a b = compare b a
        sortWith compareDescending rarr
        
                
    /// <summary>Sorts the elements of an Rarr by mutating the Rarr in-place, using the given comparison function. 
    /// Elements are compared using <see cref="M:Microsoft.FSharp.Core.Operators.compare"/>.</summary>
    /// <param name="rarr">The input Rarr.</param>
    
    let sortInPlace (rarr: Rarr<'T>) = 
        Microsoft.FSharp.Primitives.Basics.Rarr.unstableSortInPlace rarr
        
                
    /// <summary>Sorts the elements of an Rarr by mutating the Rarr in-place, using the given projection for the keys. 
    /// Elements are compared using <see cref="M:Microsoft.FSharp.Core.Operators.compare"/>.</summary>
    /// <remarks>This is not a stable sort, i.e. the original order of equal elements is not necessarily preserved. 
    /// For a stable sort, consider using <see cref="M:Microsoft.FSharp.Collections.SeqModule.Sort"/>.</remarks>
    /// <param name="projection">The function to transform Rarr elements into the type that is compared.</param>
    /// <param name="rarr">The input Rarr.</param>
    
    let sortInPlaceBy (projection: 'T -> 'U) (rarr: Rarr<'T>) = 
        Microsoft.FSharp.Primitives.Basics.Rarr.unstableSortInPlaceBy projection rarr
        
                
    /// <summary>Sorts the elements of an Rarr by mutating the Rarr in-place, using the given comparison function as the order.</summary>
    /// <param name="comparer">The function to compare pairs of Rarr elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    
    let sortInPlaceWith comparer (rarr: Rarr<'T>) =
        let len = rarr.Count 
        if len < 2 then () 
        elif len = 2 then 
            let c = comparer rarr.[0] rarr.[1] 
            if c > 0 then
                let tmp = rarr.[0] 
                rarr.[0] <- rarr.[1]
                rarr.[1] <- tmp
        else 
            Rarr.Sort(rarr, ComparisonIdentity.FromFunction(comparer))
        
                
    /// <summary>Sorts the elements of an Rarr, using the given comparison function as the order, returning a new Rarr.</summary>
    /// <remarks>This is not a stable sort, i.e. the original order of equal elements is not necessarily preserved. 
    /// For a stable sort, consider using <see cref="M:Microsoft.FSharp.Collections.SeqModule.Sort"/>.</remarks>
    /// <param name="comparer">The function to compare pairs of Rarr elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The sorted Rarr.</returns>
    
    let sortWith (comparer: 'T -> 'T -> int) (rarr: Rarr<'T>) =
        let result = copy rarr
        sortInPlaceWith comparer result
        result
        
                
    /// <summary>Splits an Rarr into two Rarrs, at the given index.</summary>
    /// <param name="index">The index at which the Rarr is split.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The two split Rarrs.</returns>
    
    /// <exception cref="T:System.InvalidOperationException">Thrown when split index exceeds the number of elements
    /// in the Rarr.</exception>
    let splitAt index (rarr: Rarr<'T>) =
        if index < 0 then invalidArgInputMustBeNonNegative "index" index
        if rarr.Count < index then raise <| InvalidOperationException (SR.GetString(SR.notEnoughElements))
        if index = 0 then
            let right = Microsoft.FSharp.Primitives.Basics.Rarr.subUnchecked 0 rarr.Count rarr
            [||], right
        elif index = rarr.Count then
            let left = Microsoft.FSharp.Primitives.Basics.Rarr.subUnchecked 0 rarr.Count rarr
            left, [||] 
        else
            let res1 = Microsoft.FSharp.Primitives.Basics.Rarr.subUnchecked 0 index rarr
            let res2 = Microsoft.FSharp.Primitives.Basics.Rarr.subUnchecked index (rarr.Count-index) rarr
        
            res1, res2
        
                
    /// <summary>Splits the input Rarr into at most <c>count</c> chunks.</summary>
    /// <param name="count">The maximum number of chunks.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The Rarr split into chunks.</returns>
    
    /// <exception cref="T:System.ArgumentException">Thrown when <c>count</c> is not positive.</exception>
    let splitInto count (rarr: _ Rarr) =
        if count <= 0 then invalidArgInputMustBePositive "count" count
        Microsoft.FSharp.Primitives.Basics.Rarr.splitInto count rarr
        
                
    /// <summary>Returns the sum of the elements in the Rarr.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The resulting sum.</returns>
    
    let inline sum (rarr: ^T Rarr ) : ^T = 
        let mutable acc = LanguagePrimitives.GenericZero< ^T>
        for i = 0 to rarr.Count - 1 do
            acc <- Checked.(+) acc rarr.[i]
        acc
        
                
    /// <summary>Returns the sum of the results generated by applying the function to each element of the Rarr.</summary>
    /// <param name="projection">The function to transform the Rarr elements into the type to be summed.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The resulting sum.</returns>
    
    let inline sumBy (projection: 'T -> ^U) (rarr: Rarr<'T>) : ^U = 
        let mutable acc = LanguagePrimitives.GenericZero< ^U>
        for i = 0 to rarr.Count - 1 do
            acc <- Checked.(+) acc (projection rarr.[i])
        acc
        
                
    /// <summary>Returns a new Rarr containing the elements of the original except the first element.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <exception cref="T:System.ArgumentException">Thrown when the Rarr is empty.</exception>
    
    /// <returns>A new Rarr containing the elements of the original except the first element.</returns>
    let tail (rarr: Rarr<'T>) =
        if rarr.Count = 0 then invalidArg "rarr" (SR.GetString(SR.notEnoughElements))            
        Microsoft.FSharp.Primitives.Basics.Rarr.subUnchecked 1 (rarr.Count - 1) rarr
        
                
    /// <summary>Returns the first N elements of the Rarr.</summary>
    /// <remarks>Throws <c>InvalidOperationException</c>
    /// if the count exceeds the number of elements in the Rarr. <c>Array.truncate</c>
    /// returns as many items as the Rarr contains instead of throwing an exception.</remarks>
    /// <param name="count">The number of items to take.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The result Rarr.</returns>
    
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarr is empty.</exception>
    /// <exception cref="T:System.InvalidOperationException">Thrown when count exceeds the number of elements
    /// in the list.</exception>
    let take count (rarr: Rarr<'T>) =
        if count < 0 then  ArgumentOutOfRangeException.Raise "Rarr.XXXX: count %d cannot be negative "count
        if count = 0 then 
            empty
        else
            if count > rarr.Count then
                raise <| InvalidOperationException (SR.GetString(SR.notEnoughElements))
        
            Microsoft.FSharp.Primitives.Basics.Rarr.subUnchecked 0 count rarr
        
                
    /// <summary>Returns an Rarr that contains all elements of the original Rarr while the 
    /// given predicate returns True, and then returns no further elements.</summary>
    /// <param name="predicate">A function that evaluates to false when no more items should be returned.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The result Rarr.</returns>
    
    let takeWhile predicate (rarr: Rarr<'T>) = 
        if rarr.Count = 0 then 
            empty 
        else
            let mutable count = 0
            while count < rarr.Count && predicate rarr.[count] do
                count <- count + 1
        
            Microsoft.FSharp.Primitives.Basics.Rarr.subUnchecked 0 count rarr
        
 
                
    /// <summary>Builds a list from the given Rarr.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The list of Rarr elements.</returns>    
    let toList rarr = 
        List.ofRarr rarr
        
                
    /// <summary>Views the given Rarr as a sequence.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The sequence of Rarr elements.</returns>    
    let toSeq rarr = 
        Seq.ofRarr rarr
        
                
    /// <summary>Returns the transpose of the given sequence of Rarrs.</summary>
    /// <param name="rarrs">The input sequence of Rarrs.</param>
    /// <returns>The transposed Rarr.</returns>    
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>
    let transpose (rarrs: seq<Rarr<'T>>) =
        match rarrs with
        | :? (Rarr<'T> Rarr) as ts -> ts |> transposeRarrs // avoid a clone, since we only read the rarr
        | _ -> rarrs |> Seq.toRarr |> transposeRarrs
        
                
    /// <summary>Returns at most N elements in a new Rarr.</summary>
    /// <param name="count">The maximum number of items to return.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The result Rarr.</returns>
    
    let truncate count (rarr: Rarr<'T>) =
        if count <= 0 then empty
        else
            let len = rarr.Count
            let count' = Operators.min count len
            Microsoft.FSharp.Primitives.Basics.Rarr.subUnchecked 0 count' rarr
        

                    
                    
    /// <summary>Returns the only element of the Rarr or <c>None</c> if Rarr is empty or contains more than one element.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The only element of the Rarr or None.</returns>
    
    let tryExactlyOne (rarr: Rarr<'T>) =
        if rarr.Count = 1 then Some rarr.[0]
        else None
        
    let transposeRarrs (rarr: Rarr<'T> Rarr) =
        let len = rarr.Count
        if len = 0 then Rarr.create 0 else Unchecked.defaultof<_>
        let lenInner = rarr.[0].Count
        
        for j in 1..len-1 do
            if lenInner <> rarr.[j].Count then
                invalidArgDifferentRarrLength "rarr.[0]" lenInner (String.Format("rarr.[{0}]", j)) rarr.[j].Count
        
        let result: Rarr<'T> Rarr = Rarr.create lenInner Unchecked.defaultof<_>
        for i in 0..lenInner-1 do
            result.[i] <- Rarr.create len Unchecked.defaultof<_>
            for j in 0..len-1 do
                result.[i].[j] <- rarr.[j].[i]
        result
        
                
    /// <summary>Returns the first element for which the given function returns True.
    /// Return None if no such element exists.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The first element that satisfies the predicate, or None.</returns>
    
    let tryFind predicate (rarr: _ Rarr) = 
        let rec loop i = 
            if i >= rarr.Count then None else 
            if predicate rarr.[i] then Some rarr.[i]  else loop (i+1)
        loop 0 
        
                
    /// <summary>Returns the last element for which the given function returns True.
    /// Return None if no such element exists.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    
    /// <returns>The last element that satisfies the predicate, or None.</returns>
    let tryFindBack predicate (rarr: _ Rarr) =
        Microsoft.FSharp.Primitives.Basics.Rarr.tryFindBack predicate rarr
        
                
    /// <summary>Returns the index of the first element in the Rarr
    /// that satisfies the given predicate.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    
    /// <returns>The index of the first element that satisfies the predicate, or None.</returns>
    let tryFindIndex predicate (rarr: _ Rarr) = 
        let len = rarr.Count 
        let rec go n = if n >= len then None elif predicate rarr.[n] then Some n else go (n+1)
        go 0 
        
                
    /// <summary>Returns the index of the last element in the Rarr
    /// that satisfies the given predicate.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    
    /// <returns>The index of the last element that satisfies the predicate, or None.</returns>
    let tryFindIndexBack predicate (rarr: _ Rarr) =
        Microsoft.FSharp.Primitives.Basics.Rarr.tryFindIndexBack predicate rarr
        
                
    /// <summary>Returns the first element of the Rarr, or
    /// <c>None</c> if the Rarr is empty.</summary>
    /// <param name="rarr">The input Rarr.</param>
    
    /// <returns>The first element of the Rarr or None.</returns>
    let tryHead (rarr: Rarr<'T>) =
        if rarr.Count = 0 then None
        else Some rarr.[0]
        
                
    /// <summary>Tries to find the nth element in the Rarr.
    /// Returns <c>None</c> if index is negative or the input Rarr does not contain enough elements.</summary>
    /// <param name="index">The index of element to retrieve.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The nth element of the Rarr or <c>None</c>.</returns>
    
    let tryItem index (rarr: Rarr<'T>) =
        if index < 0 || index >= rarr.Count then None
        else Some(rarr.[index])
        
                
    /// <summary>Returns the last element of the Rarr.
    /// Return <c>None</c> if no such element exists.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The last element of the Rarr or None.</returns>
    
    let tryLast (rarr: Rarr<'T>) =
        if rarr.Count = 0 then None 
        else Some rarr.[rarr.Count-1]
        
                
    /// <summary>Applies the given function to successive elements, returning the first
    /// result where function returns <c>Some(x)</c> for some <c>x</c>. If the function 
    /// never returns <c>Some(x)</c> then <c>None</c> is returned.</summary>
    /// <param name="chooser">The function to transform the Rarr elements into options.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The first transformed element that is <c>Some(x)</c>.</returns>
    
    let tryPick chooser (rarr: _ Rarr) = 
        let rec loop i = 
            if i >= rarr.Count then None else 
            match chooser rarr.[i] with 
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
        let res = ResizeRarr<_>()
        let rec loop state =
            match generator state with
            | None -> ()
            | Some (x, s') ->
                res.Add(x)
                loop s'
        loop state
        res.ToRarr()
        
                
    /// <summary>Splits an Rarr of pairs into two Rarrs.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The two Rarrs.</returns>
    
    let unzip (rarr: _ Rarr) = 
        let len = rarr.Count 
        let res1 = Rarr.create len  Unchecked.defaultof<_>
        let res2 = Rarr.create len  Unchecked.defaultof<_>
        for i = 0 to rarr.Count-1 do 
            let x, y = rarr.[i] 
            res1.[i] <- x
            res2.[i] <- y
        res1, res2
        
                
    /// <summary>Splits an Rarr of triples into three Rarrs.</summary>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The tuple of three Rarrs.</returns>
    
    let unzip3 (rarr: _ Rarr) = 
        let len = rarr.Count 
        let res1 = Rarr.create len  Unchecked.defaultof<_>
        let res2 = Rarr.create len  Unchecked.defaultof<_>
        let res3 = Rarr.create len  Unchecked.defaultof<_>
        for i = 0 to rarr.Count-1 do 
            let x, y, z = rarr.[i] 
            res1.[i] <- x
            res2.[i] <- y
            res3.[i] <- z
        res1, res2, res3
        
                
    /// <summary>Returns a new Rarr containing only the elements of the Rarr
    /// for which the given predicate returns "true".</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>An Rarr containing the elements for which the given predicate returns true.</returns>
    
    let where predicate (rarr: _ Rarr) = filter predicate rarr
        
                
    /// <summary>Returns an Rarr of sliding windows containing elements drawn from the input
    /// Rarr. Each window is returned as a fresh Rarr.</summary>
    /// <param name="windowSize">The number of elements in each window.</param>
    /// <param name="rarr">The input Rarr.</param>
    /// <returns>The result Rarr.</returns>
    
    /// <exception cref="T:System.ArgumentException">Thrown when windowSize is not positive.</exception>
    let windowed windowSize (rarr: Rarr<'T>) =
        if windowSize <= 0 then invalidArgInputMustBePositive "windowSize" windowSize
        let len = rarr.Count
        if windowSize > len then
            empty
        else
            let res: Rarr<'T> Rarr = Rarr.create (len - windowSize + 1)  Unchecked.defaultof<_>
            for i = 0 to len - windowSize do
                res.[i] <- Microsoft.FSharp.Primitives.Basics.Rarr.subUnchecked i windowSize rarr
            res
        
    module Paralell = 
               
        open System.Threading.Tasks

        /// <summary>Apply the given function to each element of the Rarr. Return
        /// the Rarr comprised of the results "x" for each element where
        /// the function returns Some(x).</summary>
        /// <remarks>Performs the operation in parallel using <see cref="M:System.Threading.Tasks.Parallel.For" />.
        /// The order in which the given function is applied to elements of the input Rarr is not specified.</remarks>
        /// <param name="chooser">The function to generate options from the elements.</param>
        /// <param name="rarr">The input Rarr.</param>
        /// <returns>The Rarr of results.</returns>
                    
        let choose chooser (rarr: Rarr<'T>) = 
            let inputLength = rarr.Count
        
            let isChosen: bool  Rarr = Rarr.create inputLength Unchecked.defaultof<_>
            let results: 'U  Rarr = Rarr.create inputLength                 Unchecked.defaultof<_>
            let mutable outputLength = 0        
            Parallel.For(0, 
                            inputLength, 
                            (fun () ->0),
                            (fun i _ count -> 
                            match chooser rarr.[i] with 
                            | None -> count 
                            | Some v -> 
                                isChosen.[i] <- true; 
                                results.[i] <- v
                                count+1),
                            Action<int> (fun x -> System.Threading.Interlocked.Add(&outputLength, x) |> ignore )
                            ) |> ignore
        
            let output = Rarr.create outputLength Unchecked.defaultof<_>
            let mutable curr = 0
            for i = 0 to isChosen.Count-1 do 
                if isChosen.[i] then 
                    output.[curr] <- results.[i]
                    curr <- curr + 1
            output
                        
                    
        /// <summary>For each element of the Rarr, apply the given function. Concatenate all the results and return the combined Rarr.</summary>
        /// <remarks>Performs the operation in parallel using <see cref="M:System.Threading.Tasks.Parallel.For" />.
        /// The order in which the given function is applied to elements of the input Rarr is not specified.</remarks>
        /// <param name="mapping"></param>
        /// <param name="rarr">The input Rarr.</param>
        /// <returns>'U[]</returns>
                    
        let collect (mapping: 'T -> Rarr<'U>)  (rarr: Rarr<'T>) : Rarr<'U>=
            let inputLength = rarr.Count
            let result = Rarr.create inputLength Unchecked.defaultof<_>
            Parallel.For(0, inputLength, 
                (fun i -> result.[i] <- mapping rarr.[i])) |> ignore
            concatRarrs result
                        
                    
        /// <summary>Create an Rarr given the dimension and a generator function to compute the elements.</summary>
        /// <remarks>Performs the operation in parallel using <see cref="M:System.Threading.Tasks.Parallel.For" />.
        /// The order in which the given function is applied to indices is not specified.</remarks>
        /// <param name="count"></param>
        /// <param name="initializer"></param>
        /// <returns>The Rarr of results.</returns>
        let init count initializer =
            let result = Rarr.create count Unchecked.defaultof<_>
            Parallel.For (0, count, fun i -> result.[i] <- initializer i) |> ignore
            result
                        
                    
        /// <summary>Apply the given function to each element of the Rarr. </summary>
        /// <remarks>Performs the operation in parallel using <see cref="M:System.Threading.Tasks.Parallel.For" />.
        /// The order in which the given function is applied to elements of the input Rarr is not specified.</remarks>
        /// <param name="action"></param>
        /// <param name="rarr">The input Rarr.</param>
                    
        let iter action (rarr: Rarr<'T>) =
            Parallel.For (0, rarr.Count, fun i -> action rarr.[i]) |> ignore  
                        
                    
        /// <summary>Apply the given function to each element of the Rarr. The integer passed to the
        /// function indicates the index of element.</summary>
        /// <remarks>Performs the operation in parallel using <see cref="M:System.Threading.Tasks.Parallel.For" />.
        /// The order in which the given function is applied to elements of the input Rarr is not specified.</remarks>
        /// <param name="action"></param>
        /// <param name="rarr">The input Rarr.</param>
                    
        let iteri action (rarr: Rarr<'T>) =
            let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(action)
            Parallel.For (0, rarr.Count, fun i -> f.Invoke(i, rarr.[i])) |> ignore        
                        
                    
        /// <summary>Build a new Rarr whose elements are the results of applying the given function
        /// to each of the elements of the Rarr.</summary>
        /// <remarks>Performs the operation in parallel using <see cref="M:System.Threading.Tasks.Parallel.For" />.
        /// The order in which the given function is applied to elements of the input Rarr is not specified.</remarks>
        /// <param name="mapping"></param>
        /// <param name="rarr">The input Rarr.</param>
        /// <returns>The Rarr of results.</returns>
                    
        let map (mapping: 'T -> 'U) (rarr: Rarr<'T>) : Rarr<'U>=
            let inputLength = rarr.Count
            let result = Rarr.create inputLength Unchecked.defaultof<_>
            Parallel.For(0, inputLength, fun i ->
                result.[i] <- mapping rarr.[i]) |> ignore
            result
                        
                    
        /// <summary>Build a new Rarr whose elements are the results of applying the given function
        /// to each of the elements of the Rarr. The integer index passed to the
        /// function indicates the index of element being transformed.</summary>
        /// <remarks>Performs the operation in parallel using <see cref="M:System.Threading.Tasks.Parallel.For" />.
        /// The order in which the given function is applied to elements of the input Rarr is not specified.</remarks>
        /// <param name="mapping"></param>
        /// <param name="rarr">The input Rarr.</param>
        /// <returns>The Rarr of results.</returns>
                    
        let mapi mapping (rarr: Rarr<'T>) =
            let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(mapping)
            let inputLength = rarr.Count
            let result = Rarr.create inputLength  Unchecked.defaultof<_>
            Parallel.For(0, inputLength, fun i ->
                result.[i] <- f.Invoke (i, rarr.[i])) |> ignore
            result
                        
                    
        /// <summary>Split the collection into two collections, containing the 
        /// elements for which the given predicate returns "true" and "false"
        /// respectively </summary>
        /// <remarks>Performs the operation in parallel using <see cref="M:System.Threading.Tasks.Parallel.For" />.
        /// The order in which the given function is applied to indices is not specified.</remarks>
        /// <param name="predicate">The function to test the input elements.</param>
        /// <param name="rarr">The input Rarr.</param>
        /// <returns>The two Rarrs of results.</returns>
                    
        let partition predicate (rarr: Rarr<'T>) =
            let inputLength = rarr.Count
                       
            let isTrue = Rarr.create inputLength Unchecked.defaultof<_>
            let mutable trueLength = 0
            Parallel.For(0, 
                            inputLength, 
                            (fun () -> 0), 
                            (fun i _ trueCount -> 
                            if predicate rarr.[i] then
                                isTrue.[i] <- true
                                trueCount + 1
                            else
                                trueCount),
                            Action<int> (fun x -> System.Threading.Interlocked.Add(&trueLength, x) |> ignore) ) |> ignore
                                        
            let res1 = Rarr.create trueLength Unchecked.defaultof<_>
            let res2 = Rarr.create (inputLength - trueLength) Unchecked.defaultof<_>
        
            let mutable iTrue = 0
            let mutable iFalse = 0
            for i = 0 to isTrue.Count-1 do
                if isTrue.[i] then
                    res1.[iTrue] <- rarr.[i]
                    iTrue <- iTrue + 1
                else
                    res2.[iFalse] <- rarr.[i]
                    iFalse <- iFalse + 1
        
            res1, res2
        
    /// <summary>Creates an Rarr where the entries are initially the default value Unchecked.defaultof&lt;'T&gt;.</summary>
    /// <param name="count">The length of the Rarr to create.</param>
    /// <returns>The created Rarr.</returns>
    /// <exception cref="T:System.ArgumentOutOfRangeException">Thrown when count is negative.</exception>
    let zeroCreate count = 
        if count < 0 then  ArgumentOutOfRangeException.Raise "Rarr.XXXX: count %d cannot be negative "count
        Rarr.create count Unchecked.defaultof<_>
        
                
    /// <summary>Combines the two Rarrs into an Rarr of pairs. The two Rarrs must have equal lengths, otherwise an <c>ArgumentException</c> is
    /// raised.</summary>
    /// <param name="rarr1">The first input Rarr.</param>
    /// <param name="rarr2">The second input Rarr.</param>
   
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>
    /// <returns>The Rarr of tupled elements.</returns>
    let zip (rarr1: _ Rarr) (rarr2: _ Rarr) = 
        let len1 = rarr1.Count 
        if len1 <> rarr2.Count then invalidArgDifferentRarrLength "rarr1" rarr1.Count "rarr2" rarr2.Count
        let res = Rarr.create len1  Unchecked.defaultof<_>
        for i = 0 to res.Count-1 do 
            res.[i] <- (rarr1.[i], rarr2.[i])
        res
        
                
    /// <summary>Combines three Rarrs into an Rarr of pairs. The three Rarrs must have equal lengths, otherwise an <c>ArgumentException</c> is
    /// raised.</summary>
    /// <param name="rarr1">The first input Rarr.</param>
    /// <param name="rarr2">The second input Rarr.</param>
    /// <param name="rarr3">The third input Rarr.</param>
    DELTE LINEThrown when any of the input Rarrs are null.</exception>
    /// <exception cref="T:System.ArgumentException">Thrown when the input Rarrs differ in length.</exception>
    /// <returns>The Rarr of tupled elements.</returns>
    let zip3 (rarr1: _ Rarr) (rarr2: _ Rarr) (rarr3: _ Rarr) = 
        let len1 = rarr1.Count
        if len1 <> rarr2.Count || len1 <> rarr3.Count then invalidArg3RarrsDifferent "rarr1" "rarr2" "rarr3" len1 rarr2.Count rarr3.Count
        let res = Rarr.create len1  Unchecked.defaultof<_>
        for i = 0 to res.Count-1 do 
            res.[i] <- (rarr1.[i], rarr2.[i], rarr3.[i])
        res
        
                
        