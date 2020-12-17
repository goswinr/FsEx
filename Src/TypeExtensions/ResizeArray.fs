namespace FsEx
#nowarn "0044" // resize array is deprecated use Rarr instead
open System


//[<AutoOpen>]
[<Obsolete>]
module ResizeArrayTypeExtensions =   
    
    open Microsoft.FSharp.Core
    open System.Runtime.CompilerServices

    //[<Extension;Obsolete>] //Error 3246
    type Collections.Generic.List<'T>  with 
    
        /// Gets the index of the last item in the ResizeArray.
        /// equal to this.Count - 1    
        [<Extension;Obsolete>]
        member inline this.LastIndex = 
            if this.Count = 0 then IndexOutOfRangeException.Raise "resizeArray.LastIndex: Can not get LastIndex of empty List"
            this.Count - 1

        /// Gets the last item in the ResizeArray.
        /// equal to this.[this.Count - 1]
        [<Extension;Obsolete>]
        member inline this.Last = 
            if this.Count = 0 then IndexOutOfRangeException.Raise "resizeArray.Last: Can not get Last item of empty List"
            this.[this.Count - 1]
        
        /// Gets the second last item in the ResizeArray.
        /// equal to this.[this.Count - 2]
        [<Extension;Obsolete>]
        member inline this.SecondLast = 
            if this.Count < 2 then 
                IndexOutOfRangeException.Raise "resizeArray.SecondLast: Can not get SecondLast item of %s"   (NiceString.toNiceStringFull this)
            this.[this.Count - 2]

        /// Gets the third last item in the ResizeArray.
        /// equal to this.[this.Count - 3]
        [<Extension;Obsolete>]
        member inline this.ThirdLast = 
            if this.Count < 3 then 
                IndexOutOfRangeException.Raise "resizeArray.ThirdLast: Can not get ThirdLast item of %s"  (NiceString.toNiceStringFull this)
            this.[this.Count - 3]
                    
        /// Gets the first item in the ResizeArray.
        /// equal to this.[0]
        [<Extension;Obsolete>]
        member inline this.First = 
            if this.Count = 0 then IndexOutOfRangeException.Raise "resizeArray.First: Can not get First item of empty List"
            this.[0]

        /// Gets the second item in the ResizeArray.
        /// equal to this.[1]
        [<Extension;Obsolete>]
        member inline this.Second = 
            if this.Count < 2 then 
                IndexOutOfRangeException.Raise  "resizeArray.Second: Can not get Second item of %s"   (NiceString.toNiceStringFull this)
            this.[1]

        /// Gets the third item in the ResizeArray.
        /// equal to this.[2]
        [<Extension;Obsolete>]
        member inline this.Third = 
            if this.Count < 3 then 
                IndexOutOfRangeException.Raise "resizeArray.Third: Can not get Third item of %s"  (NiceString.toNiceStringFull this)
            this.[2]

        /// Checks if this.Count = 0 
        [<Extension;Obsolete>]
        member inline this.IsEmpty = 
            this.Count = 0 
        
        /// Checks if this.Count > 0 
        [<Extension;Obsolete>]
        member inline this.IsNotEmpty = 
            this.Count > 0 
        
        /// Insert an item at the beginning of the list = index 0, 
        /// (moving all other items up by one index)
        [<Extension;Obsolete>] 
        member inline this.Insert0 x  = 
            this.Insert(0,x)

        /// Gets an item in the ResizeArray by index.
        /// Allows for negtive index too ( -1 is last item,  like Python)
        /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
        [<Extension;Obsolete>] 
        member this.GetItem index = 
            let i = negIdx index this.Count
            this.[i]
        
        
        /// Sets an item in the ResizeArray by index.
        /// Allows for negtive index too ( -1 is last item, like Python)
        /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
        [<Extension;Obsolete>] 
        member this.SetItem index value = 
            let i = negIdx index this.Count
            this.[i] <- value 
        
        
        /// Gets and remove last item from ResizeArray
        [<Extension;Obsolete>] 
        member  this.Pop()  =
            let i = this.Count - 1
            let v = this.[i]
            this.RemoveAt(i)
            v

        
        /// Defines F# slicing notation operator use including negative indices. ( -1 is last item, like Python)
        /// The resulting ResizeArray includes the end index.
        /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
        [<Extension;Obsolete>]
        member this.GetSlice(startIdx, endIdx) =    // maps onto slicing operator .[1..3]
            let count = this.Count
            let st  = match startIdx with None -> 0        | Some i -> if i<0 then count+i      else i
            let len = match endIdx   with None -> count-st | Some i -> if i<0 then count+i-st+1 else i-st+1
    
            if st < 0 || st > count-1 then 
                IndexOutOfRangeException.Raise "resizeArray.GetSlice: Start index %d is out of range. Allowed values are -%d upto %d for List of %d items" startIdx.Value count (count-1) count
                
    
            if st+len > count then 
                IndexOutOfRangeException.Raise "resizeArray.GetSlice: End index %d is out of range. Allowed values are -%d upto %d for List of %d items" endIdx.Value count (count-1) count
                
                
            if len < 0 then
                let en =  match endIdx  with None -> count-1 | Some i -> if i<0 then count+i else i
                IndexOutOfRangeException.Raise "resizeArray.GetSlice: Start index '%A' (= %d) is bigger than end index '%A'(= %d) for List of %d items" startIdx st endIdx en  count
                
                
            this.GetRange(st, len) 
         
         
         /// Allows for negative indices too. ( -1 is last item, like Python)
         /// This method only exist to be consisten with other collections extensions in FsEx. like array. 
         /// You might prefer to use the F# slicing notation on ResizeArrays.
         /// For ResizeArrays this behaves the same way as the F# slicing notation defind in FsEx too, 
         /// (Only arrays need to use this method if they want to use negative indices since the GetSlice operators cant be overwritten.) 
         /// The resulting array includes the end index.
         /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
         [<Extension;Obsolete>]
         member this.Slice(startIdx, endIdx) = this.GetSlice(startIdx, endIdx)    
        
        /// A property like the ToString() method, 
        /// But with richer formationg for collections
        [<Extension;Obsolete>]  
        member this.ToNiceString = NiceString.toNiceString this


/// Generic operations on the type System.Collections.Generic.List, which is called ResizeArray in the F# libraries.
[<Obsolete>]
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] //need this so doesn't hide ResizeArray class in C# assemblies (should consider for other extension modules as well)
module ResizeArray =    


    /// Fetch an element from the collection.  You can also use the syntax <c>arr.[idx]</c>.
    /// However this function allows for negtive index too (like Python)
    [<Obsolete>]
    let get (arr: ResizeArray<'T>) (index: int) =  
        if index<0 then arr.[arr.Count+index]  else arr.[index]

    /// Set the value of an element in the collection. You can also use the syntax <c>arr.[idx] <- e</c>.
    /// However this function allows for negtive index too (like Python)
    [<Obsolete>] 
    let set (arr: ResizeArray<'T>) (index: int) (x:'T) =  
        if index<0 then arr.[arr.Count+index] <- x  else arr.[index] <- x

        
    //---------------------------------------------------
    // extensions added only in FsEx:
    //----------------------------------------------------
   
    [<Obsolete>] 
    let empty() = ResizeArray<_>()

    /// Create a ResizeArray by calling the given generator on each index.
    //-[<CompiledName("Init")>]
    [<Obsolete>]
    let init count initializer : ResizeArray<'T> =
        if count < 0 then invalidArg "count" "The number of elements may not be negative."
        let resizeArray = ResizeArray (count)
        for i = 0 to count - 1 do resizeArray.Add ( initializer count)
        resizeArray

    /// Considers List cirular and move elements up or down
    /// e.g.: rotate +1 [ a, b, c, d] = [ d, a, b, c]
    /// e.g.: rotate -1 [ a, b, c, d] = [ b, c, d, a]
    [<Obsolete>] 
    let rotate k (xs: ResizeArray<_>)  =  init xs.Count (fun i -> xs.[negIdxLooped (i-k) xs.Count])


    /// Returns a ResizeArray of the index and the item. (like enumerate in Python)
    [<Obsolete>] 
    let indexed (xs: ResizeArray<_>)  =  init xs.Count (fun i -> i,xs.[i])


    /// internal only for finding 
    module private MinMax =
        let inline simple cmpF (xs:ResizeArray<'T>) =
            if xs.Count < 1 then failwithf "*** Empty %A in ResizeArray max / min" xs
            let mutable m = xs.[0]
            for i=1 to xs.Count-1 do
                if cmpF xs.[i] m then m <- xs.[i]
            m 
            
        let inline simple2 cmpF (xs:ResizeArray<'T>) =
            if xs.Count < 2 then failwithf "*** Only %d elements in %A, for ResizeArray first+second max / min" xs.Count xs
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

        let inline simple3 cmpF (xs:ResizeArray<'T>) =
            if xs.Count < 3 then failwithf "*** Only %d elements in %A, for ResizeArray first+second+third max / min" xs.Count xs
            let e1 = xs.[0]
            let e2 = xs.[1]
            let e3 = xs.[2]
            // sort first 3
            let mutable m1, m2, m3 =  sort3 cmpF e1 e2 e3   // otherwise would fail on sorting first 3, test on ResizeArray([5;6;3;1;2;0])|> ResizeArray.max3 
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
                                    
        let inline indexByFun cmpF func (xs:ResizeArray<'T>) = 
            if xs.Count < 1 then failwithf "*** Empty %A, ResizeArray  max / min IndexByFun" xs
            let mutable f = func xs.[0]
            let mutable mf = f
            let mutable ii = 0
            for i=1 to xs.Count-1 do
                f <- func xs.[i] 
                if cmpF f mf then 
                    ii <- i
                    mf <- f
            ii

        let inline index2ByFun cmpF func (xs:ResizeArray<'T>) =
            if xs.Count < 2 then failwithf "*** Only %d elements in %A, for ResizeArray index2ByFun max / min" xs.Count xs            
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


        let inline index3ByFun (cmpOp:'U->'U->bool)  (byFun:'T->'U) (xs:ResizeArray<'T>) =
            if xs.Count < 3 then failwithf "*** Only %d elements in %A, for ResizeArray index3ByFun max / min" xs.Count xs 
            // sort first 3
            let mutable i1,i2,i3 =  indexOfSort3By byFun cmpOp xs.[0] xs.[1] xs.[2] // otherwise would fail on sorting first 3, test on ResizeArray([5;6;3;1;2;0])|> ResizeArray.max3 
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
        
    //TODO test keeping of order if equal !

    (*
    /// Returns the smallest element of the ResizeArray.
    [<Obsolete>] 
    let min xs =     xs |> MinMax.simple (<)  // why inline? type specialisation ?

    /// Returns the biggest element of the ResizeArray.
    [<Obsolete>] 
    let max xs =     xs |> MinMax.simple (>)

    /// Returns the smallest element of the ResizeArray.
    /// Elements are compared by applying the predicate function first.
    [<Obsolete>] 
    let minBy f xs = let i = xs |> MinMax.indexByFun (<) f in xs.[i]

    /// Returns the biggest element of the ResizeArray.
    /// Elements are compared by applying the predicate function first.
    [<Obsolete>] 
    let maxBy f xs = let i = xs |> MinMax.indexByFun (>) f in xs.[i]
    *)

    /// Returns the Index of the smallest element of the ResizeArray.
    /// Elements are compared by applying the predicate function first.
    [<Obsolete>] 
    let minIndBy f xs = xs |> MinMax.indexByFun (<) f

    /// Returns the Index of the biggest element of the ResizeArray.
    /// Elements are compared by applying the predicate function first.
    [<Obsolete>] 
    let maxIndBy f xs = xs |> MinMax.indexByFun (>) f

    /// Returns the smallest two elements of the ResizeArray.
    /// If they are equal then the the order is kept
    [<Obsolete>] 
    let min2 xs =     xs |> MinMax.simple2 (<)

    /// Returns the biggest two elements of the ResizeArray.
    /// If they are equal then the the order is kept
    [<Obsolete>] 
    let max2 xs =     xs |> MinMax.simple2 (>)
        
    /// Returns the smallest two elements of the ResizeArray.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    [<Obsolete>] 
    let min2By f xs = let i,ii = xs |> MinMax.index2ByFun (<) f in xs.[i],xs.[ii]
        
    /// Returns the biggest two elements of the ResizeArray.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    [<Obsolete>] 
    let max2By f xs = let i,ii = xs |> MinMax.index2ByFun (>) f in xs.[i],xs.[ii]

    /// Returns the indices of the two smallest elements of the ResizeArray.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    [<Obsolete>] 
    let min2IndBy f xs = xs |> MinMax.index2ByFun (<) f

    /// Returns the indices of the two biggest elements of the ResizeArray.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    [<Obsolete>] 
    let max2IndBy f xs = xs |> MinMax.index2ByFun (>) f

    /// Returns the smallest three elements of the ResizeArray.
    /// If they are equal then the the order is kept
    [<Obsolete>] 
    let min3 xs =  xs |> MinMax.simple3 (<)

    /// Returns the biggest three elements of the ResizeArray.
    /// If they are equal then the the order is kept
    [<Obsolete>] 
    let max3 xs =  xs |> MinMax.simple3 (>)

    /// Returns the smallest three elements of the ResizeArray.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    [<Obsolete>] 
    let min3By f xs = let i,ii,iii = xs |> MinMax.index3ByFun (<) f in xs.[i],xs.[ii],xs.[iii]

    /// Returns the biggest three elements of the ResizeArray.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    [<Obsolete>] 
    let max3By f xs = let i,ii,iii = xs |> MinMax.index3ByFun (>) f in xs.[i],xs.[ii],xs.[iii]

    /// Returns the indices of the three smallest elements of the ResizeArray.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    [<Obsolete>] 
    let min3IndBy f xs = xs |> MinMax.index3ByFun(<) f

    /// Returns the indices of the three biggest elements of the ResizeArray.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    [<Obsolete>] 
    let max3IndBy f xs = xs |> MinMax.index3ByFun (>) f

    [<Obsolete>] 
    let inline checkNonNull tx xs = 
        match xs with null -> raise (ArgumentNullException("in module FsEx.ResizeArray: " + tx)) |_ -> ()

    // taken and extended from https://github.com/jack-pappas/ExtCore/blob/master/ExtCore/Collections.ResizeArray.fs 
    // previously https://github.com/dotnet/fsharp/tree/master/src/utils   
    
    open Microsoft.FSharp.Core
    open Microsoft.FSharp.Core.OptimizedClosures
    open System.Collections.Generic
    open LanguagePrimitives

    /// Return the length of the collection.
    //-[<CompiledName("Length")>]
    [<Obsolete>] 
    let inline length (resizeArray : ResizeArray<'T>) : int =
        resizeArray.Count

    /// Return true if the given array is empty, otherwise false.
    //-[<CompiledName("IsEmpty")>]
    [<Obsolete>] 
    let inline isEmpty (resizeArray : ResizeArray<'T>) : bool =
        resizeArray.Count = 0


    /// Create a ResizeArray whose elements are all initially the given value.
    //-[<CompiledName("Create")>]
    [<Obsolete>] 
    let create count value : ResizeArray<'T> =
        if count < 0 then
            invalidArg "count" "The number of elements may not be negative."

        let resizeArray = ResizeArray (count)
        for i = 0 to count - 1 do
            resizeArray.Add value
        resizeArray


    /// Adds an object to the end of the ResizeArray.
    //-[<CompiledName("Add")>]
    [<Obsolete>] 
    let inline add item (resizeArray : ResizeArray<'T>) : unit =
        resizeArray.Add item

    /// Determines whether an element is in the ResizeArray.
    //-[<CompiledName("Contains")>]
    [<Obsolete>] 
    let inline contains (value : 'T) (resizeArray : ResizeArray<'T>) : bool =
        checkNonNull "resizeArray" resizeArray

        resizeArray.Contains value

    /// Build a ResizeArray from the given sequence.
    //-[<CompiledName("OfSeq")>]
    [<Obsolete>] 
    let inline ofSeq (sequence : seq<'T>) : ResizeArray<'T> =
        ResizeArray (sequence)

    /// Build a ResizeArray from the given list.
    //-[<CompiledName("OfList")>]
    [<Obsolete>] 
    let ofList (list : 'T list) : ResizeArray<'T> =
        //checkNonNull "list" list

        //let len = list.Length //edit by Goswin, could be costly
        let res = ResizeArray<_>() //(len) 
        let rec add = function
            | [] -> ()
            | e::l -> res.Add(e); add l
        add list
        res

    /// Build a ResizeArray from the given array.
    //-[<CompiledName("OfArray")>]
    [<Obsolete>] 
    let inline ofArray (arr : 'T[]) : ResizeArray<'T> =
        ResizeArray (arr)


    /// Return a view of the ResizeArray as an enumerable object.
    //-[<CompiledName("ToSeq")>]
    [<Obsolete>] 
    let toSeq (resizeArray : ResizeArray<'T>) : seq<'T> =
        checkNonNull "resizeArray" resizeArray

        Seq.readonly resizeArray

    /// Build a list from the given ResizeArray.
    //-[<CompiledName("ToList")>]
    [<Obsolete>] 
    let toList (resizeArray : ResizeArray<'T>) : 'T list =
        checkNonNull "resizeArray" resizeArray

        let mutable res = []
        for i = length resizeArray - 1 downto 0 do
            res <- resizeArray.[i] :: res
        res

    /// Return a fixed-length array containing the elements of the input ResizeArray.
    //-[<CompiledName("ToArray")>]
    [<Obsolete>] 
    let inline toArray (resizeArray : ResizeArray<'T>) : 'T[] =
        resizeArray.ToArray ()

   
    /// Sorts the elements of the ResizeArray by mutating the ResizeArray in-place.
    /// Elements are compared using Operators.compare.
    //-[<CompiledName("SortInPlace")>]
    [<Obsolete>] 
    let inline sortInPlace<'T when 'T : comparison> (resizeArray : ResizeArray<'T>) : unit =
        resizeArray.Sort ()
        
    /// Sort the elements using the key extractor and generic comparison on the keys.
    //-[<CompiledName("SortInPlaceBy")>]
    [<Obsolete>] 
    let inline sortInPlaceBy<'T, 'Key when 'Key : comparison>     (projection : 'T -> 'Key) (resizeArray : ResizeArray<'T>) =
        resizeArray.Sort (fun x y ->
            compare (projection x) (projection y))

    /// Sort the elements using the given comparison function.
    //-[<CompiledName("SortInPlaceWith")>]
    [<Obsolete>] 
    let inline sortInPlaceWith (comparer : 'T -> 'T -> int) (resizeArray : ResizeArray<'T>) : unit =
        resizeArray.Sort (comparer)

    /// Build a new ResizeArray that contains the elements of the given ResizeArray.
    //-[<CompiledName("Copy")>]
    [<Obsolete>] 
    let inline copy (resizeArray : ResizeArray<'T>) : ResizeArray<'T> =
        ResizeArray (resizeArray)

    /// Return an array containing the given element.
    //-[<CompiledName("Singleton")>]
    [<Obsolete>] 
    let singleton value : ResizeArray<'T> =
        let resizeArray = ResizeArray ()
        resizeArray.Add value
        resizeArray

    /// Build a new ResizeArray that contains the elements of each of the given sequence of ResizeArrays.
    //-[<CompiledName("Concat")>]
    [<Obsolete>] 
    let concat (resizeArrays : seq<ResizeArray<'T>>) : ResizeArray<'T> =
        checkNonNull "resizeArrays" resizeArrays

        let flattened = ResizeArray ()
        for resizeArray in resizeArrays do
            flattened.AddRange resizeArray
        flattened
    
    /// Build a new ResizeArray that contains the elements of the first ResizeArray followed by
    /// the elements of the second ResizeArray.
    //-[<CompiledName("Append")>]
    [<Obsolete>] 
    let append (resizeArray1 : ResizeArray<'T>) (resizeArray2 : ResizeArray<'T>) : ResizeArray<'T> =
        checkNonNull "resizeArray1" resizeArray1
        checkNonNull "resizeArray2" resizeArray2

        let combined = ResizeArray (resizeArray1.Count + resizeArray2.Count)
        combined.AddRange resizeArray1
        combined.AddRange resizeArray2
        combined

    /// Build a new ResizeArray that contains the given subrange specified by
    /// starting index and length.
    //-[<CompiledName("Sub")>]
    [<Obsolete>] 
    let sub (resizeArray : ResizeArray<'T>) start count : ResizeArray<'T> =
        checkNonNull "resizeArray" resizeArray
        if start < 0 then
            invalidArg "start" "The start index cannot be less than zero (0)."
        elif count < 0 then
            invalidArg "count" "The number of elements to copy cannot be less than zero (0)."
        elif start + count > length resizeArray then
            invalidArg "count" "There are fewer than 'count' elements between the 'start' index and the end of the collection."
    
        resizeArray.GetRange (start, count)

    /// Fill a range of the collection with the given element.
    //-[<CompiledName("Fill")>]
    [<Obsolete>] 
    let fill (resizeArray : ResizeArray<'T>) start count value : unit =
        checkNonNull "resizeArray" resizeArray
        if start < 0 then
            invalidArg "start" "The start index cannot be less than zero (0)."
        elif count < 0 then
            invalidArg "count" "The number of elements to copy cannot be less than zero (0)."
        elif start + count > length resizeArray then
            invalidArg "count" "There are fewer than 'count' elements between the 'start' index and the end of the collection."
    
        // Overwrite the items within the range using the specified value.
        for i = start to start + count - 1 do
            resizeArray.[i] <- value

    /// Return a new ResizeArray with the elements in reverse order.
    //-[<CompiledName("Rev")>]
    [<Obsolete>] 
    let rev (resizeArray : ResizeArray<'T>) : ResizeArray<'T> =
        checkNonNull "resizeArray" resizeArray

        let len = length resizeArray
        let result = ResizeArray (len)
        for i = len - 1 downto 0 do
            result.Add resizeArray.[i]
        result

    /// Read a range of elements from the first ResizeArray and write them into the second.
    //-[<CompiledName("Blit")>]
    [<Obsolete>] 
    let blit (source : ResizeArray<'T>) sourceIndex (target : ResizeArray<'T>) targetIndex count : unit =
        checkNonNull "source" source
        checkNonNull "target" target
        if sourceIndex < 0 then
            invalidArg "sourceIndex" "The source index cannot be negative."
        elif targetIndex < 0 then
            invalidArg "targetIndex" "The target index cannot be negative."
        elif count < 0 then
            invalidArg "count" "Cannot copy a negative number of items."
        elif sourceIndex + count > length source then
            invalidArg "sourceIndex" "There are fewer than 'count' elements between 'sourceIndex' and the end of the source ResizeArray."
        elif targetIndex + count > length target then
            invalidArg "sourceIndex" "There are fewer than 'count' elements between 'sourceIndex' and the end of the target ResizeArray."

        for i = 0 to count - 1 do
            target.[targetIndex + i] <- source.[sourceIndex + i]

    /// Combine the two ResizeArrays into a ResizeArray of pairs.
    /// The two arrays must have equal lengths, otherwise an <c>ArgumentException</c> is raised.
    //-[<CompiledName("Zip")>]
    [<Obsolete>] 
    let zip (resizeArray1 : ResizeArray<'T1>) (resizeArray2 : ResizeArray<'T2>)
        : ResizeArray<'T1 * 'T2> =
        checkNonNull "resizeArray1" resizeArray1
        checkNonNull "resizeArray2" resizeArray2

        let len = length resizeArray1
        if len <> length resizeArray2 then
            invalidArg "resizeArray2" "The ResizeArrays have different lengths."

        let results = ResizeArray (len)
        for i = 0 to len - 1 do
            results.Add (resizeArray1.[i], resizeArray2.[i])
        results

    /// Split a ResizeArray of pairs into two ResizeArrays.
    //-[<CompiledName("Unzip")>]
    [<Obsolete>] 
    let unzip (resizeArray : ResizeArray<'T1 * 'T2>) : ResizeArray<'T1> * ResizeArray<'T2> =
        checkNonNull "resizeArray" resizeArray

        let len = length resizeArray
        let results1 = ResizeArray (len)
        let results2 = ResizeArray (len)

        for i = 0 to len - 1 do
            let x, y = resizeArray.[i]
            results1.Add x
            results2.Add y

        results1, results2

    /// Test if any element of the array satisfies the given predicate.
    /// If the input function is <c>f</c> and the elements are <c>i0...iN</c> 
    /// then computes <c>p i0 or ... or p iN</c>.
    //-[<CompiledName("Exists")>]
    [<Obsolete>] 
    let inline exists (predicate : 'T -> bool) (resizeArray : ResizeArray<'T>) : bool =
        checkNonNull "resizeArray" resizeArray
        resizeArray.Exists (System.Predicate predicate)
        

    /// Test elements of the two arrays pairwise to see if any pair of element satisfies the given predicate.
    /// Raise ArgumentException if the arrays have different lengths.
    //-[<CompiledName("Exists2")>]
    [<Obsolete>] 
    let exists2 predicate (resizeArray1 : ResizeArray<'T1>) (resizeArray2 : ResizeArray<'T2>) : bool =
        checkNonNull "resizeArray1" resizeArray1
        checkNonNull "resizeArray2" resizeArray2

        let len = length resizeArray1
        if len <> length resizeArray2 then
            invalidArg "resizeArray2" "The ResizeArrays have different lengths."

        let predicate = FSharpFunc<_,_,_>.Adapt predicate
    
        let mutable index = 0
        let mutable foundMatch = false
        while index < len && not foundMatch do
            foundMatch <- predicate.Invoke (resizeArray1.[index], resizeArray2.[index])
            index <- index + 1
        foundMatch

    /// Test if all elements of the array satisfy the given predicate.
    /// If the input function is <c>f</c> and the elements are <c>i0...iN</c> and "j0...jN"
    /// then computes <c>p i0 && ... && p iN</c>.
    //-[<CompiledName("Forall")>]
    [<Obsolete>] 
    let inline forall (predicate : 'T -> bool) (resizeArray : ResizeArray<'T>) : bool =
        checkNonNull "resizeArray" resizeArray
        resizeArray.TrueForAll (System.Predicate predicate)
       

    /// Test elements of the two arrays pairwise to see if all pairs of elements satisfy the given predicate.
    /// Raise ArgumentException if the arrays have different lengths.
    //-[<CompiledName("Forall2")>]
    [<Obsolete>] 
    let forall2 predicate (resizeArray1 : ResizeArray<'T1>) (resizeArray2 : ResizeArray<'T2>) : bool =
        checkNonNull "resizeArray1" resizeArray1
        checkNonNull "resizeArray2" resizeArray2

        let len = length resizeArray1
        if len <> length resizeArray2 then
            invalidArg "resizeArray2" "The ResizeArrays have different lengths."

        let predicate = FSharpFunc<_,_,_>.Adapt predicate
    
        let mutable index = 0
        let mutable allMatch = true
        while index < len && allMatch do
            allMatch <- predicate.Invoke (resizeArray1.[index], resizeArray2.[index])
            index <- index + 1
        allMatch

    /// Return a new collection containing only the elements of the collection
    /// for which the given predicate returns <c>true</c>.
    //-[<CompiledName("Filter")>]
    [<Obsolete>] 
    let inline filter (predicate : 'T -> bool) (resizeArray : ResizeArray<'T>) : ResizeArray<'T> =
        checkNonNull "resizeArray" resizeArray
        resizeArray.FindAll (System.Predicate predicate)
       

    /// <summary>
    /// Apply the given function to each element of the array. Return
    /// the array comprised of the results "x" for each element where
    /// the function returns <c>Some(x)</c>.
    /// </summary>
    //-[<CompiledName("Choose")>]
    [<Obsolete>] 
    let choose (chooser : 'T -> 'U option) (resizeArray : ResizeArray<'T>) : ResizeArray<'U> =
        checkNonNull "resizeArray" resizeArray

        // OPTIMIZATION : If the input list is empty return immediately.
        if isEmpty resizeArray then
            ResizeArray ()
        else
            let result = ResizeArray ()
            let count = resizeArray.Count

            for i = 0 to count - 1 do
                match chooser resizeArray.[i] with
                | None -> ()
                | Some value ->
                    result.Add value

            result

    /// <summary>
    /// Return the first element for which the given function returns <c>true</c>.
    /// Return <c>None</c> if no such element exists.
    /// </summary>
    //-[<CompiledName("TryFind")>]
    [<Obsolete>] 
    let tryFind (predicate : 'T -> bool) (resizeArray : ResizeArray<'T>) : 'T option =
        checkNonNull "resizeArray" resizeArray

        let elementIndex =            
            resizeArray.FindIndex (System.Predicate predicate)

        match elementIndex with
        | -1 ->
            None
        | index ->
            Some resizeArray.[index]

    /// <summary>
    /// Return the first element for which the given function returns <c>true</c>.
    /// Raise <c>KeyNotFoundException</c> if no such element exists.
    /// </summary>
    //-[<CompiledName("Find")>]
    [<Obsolete>] 
    let find (predicate : 'T -> bool) (resizeArray : ResizeArray<'T>) : 'T =
        checkNonNull "resizeArray" resizeArray

        let elementIndex =           
            resizeArray.FindIndex (System.Predicate predicate)
          

        match elementIndex with
        | -1 ->
            // TODO : Add a better error message.
            // keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()
        | index ->
            resizeArray.[index]

    /// Return the index of the first element in the array
    /// that satisfies the given predicate.
    //-[<CompiledName("TryFindIndex")>]
    [<Obsolete>] 
    let tryFindIndex (predicate : 'T -> bool) (resizeArray : ResizeArray<'T>) : int option =
        checkNonNull "resizeArray" resizeArray

        let elementIndex =          
            resizeArray.FindIndex (System.Predicate predicate)
         

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
    [<Obsolete>] 
    let findIndex (predicate : 'T -> bool) (resizeArray : ResizeArray<'T>) : int =
        checkNonNull "resizeArray" resizeArray

        let elementIndex =           
            resizeArray.FindIndex (System.Predicate predicate)
           
        match elementIndex with
        | -1 ->
            // TODO : Add a better error message.
            // keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()
        | index ->
            index

    /// Return the index of the first element in the array
    /// that satisfies the given predicate.
    //-[<CompiledName("TryFindIndexIndexed")>]
    [<Obsolete>] 
    let tryFindIndexi predicate (resizeArray : ResizeArray<'T>) : int option =
        checkNonNull "resizeArray" resizeArray

        let predicate = FSharpFunc<_,_,_>.Adapt predicate

        let lastIndex = length resizeArray - 1
        let mutable index = -1
        let mutable foundMatch = false
        while index < lastIndex && not foundMatch do
            let i = index + 1
            index <- i
            foundMatch <- predicate.Invoke (i, resizeArray.[i])

        if foundMatch then
            Some index
        else None

    /// <summary>
    /// Return the index of the first element in the array
    /// that satisfies the given predicate. Raise <c>KeyNotFoundException</c> if 
    /// none of the elements satisfy the predicate.
    /// </summary>
    //-[<CompiledName("FindIndexIndexed")>]
    [<Obsolete>] 
    let findIndexi predicate (resizeArray : ResizeArray<'T>) : int =
        checkNonNull "resizeArray" resizeArray

        match tryFindIndexi predicate resizeArray with
        | Some index ->
            index
        | None ->
            KeyNotFoundException.Raise "An element satisfying the predicate was not found in the collection." // edit Goswin

    /// <summary>
    /// Applies the given function to successive elements, returning the first
    /// result where function returns <c>Some(x)</c> for some x. If the function
    /// never returns <c>Some(x)</c>, returns <c>None</c>.
    /// </summary>
    //-[<CompiledName("TryPick")>]
    [<Obsolete>] 
    let tryPick (picker : 'T -> 'U option) (resizeArray : ResizeArray<'T>) : 'U option =
        checkNonNull "resizeArray" resizeArray

        let count = resizeArray.Count
        let mutable result = None
        let mutable index = 0

        while index < count && Option.isNone result do
            result <- picker resizeArray.[index]
            index <- index + 1
        result

    /// <summary>
    /// Applies the given function to successive elements, returning the first
    /// result where function returns <c>Some(x)</c> for some x. If the function
    /// never returns <c>Some(x)</c>, raises KeyNotFoundException.
    /// </summary>
    //-[<CompiledName("Pick")>]
    [<Obsolete>] 
    let pick (picker : 'T -> 'U option) (resizeArray : ResizeArray<'T>) : 'U =
        checkNonNull "resizeArray" resizeArray

        let count = resizeArray.Count
        let mutable result = None
        let mutable index = 0

        while index < count && Option.isNone result do
            result <- picker resizeArray.[index]
            index <- index + 1

        match result with
        | Some result ->
            result
        | None ->
            // TODO : Return a better error message
            //keyNotFound ""
            raise <| System.Collections.Generic.KeyNotFoundException ()

    /// Apply the given function to each element of the array.
    //-[<CompiledName("Iterate")>]
    [<Obsolete>] 
    let iter (action : 'T -> unit) (resizeArray : ResizeArray<'T>) : unit =
        checkNonNull "resizeArray" resizeArray

        let count = resizeArray.Count
        for i = 0 to count - 1 do
            action resizeArray.[i]

    /// Apply the given function to each element of the array. The integer passed to the
    /// function indicates the index of element.
    //-[<CompiledName("IterateIndexed")>]
    [<Obsolete>] 
    let iteri (action : int -> 'T -> unit) (resizeArray : ResizeArray<'T>) : unit =
        checkNonNull "resizeArray" resizeArray

        let action = FSharpFunc<_,_,_>.Adapt action

        let count = resizeArray.Count
        for i = 0 to count - 1 do
            action.Invoke (i, resizeArray.[i])

    /// <summary>
    /// Apply the given function to two arrays simultaneously. The two arrays
    /// must have the same lengths, otherwise an <c>ArgumentException</c> is raised.
    /// </summary>
    //-[<CompiledName("Iterate2")>]
    [<Obsolete>] 
    let iter2 action (resizeArray1 : ResizeArray<'T1>) (resizeArray2 : ResizeArray<'T1>) : unit =
        checkNonNull "resizeArray1" resizeArray1
        checkNonNull "resizeArray2" resizeArray2

        let len = length resizeArray1
        if len <> length resizeArray2 then
            invalidArg "resizeArray2" "The ResizeArrays have different lengths."

        let action = FSharpFunc<_,_,_>.Adapt action

        for i = 0 to len - 1 do
            action.Invoke (resizeArray1.[i], resizeArray2.[i])

    /// <summary>
    /// Apply the given function to pair of elements drawn from matching indices in two arrays,
    /// also passing the index of the elements. The two arrays must have the same lengths, 
    /// otherwise an <c>ArgumentException</c> is raised.
    /// </summary>
    //-[<CompiledName("IterateIndexed2")>]
    [<Obsolete>] 
    let iteri2 action (resizeArray1 : ResizeArray<'T1>) (resizeArray2 : ResizeArray<'T2>) : unit =
        checkNonNull "resizeArray1" resizeArray1
        checkNonNull "resizeArray2" resizeArray2

        let len = length resizeArray1
        if len <> length resizeArray2 then
            invalidArg "resizeArray2" "The ResizeArrays have different lengths."

        let action = FSharpFunc<_,_,_,_>.Adapt action

        for i = 0 to len - 1 do
            action.Invoke (i, resizeArray1.[i], resizeArray2.[i])

    /// <summary>
    /// Build a new array whose elements are the results of applying the given function
    /// to each of the elements of the array.
    /// </summary>
    /// <param name="mapping"></param>
    /// <param name="resizeArray"></param>
    /// <returns></returns>
    //-[<CompiledName("Map")>]
    [<Obsolete>] 
    let inline map (mapping : 'T -> 'U) (resizeArray : ResizeArray<'T>) : ResizeArray<'U> =
        checkNonNull "resizeArray" resizeArray
        resizeArray.ConvertAll (System.Converter mapping)
        

    /// <summary>
    /// Build a new array whose elements are the results of applying the given function
    /// to each of the elements of the array. The integer index passed to the
    /// function indicates the index of element being transformed.
    /// </summary>
    /// <param name="mapping"></param>
    /// <param name="resizeArray"></param>
    /// <returns></returns>
    //-[<CompiledName("MapIndexed")>]
    [<Obsolete>] 
    let mapi (mapping : int -> 'T -> 'U) (resizeArray : ResizeArray<'T>) : ResizeArray<'U> =
        checkNonNull "resizeArray" resizeArray

        let mapping = FSharpFunc<_,_,_>.Adapt mapping
        let count = resizeArray.Count
        let result = ResizeArray (count)

        for i = 0 to count - 1 do
            result.Add <| mapping.Invoke (i, resizeArray.[i])
        result

    /// <summary>
    /// Build a new collection whose elements are the results of applying the given function
    /// to the corresponding elements of the two collections pairwise. The two input
    /// arrays must have the same lengths.
    /// </summary>
    /// <param name="mapping"></param>
    /// <param name="resizeArray1"></param>
    /// <param name="resizeArray2"></param>
    /// <returns></returns>
    //-[<CompiledName("Map2")>]
    [<Obsolete>] 
    let map2 mapping (resizeArray1 : ResizeArray<'T1>) (resizeArray2 : ResizeArray<'T2>) : ResizeArray<'U> =
        checkNonNull "resizeArray1" resizeArray1
        checkNonNull "resizeArray2" resizeArray2

        let len = length resizeArray1
        if len <> length resizeArray2 then
            invalidArg "resizeArray2" "The ResizeArrays have different lengths."

        let mapping = FSharpFunc<_,_,_>.Adapt mapping
        let results = ResizeArray (len)

        for i = 0 to len - 1 do
            mapping.Invoke (resizeArray1.[i], resizeArray2.[i])
            |> results.Add
        results

    /// <summary>
    /// Build a new collection whose elements are the results of applying the given function
    /// to the corresponding elements of the two collections pairwise. The two input
    /// arrays must have the same lengths, otherwise an <c>ArgumentException</c> is raised.
    /// </summary>
    /// <param name="mapping"></param>
    /// <param name="resizeArray1"></param>
    /// <param name="resizeArray2"></param>
    /// <returns></returns>
    //-[<CompiledName("MapIndexed2")>]
    [<Obsolete>] 
    let mapi2 mapping (resizeArray1 : ResizeArray<'T1>) (resizeArray2 : ResizeArray<'T2>) : ResizeArray<'U> =
        checkNonNull "resizeArray1" resizeArray1
        checkNonNull "resizeArray2" resizeArray2

        let len = length resizeArray1
        if len <> length resizeArray2 then
            invalidArg "resizeArray2" "The ResizeArrays have different lengths."

        let mapping = FSharpFunc<_,_,_,_>.Adapt mapping
        let results = ResizeArray (len)

        for i = 0 to len - 1 do
            mapping.Invoke (i, resizeArray1.[i], resizeArray2.[i])
            |> results.Add
        results

    /// <summary>
    /// Apply a function to each element of the collection, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> 
    /// then computes <c>f (... (f s i0)...) iN</c>.
    /// </summary>
    /// <param name="folder"></param>
    /// <param name="state"></param>
    /// <param name="resizeArray"></param>
    /// <returns></returns>
    //-[<CompiledName("Fold")>]
    [<Obsolete>] 
    let fold (folder : 'State -> 'T -> 'State) state (resizeArray : ResizeArray<'T>) : 'State =
        checkNonNull "resizeArray" resizeArray

        let folder = FSharpFunc<_,_,_>.Adapt folder

        let mutable state = state
        let count = resizeArray.Count
        for i = 0 to count - 1 do
            state <- folder.Invoke (state, resizeArray.[i])
        state

    /// <summary>foldSub on just part of teh ResizeArray</summary>
    /// <param name="folder"></param>
    /// <param name="state"></param>
    /// <param name="resizeArray"></param>
    /// <param name="startIndex"></param>
    /// <param name="endIndex"></param>
    /// <returns></returns>
    //-[<CompiledName("FoldSub")>]
    [<Obsolete>] 
    let foldSub folder (state : 'State) (resizeArray : ResizeArray<'T>) startIndex endIndex : 'State =
        checkNonNull "resizeArray" resizeArray
        if startIndex < 0 then
            IndexOutOfRangeException.Raise "The starting index cannot be negative. but is %d" startIndex
        elif endIndex > 0 then
            IndexOutOfRangeException.Raise "The ending index cannot be negative. but is %d" endIndex
    
        let len = length resizeArray
        if startIndex >= len then
            IndexOutOfRangeException.Raise  "The starting index is outside the bounds of the ResizeArray: %d of %d" startIndex len
        elif endIndex >= len then
            IndexOutOfRangeException.Raise  "The ending index is outside the bounds of the ResizeArray: %d of %d" endIndex len

        let folder = FSharpFunc<_,_,_>.Adapt folder

        // Fold over the specified range of items.
        let mutable state = state
        for i = startIndex to endIndex do
            state <- folder.Invoke (state, resizeArray.[i])
        state

    /// <summary>
    /// Applies a function to each element of the collection, threading an accumulator argument
    /// through the computation. The integer index passed to the function indicates the
    /// index of the element within the collection.
    /// </summary>
    /// <param name="folder"></param>
    /// <param name="state"></param>
    /// <param name="resizeArray"></param>
    /// <returns></returns>
    //-[<CompiledName("FoldIndexed")>]
    [<Obsolete>] 
    let foldi (folder : 'State -> int -> 'T -> 'State) state (resizeArray : ResizeArray<'T>) : 'State =
        checkNonNull "resizeArray" resizeArray

        let folder = FSharpFunc<_,_,_,_>.Adapt folder

        let mutable state = state
        let count = resizeArray.Count
        for i = 0 to count - 1 do
            state <- folder.Invoke (state, i, resizeArray.[i])
        state

    /// <summary>
    /// Apply a function to pairs of elements drawn from the two collections, left-to-right,
    /// threading an accumulator argument through the computation.  The two input arrays must
    /// have the same lengths, otherwise an <c>ArgumentException</c> is raised.
    /// </summary>
    /// <param name="folder"></param>
    /// <param name="state"></param>
    /// <param name="resizeArray1"></param>
    /// <param name="resizeArray2"></param>
    /// <returns></returns>
    //-[<CompiledName("Fold2")>]
    [<Obsolete>] 
    let fold2 folder (state : 'State) (resizeArray1 : ResizeArray<'T1>) (resizeArray2 : ResizeArray<'T2>) : 'State =
        checkNonNull "resizeArray1" resizeArray1
        checkNonNull "resizeArray2" resizeArray2

        let len = length resizeArray1
        if len <> length resizeArray2 then
            invalidArg "resizeArray2" "The arrays have different lengths."

        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        let mutable state = state
        for i = 0 to len - 1 do
            state <- folder.Invoke (state, resizeArray1.[i], resizeArray2.[i])
        state

    /// <summary>
    /// Apply a function to each element of the array, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are
    /// <c>i0...iN</c> then computes <c>f i0 (...(f iN s))</c>.
    /// </summary>
    /// <param name="folder"></param>
    /// <param name="resizeArray"></param>
    /// <param name="state"></param>
    /// <returns></returns>
    //-[<CompiledName("FoldBack")>]
    [<Obsolete>] 
    let foldBack (folder : 'T -> 'State -> 'State) (resizeArray : ResizeArray<'T>) state : 'State =
        checkNonNull "resizeArray" resizeArray

        let folder = FSharpFunc<_,_,_>.Adapt folder

        let mutable state = state
        for i = resizeArray.Count - 1 downto 0 do
            state <- folder.Invoke (resizeArray.[i], state)
        state

    /// <summary>foldBackSub</summary>
    /// <param name="folder"></param>
    /// <param name="resizeArray"></param>
    /// <param name="startIndex"></param>
    /// <param name="endIndex"></param>
    /// <param name="state"></param>
    /// <returns></returns>
    //-[<CompiledName("FoldBackSub")>]
    [<Obsolete>] 
    let foldBackSub folder (resizeArray : ResizeArray<'T>) startIndex endIndex (state : 'State) : 'State =
        checkNonNull "resizeArray" resizeArray
        if startIndex < 0 then
            IndexOutOfRangeException.Raise "The starting index cannot be negative. but is %d" startIndex
        elif endIndex > 0 then
            IndexOutOfRangeException.Raise "The ending index cannot be negative. but is %d" endIndex
    
        let len = length resizeArray
        if startIndex >= len then
            IndexOutOfRangeException.Raise  "The starting index is outside the bounds of the ResizeArray: %d of %d" startIndex len
        elif endIndex >= len then
            IndexOutOfRangeException.Raise  "The ending index is outside the bounds of the ResizeArray: %d of %d" endIndex len

        let folder = FSharpFunc<_,_,_>.Adapt folder

        // Fold over the specified range of items.
        let mutable state = state
        for i = endIndex downto startIndex do
            state <- folder.Invoke (resizeArray.[i], state)
        state

    /// <summary>foldiBack</summary>
    /// <param name="folder"></param>
    /// <param name="resizeArray"></param>
    /// <param name="state"></param>
    /// <returns></returns>
    //-[<CompiledName("FoldBackIndexed")>]
    [<Obsolete>] 
    let foldiBack (folder : int -> 'T -> 'State -> 'State) (resizeArray : ResizeArray<'T>) state : 'State =
        checkNonNull "resizeArray" resizeArray

        let folder = FSharpFunc<_,_,_,_>.Adapt folder

        let mutable state = state
        for i = resizeArray.Count - 1 downto 0 do
            state <- folder.Invoke (i, resizeArray.[i], state)
        state

    /// <summary>
    /// Apply a function to pairs of elements drawn from the two collections, right-to-left, 
    /// threading an accumulator argument through the computation.  The two input
    /// arrays must have the same lengths, otherwise an <c>ArgumentException</c> is raised.
    /// </summary>
    /// <param name="folder"></param>
    /// <param name="resizeArray1"></param>
    /// <param name="resizeArray2"></param>
    /// <param name="state"></param>
    /// <returns></returns>
    //-[<CompiledName("FoldBack2")>]
    [<Obsolete>] 
    let foldBack2 folder (resizeArray1 : ResizeArray<'T1>) (resizeArray2 : ResizeArray<'T2>) (state : 'State) : 'State =
        checkNonNull "resizeArray1" resizeArray1
        checkNonNull "resizeArray2" resizeArray2

        let len = length resizeArray1
        if len <> length resizeArray2 then
            invalidArg "resizeArray2" "The arrays have different lengths."

        let folder = FSharpFunc<_,_,_,_>.Adapt folder
        let mutable state = state
        for i = len - 1 downto 0 do
            state <- folder.Invoke (resizeArray1.[i], resizeArray2.[i], state)
        state

    /// <summary>
    /// Apply a function to each element of the array, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> 
    /// then computes <c>f (... (f i0 i1)...) iN</c>.
    /// Raises <c>ArgumentException</c> if the array has size zero.
    /// </summary>
    /// <param name="reduction"></param>
    /// <param name="resizeArray"></param>
    /// <returns></returns>
    //-[<CompiledName("Reduce")>]
    [<Obsolete>] 
    let reduce (reduction : 'T -> 'T -> 'T) (resizeArray : ResizeArray<'T>) =
        checkNonNull "resizeArray" resizeArray
        if isEmpty resizeArray then
            invalidArg "resizeArray" "The ResizeArray is empty."

        let reduction = FSharpFunc<_,_,_>.Adapt reduction

        let mutable state = resizeArray.[0]
        let count = resizeArray.Count
        for i = 1 to count - 1 do
            state <- reduction.Invoke (state, resizeArray.[i])
        state

    /// <summary>
    /// Apply a function to each element of the array, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> then 
    /// computes <c>f i0 (...(f iN-1 iN))</c>.
    /// Raises <c>ArgumentException</c> if the array has size zero.
    /// </summary>
    /// <param name="reduction"></param>
    /// <param name="resizeArray"></param>
    /// <returns></returns>
    //-[<CompiledName("ReduceBack")>]
    [<Obsolete>] 
    let reduceBack (reduction : 'T -> 'T -> 'T) (resizeArray : ResizeArray<'T>) : 'T =
        checkNonNull "resizeArray" resizeArray
        if isEmpty resizeArray then
            invalidArg "resizeArray" "The ResizeArray is empty."

        let reduction = FSharpFunc<_,_,_>.Adapt reduction

        let count = resizeArray.Count
        let mutable state = resizeArray.[count - 1]

        for i = count - 2 downto 0 do
            state <- reduction.Invoke (resizeArray.[i], state)
        state

    /// <summary>scanSub</summary>
    /// <param name="folder"></param>
    /// <param name="state"></param>
    /// <param name="resizeArray"></param>
    /// <param name="startIndex"></param>
    /// <param name="endIndex"></param>
    /// <returns></returns>
    //-[<CompiledName("ScanSub")>]
    [<Obsolete>] 
    let scanSub folder (state : 'State) (resizeArray : ResizeArray<'T>) startIndex endIndex : ResizeArray<'State> =
        checkNonNull "resizeArray" resizeArray
        if startIndex < 0 then
            IndexOutOfRangeException.Raise "The starting index cannot be negative. but is %d" startIndex
        elif endIndex > 0 then
            IndexOutOfRangeException.Raise "The ending index cannot be negative. but is %d" endIndex
    
        let len = length resizeArray
        if startIndex >= len then
            IndexOutOfRangeException.Raise  "The starting index is outside the bounds of the ResizeArray: %d of %d" startIndex len
        elif endIndex >= len then
            IndexOutOfRangeException.Raise  "The ending index is outside the bounds of the ResizeArray: %d of %d" endIndex len

        let folder = FSharpFunc<_,_,_>.Adapt folder

        // Holds the initial and intermediate state values.
        let results = ResizeArray (endIndex - startIndex + 2)
        results.Add state

        // Fold over the specified range of items.
        let mutable state = state
        for i = startIndex to endIndex do
            state <- folder.Invoke (state, resizeArray.[i])
            results.Add state
        results

    /// <summary>
    /// Like <c>fold</c>, but return the intermediary and final results.
    /// </summary>
    /// <param name="folder"></param>
    /// <param name="state"></param>
    /// <param name="resizeArray"></param>
    /// <returns></returns>
    //-[<CompiledName("Scan")>]
    [<Obsolete>] 
    let scan folder (state : 'State) (resizeArray : ResizeArray<'T>) : ResizeArray<'State> =
        checkNonNull "resizeArray" resizeArray

        scanSub folder state resizeArray 0 (length resizeArray - 1)

    /// <summary>scanBackSub</summary>
    /// <param name="folder"></param>
    /// <param name="resizeArray"></param>
    /// <param name="startIndex"></param>
    /// <param name="endIndex"></param>
    /// <param name="state"></param>
    /// <returns></returns>
    //-[<CompiledName("ScanBackSub")>]
    [<Obsolete>] 
    let scanBackSub folder (resizeArray : ResizeArray<'T>) startIndex endIndex (state : 'State) : ResizeArray<'State> =
        checkNonNull "resizeArray" resizeArray
        if startIndex < 0 then
            IndexOutOfRangeException.Raise "The starting index cannot be negative. but is %d" startIndex
        elif endIndex > 0 then
            IndexOutOfRangeException.Raise "The ending index cannot be negative. but is %d" endIndex
    
        let len = length resizeArray
        if startIndex >= len then
            IndexOutOfRangeException.Raise  "The starting index is outside the bounds of the ResizeArray: %d of %d" startIndex len
        elif endIndex >= len then
            IndexOutOfRangeException.Raise  "The ending index is outside the bounds of the ResizeArray: %d of %d" endIndex len

        let folder = FSharpFunc<_,_,_>.Adapt folder

        // Holds the initial and intermediate state values.
        let results = ResizeArray (endIndex - startIndex + 2)
        results.Add state

        // Fold over the specified range of items.
        let mutable state = state
        for i = endIndex downto startIndex do
            state <- folder.Invoke (resizeArray.[i], state)
            results.Insert (0, state)
        results

    /// <summary>
    /// Like <c>foldBack</c>, but return both the intermediary and final results.
    /// </summary>
    /// <param name="folder"></param>
    /// <param name="resizeArray"></param>
    /// <param name="state"></param>
    /// <returns></returns>
    //-[<CompiledName("ScanBack")>]
    [<Obsolete>] 
    let scanBack folder (resizeArray : ResizeArray<'T>) (state : 'State) : ResizeArray<'State> =
        checkNonNull "resizeArray" resizeArray

        scanBackSub folder resizeArray 0 (length resizeArray - 1) state

    /// <summary>
    /// Split the collection into two collections, containing the elements for which
    /// the given predicate returns <c>true</c> and <c>false</c> respectively.
    /// </summary>
    /// <param name="predicate"></param>
    /// <param name="resizeArray"></param>
    /// <returns></returns>
    //-[<CompiledName("Partition")>]
    [<Obsolete>] 
    let partition predicate (resizeArray : ResizeArray<'T>) : ResizeArray<'T> * ResizeArray<'T> =
        checkNonNull "resizeArray" resizeArray

        let trueResults = ResizeArray ()
        let falseResults = ResizeArray ()

        let len = length resizeArray
        for i = 0 to len - 1 do
            let el = resizeArray.[i]
            if predicate el then
                trueResults.Add el
            else
                falseResults.Add el

        trueResults, falseResults

    /// <summary>
    /// Splits the collection into two (2) collections, containing the elements for which the
    /// given function returns <c>Choice1Of2</c> or <c>Choice2Of2</c>, respectively. This function is similar to
    /// <c>ResizeArray.partition</c>, but it allows the returned collections to have different element types.
    /// </summary>
    /// <param name="partitioner"></param>
    /// <param name="resizeArray"></param>
    /// <returns></returns>
    //-[<CompiledName("MapPartition")>]
    [<Obsolete>] 
    let mapPartition partitioner (resizeArray : ResizeArray<'T>) : ResizeArray<'U1> * ResizeArray<'U2> =
        checkNonNull "resizeArray" resizeArray

        let results1 = ResizeArray ()
        let results2 = ResizeArray ()

        let len = length resizeArray
        for i = 0 to len - 1 do
            match partitioner resizeArray.[i] with
            | Choice1Of2 value ->
                results1.Add value
            | Choice2Of2 value ->
                results2.Add value

        results1, results2

    /// <summary>Returns the sum of the elements in the ResizeArray.</summary>
    /// <param name="resizeArray">The input ResizeArray.</param>
    /// <returns>The resulting sum.</returns>
    //-[<CompiledName("Sum")>]
    [<Obsolete>] 
    let inline sum (resizeArray : ResizeArray< ^T>) : ^T = 
        checkNonNull "resizeArray" resizeArray

        let mutable acc = LanguagePrimitives.GenericZero< (^T) >
    //    for x in resizeArray do
    //        acc <- Checked.(+) acc x
        for i = 0 to resizeArray.Count - 1 do
            acc <- Checked.(+) acc resizeArray.[i]
        acc

    /// <summary>Returns the sum of the results generated by applying the function to each element of the ResizeArray.</summary>
    /// <param name="projection">The function to transform the ResizeArray elements into the type to be summed.</param>
    /// <param name="resizeArray">The input ResizeArray.</param>
    /// <returns>The resulting sum.</returns>
    //-[<CompiledName("SumBy")>]
    [<Obsolete>] 
    let inline sumBy (projection : 'T -> ^U) (resizeArray : ResizeArray<'T>) : ^U = 
        checkNonNull "resizeArray" resizeArray

        let mutable acc = LanguagePrimitives.GenericZero< (^U) >
    //    for x in resizeArray do
    //        acc <- Checked.(+) acc (projection x)
        for i = 0 to resizeArray.Count - 1 do
            acc <- Checked.(+) acc (projection resizeArray.[i])
        acc

    /// <summary>Returns the lowest of all elements of the ResizeArray, compared via Operators.min.</summary>
    /// <param name="resizeArray">The input ResizeArray.</param>
    /// <returns>The minimum element.</returns>
    /// <exception cref="System.ArgumentException">Thrown when <paramref name="resizeArray"/> is empty.</exception>
    //-[<CompiledName("Min")>]
    [<Obsolete>] 
    let inline min (resizeArray : ResizeArray<'T>) =
        checkNonNull "resizeArray" resizeArray
        if resizeArray.Count = 0 then
            invalidArg "resizeArray" "The input collection is empty."

        let mutable acc = resizeArray.[0]
        for i = 1 to resizeArray.Count - 1 do
            let curr = resizeArray.[i]
            if curr < acc then
                acc <- curr
        acc

    /// <summary>Returns the lowest of all elements of the ResizeArray, compared via Operators.min on the function result.</summary>
    /// <param name="projection">The function to transform the elements into a type supporting comparison.</param>
    /// <param name="resizeArray">The input ResizeArray.</param>
    /// <returns>The minimum element.</returns>
    /// <exception cref="System.ArgumentException">Thrown when <paramref name="resizeArray"/> is empty.</exception>
    //-[<CompiledName("MinBy")>]
    [<Obsolete>] 
    let inline minBy (projection : 'T -> 'U) (resizeArray : ResizeArray<'T>) =
        checkNonNull "resizeArray" resizeArray
        if resizeArray.Count = 0 then
            invalidArg "resizeArray" "The input collection is empty."

        let mutable accv = resizeArray.[0]
        let mutable acc = projection accv
        for i = 1 to resizeArray.Count - 1 do
            let currv = resizeArray.[i]
            let curr = projection currv
            if curr < acc then
                acc <- curr
                accv <- currv
        accv

    /// <summary>Returns the greatest of all elements of the ResizeArray, compared via Operators.max on the function result.</summary>
    /// <param name="resizeArray">The input ResizeArray.</param>
    /// <returns>The maximum element.</returns>
    /// <exception cref="System.ArgumentException">Thrown when <paramref name="resizeArray"/> is empty.</exception>
    //-[<CompiledName("Max")>]
    [<Obsolete>] 
    let inline max (resizeArray : ResizeArray<'T>) =
        checkNonNull "resizeArray" resizeArray
        if resizeArray.Count = 0 then
            invalidArg "resizeArray" "The input collection is empty."

        let mutable acc = resizeArray.[0]
        for i = 1 to resizeArray.Count - 1 do
            let curr = resizeArray.[i]
            if curr > acc then
                acc <- curr
        acc

    /// <summary>Returns the greatest of all elements of the ResizeArray, compared via Operators.max on the function result.</summary>
    /// <param name="projection">The function to transform the elements into a type supporting comparison.</param>
    /// <param name="resizeArray">The input ResizeArray.</param>
    /// <returns>The maximum element.</returns>
    /// <exception cref="System.ArgumentException">Thrown when <paramref name="resizeArray"/> is empty.</exception>
    //-[<CompiledName("MaxBy")>]
    [<Obsolete>] 
    let inline maxBy (projection : 'T -> 'U) (resizeArray : ResizeArray<'T>) =
        checkNonNull "resizeArray" resizeArray
        if resizeArray.Count = 0 then
            invalidArg "resizeArray" "The input collection is empty."

        let mutable accv = resizeArray.[0]
        let mutable acc = projection accv
        for i = 1 to resizeArray.Count - 1 do
            let currv = resizeArray.[i]
            let curr = projection currv
            if curr > acc then
                acc <- curr
                accv <- currv
        accv

    /// <summary>Returns the average of the elements in the ResizeArray.</summary>
    /// <param name="resizeArray">The input ResizeArray.</param>
    /// <returns>The average of the elements in the ResizeArray.</returns>
    /// <exception cref="System.ArgumentException">Thrown when <paramref name="resizeArray"/> is empty.</exception>
    //-[<CompiledName("Average")>]
    [<Obsolete>] 
    let inline average (resizeArray : ResizeArray<'T>) : ^T =
        checkNonNull "resizeArray" resizeArray

        Seq.average resizeArray

    /// <summary>Returns the average of the elements generated by applying the function to each element of the ResizeArray.</summary>
    /// <param name="projection">The function to transform the ResizeArray elements before averaging.</param>
    /// <param name="resizeArray">The input ResizeArray.</param>
    /// <returns>The computed average.</returns>
    /// <exception cref="System.ArgumentException">Thrown when <paramref name="resizeArray"/> is empty.</exception>
    //-[<CompiledName("AverageBy")>]
    [<Obsolete>] 
    let inline averageBy (projection : 'T -> ^U) (resizeArray : ResizeArray<'T>) : ^U = 
        checkNonNull "resizeArray" resizeArray

        Seq.averageBy projection resizeArray
