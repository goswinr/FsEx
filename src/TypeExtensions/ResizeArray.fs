namespace FsEx


[<AutoOpen>]
module TypeExtensionsResizeArray =   
    open System
    open Microsoft.FSharp.Core
    open System.Runtime.CompilerServices

    //[<Extension>] //Error 3246
    type Collections.Generic.List<'T>  with 
    
        /// Gets the index of the last item in the ResizeArray.
        /// equal to this.Count - 1    
        [<Extension>]
        member inline this.LastIndex = 
            if this.Count = 0 then failwithf "resizeArray.LastIndex: Can not get LastIndex of empty List"
            this.Count - 1

        /// Gets the last item in the ResizeArray.
        /// equal to this.[this.Count - 1]
        [<Extension>]
        member inline this.Last = 
            if this.Count = 0 then failwithf "resizeArray.Last: Can not get Last item of empty List"
            this.[this.Count - 1]
        
        /// Gets the second last item in the ResizeArray.
        /// equal to this.[this.Count - 2]
        [<Extension>]
        member inline this.SecondLast = 
            if this.Count < 2 then failwithf "resizeArray.SecondLast: Can not get SecondLast item of %s"  (NiceString.toNiceStringFull this)
            this.[this.Count - 2]

        /// Gets the third last item in the ResizeArray.
        /// equal to this.[this.Count - 3]
        [<Extension>]
        member inline this.ThirdLast = 
            if this.Count < 3 then failwithf "resizeArray.ThirdLast: Can not get ThirdLast item of %s"  (NiceString.toNiceStringFull this)
            this.[this.Count - 3]

        
        /// Gets the first item in the ResizeArray.
        /// equal to this.[0]
        [<Extension>]
        member inline this.First = 
            if this.Count = 0 then failwithf "resizeArray.First: Can not get First item of empty List"
            this.[0]

        /// Gets the second item in the ResizeArray.
        /// equal to this.[1]
        [<Extension>]
        member inline this.Second = 
            if this.Count < 2 then failwithf "resizeArray.Second: Can not get Second item of %s"  (NiceString.toNiceStringFull this)
            this.[1]

        /// Gets the third item in the ResizeArray.
        /// equal to this.[2]
        [<Extension>]
        member inline this.Third = 
            if this.Count < 3 then failwithf "resizeArray.Third: Can not get Third item of %s"  (NiceString.toNiceStringFull this)
            this.[2]

        /// Checks if this.Count = 0 
        [<Extension>]
        member inline this.IsEmpty = 
            this.Count = 0 
        
        /// Checks if this.Count > 0 
        [<Extension>]
        member inline this.IsNotEmpty = 
            this.Count > 0 

        /// Gets an item in the ResizeArray by index.
        /// Allows for negtive index too ( -1 is last item,  like Python)
        /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
        [<Extension>] 
        member this.GetItem index = 
            let i = negIdx index this.Count
            this.[i]
        
        
        /// Sets an item in the ResizeArray by index.
        /// Allows for negtive index too ( -1 is last item, like Python)
        /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
        [<Extension>] 
        member this.SetItem index value = 
            let i = negIdx index this.Count
            this.[i] <- value 
        
        
        /// Gets and remove last item from ResizeArray
        [<Extension>] 
        member  this.Pop()  =
            let i = this.Count - 1
            let v = this.[i]
            this.RemoveAt(i)
            v

        
        /// Defines F# slicing notation operator use including negative indices. ( -1 is last item, like Python)
        /// The resulting ResizeArray includes the end index.
        /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
        [<Extension>]
        member this.GetSlice(startIdx, endIdx) =    // maps onto slicing operator .[1..3]
            let count = this.Count
            let st  = match startIdx with None -> 0        | Some i -> if i<0 then count+i      else i
            let len = match endIdx   with None -> count-st | Some i -> if i<0 then count+i-st+1 else i-st+1
    
            if st < 0 || st > count-1 then 
                let err = sprintf "resizeArray.GetSlice: Start index %d is out of range. Allowed values are -%d upto %d for List of %d items" startIdx.Value count (count-1) count
                raise (IndexOutOfRangeException(err))
    
            if st+len > count then 
                let err = sprintf "resizeArray.GetSlice: End index %d is out of range. Allowed values are -%d upto %d for List of %d items" endIdx.Value count (count-1) count
                raise (IndexOutOfRangeException(err)) 
                
            if len < 0 then
                let en =  match endIdx  with None -> count-1 | Some i -> if i<0 then count+i else i
                let err = sprintf "resizeArray.GetSlice: Start index '%A' (= %d) is bigger than end index '%A'(= %d) for List of %d items" startIdx st endIdx en  count
                raise (IndexOutOfRangeException(err)) 
                
            this.GetRange(st, len) 
         
         
         /// Allows for negative indices too. ( -1 is last item, like Python)
         /// This method only exist to be consisten with other collections extensions in FsEx. like array. 
         /// You might prefer to use the F# slicing notation on ResizeArrays.
         /// For ResizeArrays this behaves the same way as the F# slicing notation defind in FsEx too, 
         /// (Only arrays need to use this method if they want to use negative indices since the GetSlice operators cant be overwritten.) 
         /// The resulting array includes the end index.
         /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
         [<Extension>]
         member this.Slice(startIdx, endIdx) = this.GetSlice(startIdx, endIdx)    
        
        /// A property like the ToString() method, 
        /// But with richer formationg for collections
        [<Extension>]  
        member this.ToNiceString = NiceString.toNiceString this


/// Generic operations on the type System.Collections.Generic.List, which is called ResizeArray in the F# libraries.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] //need this so doesn't hide ResizeArray class in C# assemblies (should consider for other extension modules as well)
module ResizeArray =
    // taken and extended from https://github.com/dotnet/fsharp/tree/master/src/utils    
    open Microsoft.FSharp.Core
    open Microsoft.FSharp.Core.OptimizedClosures

    /// Return the length of the collection.  You can also use property <c>arr.Length</c>.
    let length (arr: ResizeArray<'T>) =  arr.Count

    /// Fetch an element from the collection.  You can also use the syntax <c>arr.[idx]</c>.
    /// However this function allows for negtive index too (like Python)
    let get (arr: ResizeArray<'T>) (index: int) =  
        if index<0 then arr.[arr.Count+index]  else arr.[index]

    /// Set the value of an element in the collection. You can also use the syntax <c>arr.[idx] <- e</c>.
    /// However this function allows for negtive index too (like Python)
    let set (arr: ResizeArray<'T>) (index: int) (x:'T) =  
        if index<0 then arr.[arr.Count+index] <- x  else arr.[index] <- x

    /// Create an array whose elements are all initially the given value.
    let create  (n: int) x = 
        let r = new ResizeArray<'T>(n)
        for i = 0 to n-1 do
            r.Add x 
        r       
     
    /// Create an array by calling the given generator on each index.
    let init (n: int) (f: int -> 'T) =  
        let r = new ResizeArray<'T>(n)
        for i = 0 to n-1 do
            r.Add (f i )
        r

    /// Read a range of elements from the first array and write them into the second.
    let blit (arr1: ResizeArray<'T>) start1 (arr2: ResizeArray<'T>) start2 len =
        if start1 < 0 then invalidArg "ResizeArray: start1" "index must be positive"
        if start2 < 0 then invalidArg "ResizeArray: start2" "index must be positive"
        if len < 0 then invalidArg "ResizeArray: len" "length must be positive"
        if start1 + len > length arr1 then invalidArg "ResizeArray: start1" "(start1+len) out of range"
        if start2 + len > length arr2 then invalidArg "ResizeArray: start2" "(start2+len) out of range"
        for i = 0 to len - 1 do 
            arr2.[start2+i] <- arr1.[start1 + i]

    /// Build a new array that contains the elements of each of the given list of arrays.
    let concat (arrs: ResizeArray<'T> seq) = 
        let len = arrs |> Seq.sumBy (fun r -> r.Count)
        let r = ResizeArray(len)
        for arr in arrs do 
            r.AddRange arr
        r

    /// Build a new array that contains the elements of the first array followed by the elements of the second array.
    let append (arr1: ResizeArray<'T>) (arr2: ResizeArray<'T>) = concat [arr1; arr2]
    
    /// Build a new array that contains the given subrange specified by
    /// starting index and length.
    let sub (arr: ResizeArray<'T>) start len =
        if start < 0 then invalidArg "ResizeArray: start" "index must be positive"
        if len < 0 then invalidArg "ResizeArray: len" "length must be positive"
        if start + len > length arr then invalidArg "ResizeArray: len" "length must be positive"
        arr.GetRange(start, len)

    /// Fill a range of the collection with the given element.
    let fill (arr: ResizeArray<'T>) (start: int) (len: int) (x:'T) =
        if start < 0 then invalidArg "ResizeArray: start" "index must be positive"
        if len < 0 then invalidArg "ResizeArray: len" "length must be positive"
        if start + len > length arr then invalidArg "ResizeArray: len" "length must be positive"
        for i = start to start + len - 1 do 
            arr.[i] <- x

    /// Build a new array that contains the elements of the given array.
    let copy (arr: ResizeArray<'T>) = new ResizeArray<_>(arr)

    /// Build a list from the given array.
    let toList (arr: ResizeArray<_>) =
        let mutable res = []
        for i = length arr - 1 downto 0 do
            res <- arr.[i] :: res
        res

    /// Build an array from the given list.
    let ofList (l: _ list) =
        let len = l.Length
        let res = new ResizeArray<_>(len)
        let rec add = function
          | [] -> ()
          | e :: l -> res.Add(e); add l
        add l
        res

    /// Apply the given function to each element of the array. 
    let iter f (arr: ResizeArray<_>) = 
        for i = 0 to arr.Count - 1 do
            f arr.[i]

    /// Build a new array whose elements are the results of applying the given function
    /// to each of the elements of the array.
    let map f (arr: ResizeArray<_>) =
        let len = length arr
        let res = new ResizeArray<_>(len)
        for i = 0 to len - 1 do
            res.Add(f arr.[i])
        res

    /// Build a new array whose elements are the results of applying the given function
    /// to each of the elements of the array. The integer index passed to the
    /// function indicates the index of element being transformed.
    let mapi f (arr: ResizeArray<_>) =
        let f = FSharpFunc<_,_,_>.Adapt(f)
        let len = length arr
        let res = new ResizeArray<_>(len)
        for i = 0 to len - 1 do
            res.Add(f.Invoke(i, arr.[i]))
        res

    /// Apply the given function to each element of the array.  The integer passed to the
    /// function indicates the index of element.        
    let iteri f (arr: ResizeArray<_>) =
        let f = FSharpFunc<_,_,_>.Adapt(f)
        for i = 0 to arr.Count - 1 do
            f.Invoke(i, arr.[i])

    /// Test if any element of the array satisfies the given predicate.
    /// If the input function is <c>f</c> and the elements are <c>i0...iN</c> 
    /// then computes <c>p i0 or ... or p iN</c>.
    let exists (f: 'T -> bool) (arr: ResizeArray<'T>) =
        let len = length arr 
        let rec loop i = i < len && (f arr.[i] || loop (i+1))
        loop 0

    /// Test if all elements of the array satisfy the given predicate.
    /// If the input function is <c>f</c> and the elements are <c>i0...iN</c> and "j0...jN"
    /// then computes <c>p i0 && ... && p iN</c>.
    let forall f (arr: ResizeArray<_>) =
        let len = length arr
        let rec loop i = i >= len || (f arr.[i] && loop (i+1))
        loop 0

    let private indexNotFound i (arr:ResizeArray<_>) = raise (System.Collections.Generic.KeyNotFoundException(sprintf "The index %d was not found in the ResizeArray / List of %d items" i arr.Count) )

    /// Return the first element for which the given function returns True.
    /// Raise <c>KeyNotFoundException</c> if no such element exists.
    let find f (arr: ResizeArray<_>) = 
        let rec loop i = 
            if i >= length arr then indexNotFound i arr
            elif f arr.[i] then arr.[i]
            else loop (i+1)
        loop 0

    /// Apply the given function to successive elements, returning the first
    /// result where function returns Some(x) for some x.
    let tryPick f (arr: ResizeArray<_>) =
        let rec loop i = 
            if i >= length arr then None else
            match f arr.[i] with 
            | None -> loop(i+1)
            | res -> res
        loop 0

    /// Return the first element for which the given function returns True.
    /// Return None if no such element exists.
    let tryFind f (arr: ResizeArray<_>) = 
        let rec loop i = 
            if i >= length arr then None
            elif f arr.[i] then Some arr.[i]
            else loop (i+1)
        loop 0

    /// Apply the given function to two arrays simultaneously. The
    /// two arrays must have the same lengths, otherwise an Invalid Argument exception is
    /// raised.
    let iter2 f (arr1: ResizeArray<'T>) (arr2: ResizeArray<'b>) = 
        let f = FSharpFunc<_,_,_>.Adapt(f)
        let len1 = length arr1
        if len1 <> length arr2 then invalidArg "ResizeArray: arr2" "the arrays have different lengths"
        for i = 0 to len1 - 1 do 
            f.Invoke(arr1.[i], arr2.[i])

    /// Build a new collection whose elements are the results of applying the given function
    /// to the corresponding elements of the two collections pairwise.  The two input
    /// arrays must have the same lengths.
    let map2 f (arr1: ResizeArray<'T>) (arr2: ResizeArray<'b>) = 
        let f = FSharpFunc<_,_,_>.Adapt(f)
        let len1 = length arr1
        if len1 <> length arr2 then invalidArg "ResizeArray: arr2" "the arrays have different lengths"
        let res = new ResizeArray<_>(len1)
        for i = 0 to len1 - 1 do
            res.Add(f.Invoke(arr1.[i], arr2.[i]))
        res

    /// Apply the given function to each element of the array. Return
    /// the array comprised of the results 'x' for each element where
    /// the function returns <c>Some(x)</c>.
    let choose f (arr: ResizeArray<_>) = 
        let res = new ResizeArray<_>() 
        for i = 0 to length arr - 1 do
            match f arr.[i] with 
            | None -> ()
            | Some b -> res.Add(b)
        res

    /// Return a new collection containing only the elements of the collection
    /// for which the given predicate returns True.
    let filter f (arr: ResizeArray<_>) = 
        let res = new ResizeArray<_>() 
        for i = 0 to length arr - 1 do 
            let x = arr.[i] 
            if f x then res.Add(x)
        res

    /// Split the collection into two collections, containing the 
    /// elements for which the given predicate returns True and False
    /// respectively.
    let partition f (arr: ResizeArray<_>) = 
      let res1 = new ResizeArray<_>()
      let res2 = new ResizeArray<_>()
      for i = 0 to length arr - 1 do 
          let x = arr.[i] 
          if f x then res1.Add(x) else res2.Add(x)
      res1, res2

    /// Return a new array with the elements in reverse order.
    let rev (arr: ResizeArray<_>) = 
      let len = length arr 
      let res = new ResizeArray<_>(len)
      for i = len - 1 downto 0 do 
          res.Add(arr.[i])
      res

    /// Apply a function to each element of the array, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> then 
    /// computes <c>f i0 (...(f iN s))</c>.
    let foldBack (f : 'T -> 'State -> 'State) (arr: ResizeArray<'T>) (acc: 'State) =
        let mutable res = acc 
        let len = length arr 
        for i = len - 1 downto 0 do 
            res <- f (get arr i) res
        res

    /// Apply a function to each element of the collection, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> 
    /// then computes <c>f (... (f s i0)...) iN</c>
    let fold (f : 'State -> 'T -> 'State) (acc: 'State) (arr: ResizeArray<'T>) =
        let mutable res = acc 
        let len = length arr 
        for i = 0 to len - 1 do 
            res <- f res (get arr i)
        res

    /// Return a fixed-length array containing the elements of the input <c>ResizeArray</c>.
    let toArray (arr: ResizeArray<'T>) = arr.ToArray()

    /// Build a <c>ResizeArray</c> from the given elements.
    let ofArray (arr: 'T[]) = new ResizeArray<_>(arr)

    /// Return a view of the array as an enumerable object.
    let toSeq (arr: ResizeArray<'T>) = Seq.readonly arr

    /// Build a <c>ResizeArray</c> from the given elements.
    let ofSeq (arr: 'T seq) = new ResizeArray<_>(arr)

    /// Sort in place the elements using the given comparison function.
    let sort (f: 'T -> 'T -> int) (arr: ResizeArray<'T>) :unit = 
        arr.Sort (System.Comparison(f))

    /// Sort in place the elements using the key extractor and generic comparison on the keys.
    let sortBy (f:'T -> 'Key) (arr: ResizeArray<'T>): unit when 'Key : comparison = 
        arr.Sort (System.Comparison(fun x y -> compare (f x) (f y)))


    /// Test elements of the two arrays pairwise to see if any pair of element satisfies the given predicate.
    /// Raise ArgumentException if the arrays have different lengths.
    let exists2 f (arr1: ResizeArray<_>) (arr2: ResizeArray<_>) =
        let len1 = length arr1
        if len1 <> length arr2 then invalidArg "ResizeArray: arr2" "the arrays have different lengths"
        let rec loop i = i < len1 && (f arr1.[i] arr2.[i] || loop (i+1))
        loop 0

    /// Return the index of the first element in the array
    /// that satisfies the given predicate. Raise <c>KeyNotFoundException</c> if 
    /// none of the elements satisfy the predicate.
    let findIndex f (arr: ResizeArray<_>) =
        let rec go n = if n >= length arr then indexNotFound n arr elif f arr.[n] then n else go (n+1)
        go 0

    /// Return the index of the first element in the array
    /// that satisfies the given predicate. Raise <c>KeyNotFoundException</c> if 
    /// none of the elements satisfy the predicate.
    let findIndexi f (arr: ResizeArray<_>) =
        let rec go n = if n >= length arr then indexNotFound n arr elif f n arr.[n] then n else go (n+1)
        go 0

    let private foldSub f acc (arr: ResizeArray<_>) start fin = 
        let mutable res = acc
        for i = start to fin do
            res <- f res arr.[i] 
        res

    let private foldBackSub f (arr: ResizeArray<_>) start fin acc = 
        let mutable res = acc 
        for i = fin downto start do
            res <- f arr.[i] res
        res

    /// Apply a function to each element of the array, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> 
    /// then computes <c>f (... (f i0 i1)...) iN</c>. Raises ArgumentException if the array has size zero.
    let reduce f (arr : ResizeArray<_>) =
        let arrn = length arr
        if arrn = 0 then invalidArg "ResizeArray: arr" "the input array may not be empty"
        else foldSub f arr.[0] arr 1 (arrn - 1)

    /// Apply a function to each element of the array, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> then 
    /// computes <c>f i0 (...(f iN-1 iN))</c>. Raises ArgumentException if the array has size zero.        
    let reduceBack f (arr: ResizeArray<_>) = 
        let arrn = length arr
        if arrn = 0 then invalidArg "ResizeArray: arr" "the input array may not be empty"
        else foldBackSub f arr 0 (arrn - 2) arr.[arrn - 1]

    /// Apply a function to pairs of elements drawn from the two collections, 
    /// left-to-right, threading an accumulator argument
    /// through the computation.  The two input
    /// arrays must have the same lengths, otherwise an <c>ArgumentException</c> is
    /// raised.
    let fold2 f (acc: 'T) (arr1: ResizeArray<'T>) (arr2: ResizeArray<'U>) =
        let f = FSharpFunc<_,_,_,_>.Adapt(f)
        let mutable res = acc 
        let len = length arr1
        if len <> length arr2 then invalidArg "ResizeArray: arr2" "the arrays have different lengths"
        for i = 0 to len - 1 do
            res <- f.Invoke(res, arr1.[i],arr2.[i])
        res

    /// Apply a function to pairs of elements drawn from the two collections, right-to-left, 
    /// threading an accumulator argument through the computation.  The two input
    /// arrays must have the same lengths, otherwise an <c>ArgumentException</c> is
    /// raised.
    let foldBack2 f (arr1: ResizeArray<'T1>) (arr2: ResizeArray<'T2>) (acc: 'b) =
        let f = FSharpFunc<_,_,_,_>.Adapt(f)
        let mutable res = acc 
        let len = length arr1
        if len <> length arr2 then invalidArg "ResizeArray: arr2" "the arrays have different lengths"
        for i = len - 1 downto 0 do 
            res <- f.Invoke(arr1.[i],arr2.[i],res)
        res

    /// Test elements of the two arrays pairwise to see if all pairs of elements satisfy the given predicate.
    /// Raise <c>ArgumentException</c> if the arrays have different lengths.
    let forall2 f (arr1: ResizeArray<_>) (arr2: ResizeArray<_>) = 
        let len1 = length arr1
        if len1 <> length arr2 then invalidArg "ResizeArray: arr2" "the arrays have different lengths"
        let rec loop i = i >= len1 || (f arr1.[i] arr2.[i] && loop (i+1))
        loop 0

   /// Return True if the given array is empty, otherwise False.        
    let isEmpty (arr: ResizeArray<_>) = length (arr: ResizeArray<_>) = 0
    
    /// Apply the given function to pair of elements drawn from matching indices in two arrays,
    /// also passing the index of the elements. The two arrays must have the same lengths, 
    /// otherwise an <c>ArgumentException</c> is raised.
    let iteri2 f (arr1: ResizeArray<'T>) (arr2: ResizeArray<'b>) =
        let f = FSharpFunc<_,_,_,_>.Adapt(f)
        let len1 = length arr1
        if len1 <> length arr2 then invalidArg "ResizeArray: arr2" "the arrays have different lengths"
        for i = 0 to len1 - 1 do 
            f.Invoke(i, arr1.[i], arr2.[i])

    /// Build a new collection whose elements are the results of applying the given function
    /// to the corresponding elements of the two collections pairwise.  The two input
    /// arrays must have the same lengths, otherwise an <c>ArgumentException</c> is
    /// raised.
    let mapi2 (f: int -> 'T -> 'U -> 'c) (arr1: ResizeArray<'T>) (arr2: ResizeArray<'U>) = 
        let f = FSharpFunc<_,_,_,_>.Adapt(f)
        let len1 = length arr1
        if len1 <> length arr2 then invalidArg "ResizeArray: arr2" "the arrays have different lengths"
        init len1 (fun i -> f.Invoke(i, arr1.[i], arr2.[i]))

    let private scanBackSub f (arr: ResizeArray<'T>) start fin acc = 
        let f = FSharpFunc<_,_,_>.Adapt(f)
        let mutable state = acc
        let res = create (2+fin-start) acc
        for i = fin downto start do
            state <- f.Invoke(arr.[i], state)
            res.[i - start] <- state
        res

    let private scanSub f  acc (arr : ResizeArray<'T>) start fin = 
        let f = FSharpFunc<_,_,_>.Adapt(f)
        let mutable state = acc
        let res = create (fin-start+2) acc
        for i = start to fin do
            state <- f.Invoke(state, arr.[i])
            res.[i - start+1] <- state
        res

    /// Like <c>fold</c>, but return the intermediary and final results.
    let scan f acc (arr : ResizeArray<'T>) = 
        let arrn = length arr
        scanSub f acc arr 0 (arrn - 1)

    /// Like <c>foldBack</c>, but return both the intermediary and final results.
    let scanBack f (arr : ResizeArray<'T>) acc = 
        let arrn = length arr
        scanBackSub f arr 0 (arrn - 1) acc
    
    /// Return an array containing the given element.
    let singleton x =
        let res = new ResizeArray<_>(1)
        res.Add(x)
        res
    
    /// Return the index of the first element in the array
    /// that satisfies the given predicate.
    let tryFindIndex f (arr: ResizeArray<'T>) = 
        let rec go n = if n >= length arr then None elif f arr.[n] then Some n else go (n+1)
        go 0
    
    /// Return the index of the first element in the array
    /// that satisfies the given predicate.        
    let tryFindIndexi f (arr: ResizeArray<'T>) = 
        let rec go n = if n >= length arr then None elif f n arr.[n] then Some n else go (n+1)
        go 0
    
    /// Combine the two arrays into an array of pairs. The two arrays must have equal lengths, 
    /// otherwise an <c>ArgumentException</c> is raised..
    let zip (arr1: ResizeArray<_>) (arr2: ResizeArray<_>) = 
        let len1 = length arr1 
        if len1 <> length arr2 then invalidArg "ResizeArray: arr2" "the Lists have different lengths"
        init len1 (fun i -> arr1.[i], arr2.[i])

    /// Split an array of pairs into two arrays.
    let unzip (arr: ResizeArray<_>) = 
        let len = length arr
        let res1 = new ResizeArray<_>(len)
        let res2 = new ResizeArray<_>(len)
        for i = 0 to len - 1 do 
            let x, y = arr.[i] 
            res1.Add(x)
            res2.Add(y)
        res1, res2

    //---------------------------------------------------
    // extensions added only in FsEx:
    //----------------------------------------------------
    
    let empty() = ResizeArray<_>()

    /// Considers List cirular and move elements up or down
    /// e.g.: rotate +1 [ a, b, c, d] = [ d, a, b, c]
    /// e.g.: rotate -1 [ a, b, c, d] = [ b, c, d, a]
    let rotate k (xs: ResizeArray<_>)  =  init xs.Count (fun i -> xs.[negIdxLooped (i-k) xs.Count])


    /// Returns a ResizeArray of the index and the item. (like enumerate in Python)
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
    
    //TODO test keeping of order !

    /// Returns the smallest element of the ResizeArray.
    let min xs =     xs |> MinMax.simple (<)  // why inline? type specialisation ?

    /// Returns the biggest element of the ResizeArray.
    let max xs =     xs |> MinMax.simple (>)

    /// Returns the smallest element of the ResizeArray.
    /// Elements are compared by applying the predicate function first.
    let minBy f xs = let i = xs |> MinMax.indexByFun (<) f in xs.[i]

    /// Returns the biggest element of the ResizeArray.
    /// Elements are compared by applying the predicate function first.
    let maxBy f xs = let i = xs |> MinMax.indexByFun (>) f in xs.[i]

    /// Returns the Index of the smallest element of the ResizeArray.
    /// Elements are compared by applying the predicate function first.
    let minIndBy f xs = xs |> MinMax.indexByFun (<) f

    /// Returns the Index of the biggest element of the ResizeArray.
    /// Elements are compared by applying the predicate function first.
    let maxIndBy f xs = xs |> MinMax.indexByFun (>) f

    /// Returns the smallest two elements of the ResizeArray.
    /// If they are equal then the the order is kept
    let min2 xs =     xs |> MinMax.simple2 (<)

    /// Returns the biggest two elements of the ResizeArray.
    /// If they are equal then the the order is kept
    let max2 xs =     xs |> MinMax.simple2 (>)
    
    /// Returns the smallest two elements of the ResizeArray.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    let min2By f xs = let i,ii = xs |> MinMax.index2ByFun (<) f in xs.[i],xs.[ii]
    
    /// Returns the biggest two elements of the ResizeArray.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    let max2By f xs = let i,ii = xs |> MinMax.index2ByFun (>) f in xs.[i],xs.[ii]

    /// Returns the indices of the two smallest elements of the ResizeArray.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    let min2IndBy f xs = xs |> MinMax.index2ByFun (<) f

    /// Returns the indices of the two biggest elements of the ResizeArray.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    let max2IndBy f xs = xs |> MinMax.index2ByFun (>) f

    /// Returns the smallest three elements of the ResizeArray.
    /// If they are equal then the the order is kept
    let min3 xs =  xs |> MinMax.simple3 (<)

    /// Returns the biggest three elements of the ResizeArray.
    /// If they are equal then the the order is kept
    let max3 xs =  xs |> MinMax.simple3 (>)

    /// Returns the smallest three elements of the ResizeArray.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    let min3By f xs = let i,ii,iii = xs |> MinMax.index3ByFun (<) f in xs.[i],xs.[ii],xs.[iii]

    /// Returns the biggest three elements of the ResizeArray.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    let max3By f xs = let i,ii,iii = xs |> MinMax.index3ByFun (>) f in xs.[i],xs.[ii],xs.[iii]

    /// Returns the indices of the three smallest elements of the ResizeArray.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    let min3IndBy f xs = xs |> MinMax.index3ByFun(<) f

    /// Returns the indices of the three biggest elements of the ResizeArray.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    let max3IndBy f xs = xs |> MinMax.index3ByFun (>) f