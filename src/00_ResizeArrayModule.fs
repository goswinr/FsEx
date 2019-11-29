// taken from https://github.com/dotnet/fsharp/tree/master/src/utils

namespace FsEx

open Microsoft.FSharp.Core
open Microsoft.FSharp.Core.OptimizedClosures


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
/// Generic operations on the type System.Collections.Generic.List, which is called ResizeArray in the F# libraries.
module ResizeArray =

    /// Return the length of the collection.  You can also use property <c>arr.Length</c>.
    let length (arr: ResizeArray<'T>) =  arr.Count

    /// Fetch an element from the collection.  You can also use the syntax <c>arr.[idx]</c>.
    /// However this function allows for negtive index too (like python)
    let get (arr: ResizeArray<'T>) (index: int) =  
        if index<0 then arr.[arr.Count+index]  else arr.[index]

    /// Set the value of an element in the collection. You can also use the syntax <c>arr.[idx] <- e</c>.
    /// However this function allows for negtive index too (like python)
    let set (arr: ResizeArray<'T>) (index: int) (x:'T) =  
        if index<0 then arr.[arr.Count+index] <- x  else arr.[index] <- x

    /// Create an array whose elements are all initially the given value.
    let create  (n: int) x = 
        let r = new ResizeArray<'T>(n)
        for i = 0 to n-1 do
            r.[i] <- x 
        r       
     
    /// Create an array by calling the given generator on each index.
    let init (n: int) (f: int -> 'T) =  
        let r = new ResizeArray<'T>(n)
        for i = 0 to n-1 do
            r.[i] <- f i 
        r

    /// Read a range of elements from the first array and write them into the second.
    let blit (arr1: ResizeArray<'T>) start1 (arr2: ResizeArray<'T>) start2 len =
        if start1 < 0 then invalidArg "start1" "index must be positive"
        if start2 < 0 then invalidArg "start2" "index must be positive"
        if len < 0 then invalidArg "len" "length must be positive"
        if start1 + len > length arr1 then invalidArg "start1" "(start1+len) out of range"
        if start2 + len > length arr2 then invalidArg "start2" "(start2+len) out of range"
        for i = 0 to len - 1 do 
            arr2.[start2+i] <- arr1.[start1 + i]

    /// Build a new array that contains the elements of each of the given list of arrays.
    let concat (arrs: ResizeArray<'T> list) = new ResizeArray<_> (seq { for arr in arrs do for x in arr do yield x })

    /// Build a new array that contains the elements of the first array followed by the elements of the second array.
    let append (arr1: ResizeArray<'T>) (arr2: ResizeArray<'T>) = concat [arr1; arr2]
    
    /// Build a new array that contains the given subrange specified by
    /// starting index and length.
    let sub (arr: ResizeArray<'T>) start len =
        if start < 0 then invalidArg "start" "index must be positive"
        if len < 0 then invalidArg "len" "length must be positive"
        if start + len > length arr then invalidArg "len" "length must be positive"
        new ResizeArray<_> (seq { for i in start .. start+len-1 -> arr.[i] })

    /// Fill a range of the collection with the given element.
    let fill (arr: ResizeArray<'T>) (start: int) (len: int) (x:'T) =
        if start < 0 then invalidArg "start" "index must be positive"
        if len < 0 then invalidArg "len" "length must be positive"
        if start + len > length arr then invalidArg "len" "length must be positive"
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
        if len1 <> length arr2 then invalidArg "arr2" "the arrays have different lengths"
        for i = 0 to len1 - 1 do 
            f.Invoke(arr1.[i], arr2.[i])

    /// Build a new collection whose elements are the results of applying the given function
    /// to the corresponding elements of the two collections pairwise.  The two input
    /// arrays must have the same lengths.
    let map2 f (arr1: ResizeArray<'T>) (arr2: ResizeArray<'b>) = 
        let f = FSharpFunc<_,_,_>.Adapt(f)
        let len1 = length arr1
        if len1 <> length arr2 then invalidArg "arr2" "the arrays have different lengths"
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
        if len1 <> length arr2 then invalidArg "arr2" "the arrays have different lengths"
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
        if arrn = 0 then invalidArg "arr" "the input array may not be empty"
        else foldSub f arr.[0] arr 1 (arrn - 1)

    /// Apply a function to each element of the array, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> then 
    /// computes <c>f i0 (...(f iN-1 iN))</c>. Raises ArgumentException if the array has size zero.        
    let reduceBack f (arr: ResizeArray<_>) = 
        let arrn = length arr
        if arrn = 0 then invalidArg "arr" "the input array may not be empty"
        else foldBackSub f arr 0 (arrn - 2) arr.[arrn - 1]

    /// Apply a function to pairs of elements drawn from the two collections, 
    /// left-to-right, threading an accumulator argument
    /// through the computation.  The two input
    /// arrays must have the same lengths, otherwise an <c>ArgumentException</c> is
    /// raised.
    let fold2 f (acc: 'T) (arr1: ResizeArray<'T1>) (arr2: ResizeArray<'T2>) =
        let f = FSharpFunc<_,_,_,_>.Adapt(f)
        let mutable res = acc 
        let len = length arr1
        if len <> length arr2 then invalidArg "arr2" "the arrays have different lengths"
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
        if len <> length arr2 then invalidArg "arr2" "the arrays have different lengths"
        for i = len - 1 downto 0 do 
            res <- f.Invoke(arr1.[i],arr2.[i],res)
        res

    /// Test elements of the two arrays pairwise to see if all pairs of elements satisfy the given predicate.
    /// Raise <c>ArgumentException</c> if the arrays have different lengths.
    let forall2 f (arr1: ResizeArray<_>) (arr2: ResizeArray<_>) = 
        let len1 = length arr1
        if len1 <> length arr2 then invalidArg "arr2" "the arrays have different lengths"
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
        if len1 <> length arr2 then invalidArg "arr2" "the arrays have different lengths"
        for i = 0 to len1 - 1 do 
            f.Invoke(i, arr1.[i], arr2.[i])

    /// Build a new collection whose elements are the results of applying the given function
    /// to the corresponding elements of the two collections pairwise.  The two input
    /// arrays must have the same lengths, otherwise an <c>ArgumentException</c> is
    /// raised.
    let mapi2 (f: int -> 'T -> 'b -> 'c) (arr1: ResizeArray<'T>) (arr2: ResizeArray<'b>) = 
        let f = FSharpFunc<_,_,_,_>.Adapt(f)
        let len1 = length arr1
        if len1 <> length arr2 then invalidArg "arr2" "the arrays have different lengths"
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
        if len1 <> length arr2 then invalidArg "arr2" "the Lists have different lengths"
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
    // extensions added  by Goswin:
    //----------------------------------------------------
    
    /// shift items poistion  towards the end of ResizeArray. refilling at start from end.
    let rotate k (xs: ResizeArray<_>)  =  init xs.Count (fun i -> xs.[if i-k < 0 then xs.Count+i-k  else i-k])