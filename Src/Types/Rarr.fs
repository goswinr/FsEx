namespace FsEx

open System.Collections.Generic
open System

// Of course it would be simpler to just override the .Item method on System.Collections.Generic.List<'T> but unfortunatly it is sealed

/// A mutable list like Collections.Generic.List<'T> but with nicer error messages on bad indices.
/// It's just a very thin wrapper over a System.Collections.Generic.List<'T>
/// and has all its members and interfaces implemented.
/// The name Rarr is derived from of the F# type ResizeArray
type Rarr<'T> private (xs:List<'T>) =     
    
    static member inline internal get index (xs:List<'T>) = 
        if index >= xs.Count then ArgumentOutOfRangeException.Raise "Cant get index %d from Rarr of %d items: %A" index xs.Count xs
        xs.[index]

    static member inline internal set index value (xs:List<'T>) = 
        if index >= xs.Count then ArgumentOutOfRangeException.Raise "Cant set index %d to %A in Rarr of %d items: %A " index value xs.Count  xs
        xs.[index] <- value
    
    /// Constructs a new Rarr by using the supplied List<'T>  directly, without any copying of items
    static member CreateDirectly (xs:List<'T> ) = 
        if isNull xs then ArgumentNullException.Raise "List in Rarr.CreateDirectly is null"
        Rarr(xs)

    /// Constructs a new Rarr by copying each item from the IEnumerable<'T>
    static member CreateFromSeq (xs:seq<'T> ) = 
        if isNull xs then ArgumentNullException.Raise "Seq in Rarr.CreateFromSeq is null"
        Rarr(xs)

    /// Constructs a new Rarr. 
    /// A Rarr is a mutable list like Collections.Generic.List<'T> but with nicer error messages on bad indices.
    /// It's just a very thin wrapper over a System.Collections.Generic.List<'T> 
    /// and has all its members and interfaces implemented.
    /// The name Rarr is derived from of the F# type ResizeArray
    /// The list is initially empty and has a capacity
    /// of zero. Upon adding the first element to the list the capacity is
    /// increased to 16, and then increased in multiples of two as required.
    new () = 
        Rarr(List())  

    /// Constructs a new Rarr with a given initial capacity. 
    /// A Rarr is a mutable list like Collections.Generic.List<'T> but with nicer error messages on bad indices.
    /// It's just a very thin wrapper over a System.Collections.Generic.List<'T>
    /// and has all its members and interfaces implemented.
    /// The name Rarr is derived from of the F# type ResizeArray. The list is
    /// initially empty, but will have room for the given number of elements
    /// before any reallocations are required.
    new (capacity : int) = 
        Rarr(List(capacity))   
    
    /// Constructs a new Rarr, copying the contents of the given collection.
    /// A Rarr is a mutable list like Collections.Generic.List<'T> but with nicer error messages on bad indices.
    /// It's just a very thin wrapper over a System.Collections.Generic.List<'T>
    /// and has all its members and interfaces implemented.
    /// The name Rarr is derived from of the F# type ResizeArray.
    /// The size and capacity of the new list will both be equal to the size of the
    /// given collection.
    new (collection : IEnumerable<'T>)  = 
        if isNull collection then ArgumentNullException.Raise "IEnumerable in new Rarr(collection) constructor is null"
        Rarr(List(collection))

    /// Access the underlying Collections.Generic.List<'T>
    /// ATTENTION! this is not even a shallow copy, mutating it will also change this Instance of Rarr!
    member _.List:List<'T> = xs
    
    /// Access the underlying Collections.Generic.List<'T>
    /// ATTENTION! This is not even a shallow copy, mutating it will also change this Instance of Rarr!
    member _.ResizeArray:ResizeArray<'T> = xs


    /// Gets the index of the last item in the Rarr.
    /// equal to this.Count - 1  
    member _.LastIndex = 
        if xs.Count = 0 then IndexOutOfRangeException.Raise "Rarr.LastIndex: Can not get LastIndex of empty List"
        xs.Count - 1

    /// Gets the last item in the Rarr.
    /// equal to this.[this.Count - 1]
    member _.Last = 
        if xs.Count = 0 then IndexOutOfRangeException.Raise "Rarr.Last: Can not get Last item of empty List"
        xs.[xs.Count - 1]
    
    /// Gets the second last item in the Rarr.
    /// equal to this.[this.Count - 2]
    member this.SecondLast = 
        if xs.Count < 2 then  IndexOutOfRangeException.Raise "Rarr.SecondLast: Can not get SecondLast item of %s"   (NiceString.toNiceStringFull this)
        xs.[xs.Count - 2]

    /// Gets the third last item in the Rarr.
    /// equal to this.[this.Count - 3]
    member this.ThirdLast = 
        if xs.Count < 3 then  IndexOutOfRangeException.Raise "Rarr.ThirdLast: Can not get ThirdLast item of %s"  (NiceString.toNiceStringFull this)
        xs.[xs.Count - 3]
                
    /// Gets the first item in the Rarr.
    /// equal to this.[0]
    member _.First = 
        if xs.Count = 0 then IndexOutOfRangeException.Raise "Rarr.First: Can not get First item of empty Rarr List"
        xs.[0]

    /// Gets the second item in the Rarr.
    /// equal to this.[1]
    member this.Second = 
        if xs.Count < 2 then IndexOutOfRangeException.Raise  "Rarr.Second: Can not get Second item of %s"   (NiceString.toNiceStringFull this)
        xs.[1]

    /// Gets the third item in the Rarr.
    /// equal to this.[2]
    member this.Third = 
        if xs.Count < 3 then IndexOutOfRangeException.Raise "Rarr.Third: Can not get Third item of %s"  (NiceString.toNiceStringFull this)
        xs.[2]

    /// Checks if this.Count = 0 
    member _.IsEmpty = 
        xs.Count = 0 
    
    /// Checks if this.Count > 0 
    member _.IsNotEmpty = 
        xs.Count > 0 
    
    /// Insert an item at the beginning of the list = index 0, 
    /// (moving all other items up by one index)
    member _.Insert0 x  = 
        xs.Insert(0,x)
   
    /// Gets an item at index 
    /// (use Rarr.GetNeg(i) member if you want to use negative indices too)
    member _.Get index = Rarr.get index xs
        
    /// Sets an item at index 
    /// (use Rarr.SetNeg(i) member if you want to use negative indices too)
    member _.Set index value = Rarr.set index value xs

    /// Gets an item in the Rarr by index.
    /// Allows for negtive index too ( -1 is last item,  like Python)
    /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
    member _.GetNeg index = 
        let len = xs.Count
        let ii =  if index < 0 then len + index else index
        if ii<0 || ii >= len then ArgumentOutOfRangeException.Raise "Rarr.GetNeg: Can't get index %d from Rarr of %d items: %A" index xs.Count xs
        xs.[ii]        

    /// Sets an item in the Rarr by index.
    /// Allows for negtive index too ( -1 is last item,  like Python)
    /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
    member _.SetNeg index value = 
        let len = xs.Count
        let ii =  if index < 0 then len + index else index
        if ii<0 || ii >= len then ArgumentOutOfRangeException.Raise "Rarr.SetNeg: Can't set index %d to %A rom Rarr of %d items: %A" index value xs.Count xs
        xs.[ii] <- value        
   
    /// Any index will return a value.
    /// Rarr is treated as an endless loop in positive and negative direction   
    member _.GetLooped index = 
        let len = xs.Count
        if len=0 then ArgumentOutOfRangeException.Raise "Rarr.GetLooped: Can't get index %d from Rarr of 0 items" index
        let t = index % len
        let ii = if t >= 0 then t  else t + len 
        xs.[ii]              

    /// Any index will set a value.
    /// Rarr is treated as an endless loop in positive and negative direction   
    member _.SetLooped index value  = 
        let len = xs.Count
        if len=0 then ArgumentOutOfRangeException.Raise "Rarr.SetLooped: Can't Set index %d to %A in Rarr of 0 items" index value
        let t = index % len
        let ii = if t >= 0 then t  else t + len 
        xs.[ii] <- value

    /// Get and remove last item from Rarr
    member _.Pop()  =
        if xs.Count=0 then ArgumentOutOfRangeException.Raise "Can't pop from empty Rarr"
        let i = xs.Count - 1        
        let v = xs.[i]
        xs.RemoveAt(i)
        v

    /// Creates a shallow copy of the list
    // (for a Rarr of structs this is like a deep copy)
    member _.Clone() = Rarr(xs)
        
    
    /// Defines F# slicing notation operator use including negative indices. ( -1 is last item, like Python)
    /// The resulting Rarr includes the end index.
    /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)    
    member _.GetSlice(startIdx, endIdx) =    //.GetSlice maps onto slicing operator .[1..3]
        let count = xs.Count
        let st  = match startIdx with None -> 0        | Some i -> if i<0 then count+i      else i
        let len = match endIdx   with None -> count-st | Some i -> if i<0 then count+i-st+1 else i-st+1
    
        if st < 0 || st > count-1 then 
            IndexOutOfRangeException.Raise "Rarr.[ a.. b] (GetSlice): Start index %d is out of range. Allowed values are -%d upto %d for List of %d items" startIdx.Value count (count-1) count
                
        if st+len > count then 
            IndexOutOfRangeException.Raise "Rarr.[ a.. b] (GetSlice): End index %d is out of range. Allowed values are -%d upto %d for List of %d items" endIdx.Value count (count-1) count
                        
        if len < 0 then
            let en =  match endIdx  with None -> count-1 | Some i -> if i<0 then count+i else i
            IndexOutOfRangeException.Raise "Rrr.[ a.. b] (GetSlice): Start index '%A' (= %d) is bigger than end index '%A'(= %d) for List of %d items" startIdx st endIdx en  count
                        
        xs.GetRange(st, len) 
  
    
    /// A property like the ToString() method, 
    /// But with richer formationg for collections
    member this.ToNiceString = NiceString.toNiceString this


    //--------------------------------------------------------------------------------------------------------------------------
    //---------------------------- member of  System.Collections.Generic.List --------------------------------------------------
    //-------------------------https://referencesource.microsoft.com/#mscorlib/system/collections/generic/list.cs---------------
    

    /// Sets or Gets the element at the given index. With nice error messages on bad indices.
    member _.Item // overriding this is the main purpose off all of this class
        with get index       = Rarr.get index xs          
        and  set index value = Rarr.set index value xs            
    
    /// Gets and sets the capacity of this list.  The capacity is the size of
    /// the internal array used to hold items.  When set, the internal 
    /// array of the list is reallocated to the given capacity.
    /// Value must be same or bigger than Rarr.Count
    member _.Capacity   
        with get () = xs.Capacity
        and set c   = xs.Capacity <- c
    
    /// Read-only property describing how many elements are in the List.
    member _.Count =  xs.Count
    
    /// Adds the given object to the end of this list. The size of the list is
    /// increased by one. If required, the capacity of the list is doubled
    /// before adding the new element.
    member _.Add(item : 'T) =                                                                xs.Add item
    
    // Adds the elements of the given collection to the end of this list. If
    // required, the capacity of the list is increased to twice the previous
    // capacity or the new size, whichever is larger.
    member _.AddRange(collection : IEnumerable<'T>) =                                        xs.AddRange collection
    
    /// Returns a read-only System.Collections.ObjectModel.ReadOnlyCollection 
    /// wrapper over the current collection.
    member _.AsReadOnly() =                                                                  xs.AsReadOnly
    
    /// Searches a section of the list for a given element using a binary search
    /// algorithm. Elements of the list are compared to the search value using
    /// the given IComparer interface. If comparer is null, elements of
    /// the list are compared to the search value using the IComparable
    /// interface, which in that case must be implemented by all elements of the
    /// list and the given search value. This method assumes that the given
    /// section of the list is already sorted; if this is not the case, the
    /// result will be incorrect.
    /// The method returns the index of the given value in the list. If the
    /// list does not contain the given value, the method returns a negative
    /// integer. The bitwise complement operator (~) can be applied to a
    /// negative result to produce the index of the first element (if any) that
    /// is larger than the given search value. This is also the index at which
    /// the search value should be inserted into the list in order for the list
    /// to remain sorted.
    /// The method uses the Array.BinarySearch method to perform thesearch.
    member _.BinarySearch(index : int, count : int, item : 'T, comparer : IComparer<'T>) =   xs.BinarySearch (index, count , item , comparer )       
    
    /// Searches a section of the list for a given element using a binary search
    /// algorithm. Elements of the list are compared to the search value using
    /// the given IComparer interface. If comparer is null, elements of
    /// the list are compared to the search value using the IComparable
    /// interface, which in that case must be implemented by all elements of the
    /// list and the given search value. This method assumes that the given
    /// section of the list is already sorted; if this is not the case, the
    /// result will be incorrect.
    /// The method returns the index of the given value in the list. If the
    /// list does not contain the given value, the method returns a negative
    /// integer. The bitwise complement operator (~) can be applied to a
    /// negative result to produce the index of the first element (if any) that
    /// is larger than the given search value. This is also the index at which
    /// the search value should be inserted into the list in order for the list
    /// to remain sorted.
    /// The method uses the Array.BinarySearch method to perform thesearch.
    member _.BinarySearch(item : 'T) =                                                       xs.BinarySearch item 
    
    /// Searches a section of the list for a given element using a binary search
    /// algorithm. Elements of the list are compared to the search value using
    /// the given IComparer interface. If comparer is null, elements of
    /// the list are compared to the search value using the IComparable
    /// interface, which in that case must be implemented by all elements of the
    /// list and the given search value. This method assumes that the given
    /// section of the list is already sorted; if this is not the case, the
    /// result will be incorrect.
    /// The method returns the index of the given value in the list. If the
    /// list does not contain the given value, the method returns a negative
    /// integer. The bitwise complement operator (~) can be applied to a
    /// negative result to produce the index of the first element (if any) that
    /// is larger than the given search value. This is also the index at which
    /// the search value should be inserted into the list in order for the list
    /// to remain sorted.
    /// The method uses the Array.BinarySearch method to perform thesearch.
    member _.BinarySearch(item : 'T, comparer : IComparer<'T>) =                             xs.BinarySearch(item , comparer) 
    
    /// Clears the contents of List.
    member _.Clear() =                                                                       xs.Clear() 

    /// Contains returns true if the specified element is in the List.
    /// It does a linear, O(n) search.  Equality is determined by calling item.Equals().
    member _.Contains(item : 'T) =                                                           xs.Contains(item) 
    
    ///<summary>Converts the elements in the current List to another type, and returns 
    /// a list containing the converted elements.</summary>
    ///<param name="converter">A System.Converter delegate that converts each element from 
    /// one type to another type.</param>
    ///<typeparam name="TOutput">The type of the elements of the target array.</typeparam>
    ///<returns>A List of the target type containing the converted elements from 
    /// the current List.</returns>
    member _.ConvertAll<'TOutput>(converter : Converter<'T, 'TOutput>) =                     xs.ConvertAll(converter) |> Rarr
    
    /// Copies a section of this list to the given array at the given index.
    /// The method uses the Array.Copy method to copy the elements.
    member _.CopyTo(array : 'T[]) =                                                          xs.CopyTo(array) 
    
    ///<summary>Copies the elements of the Rarr to an Array, 
    /// starting at a particular Array index.</summary>
    ///<param name="array">The one-dimensional Array that is the destination of the 
    /// elements copied from the List The Array must have zero-based indexing.</param>
    ///<param name="arrayIndex">The zero-based index in the Array at 
    /// which copying begins.</param>
    member _.CopyTo(array : 'T[], arrayIndex : int) =                                        xs.CopyTo(array , arrayIndex) 
    
    member _.CopyTo(index : int, array : 'T[], arrayIndex : int, count : int) =              xs.CopyTo(index , array , arrayIndex , count)    // TODO add XML doc str
    member _.Exists (matchValue : Predicate<'T>) =                                           xs.Exists (matchValue) 
    member _.Find   (matchValue : Predicate<'T>) =                                           xs.Find   (matchValue) 
    member _.FindAll(matchValue : Predicate<'T>) =                                           xs.FindAll(matchValue) |> Rarr
    member _.FindIndex(matchValue : Predicate<'T>) =                                         xs.FindIndex(matchValue)
    member _.FindIndex(startIndex : int, matchValue : Predicate<'T>) =                       xs.FindIndex(startIndex , matchValue)
    member _.FindIndex(startIndex : int, count : int, matchValue : Predicate<'T>) =          xs.FindIndex(startIndex , count , matchValue) 
    member _.FindLast(matchValue : Predicate<'T>) =                                          xs.FindLast(matchValue) 
    member _.FindLastIndex(matchValue : Predicate<'T>) =                                     xs.FindLastIndex(matchValue) 
    member _.FindLastIndex(startIndex : int, matchValue : Predicate<'T>) =                   xs.FindLastIndex(startIndex , matchValue) 
    member _.FindLastIndex(startIndex : int, count : int, matchValue : Predicate<'T>) =      xs.FindLastIndex(startIndex , count , matchValue) 
    member _.ForEach(action : Action<'T>) =                                                  xs.ForEach(action) 
    
    /// Returns an enumerator for this list with the given
    /// permission for removal of elements. If modifications made to the list 
    /// while an enumeration is in progress, the MoveNext and 
    /// GetObject methods of the enumerator will throw an exception.
    member _.GetEnumerator() =                                                               xs.GetEnumerator() 
    
    /// <summary>Creates a shallow copy of a range of elements in the source List.</summary>
    /// <param name="index">The zero-based List index at which the range starts.</param>
    /// <param name="count">The number of elements in the range.</param>
    /// <returns>A shallow copy of a range of elements in the source List.</returns>
    member _.GetRange(index : int, count : int) =                                            xs.GetRange(index , count) |> Rarr
    
    /// Returns the index of the first occurrence of a given value in a range of
    /// this list. The list is searched forwards from beginning to end.
    /// The elements of the list are compared to the given value using the
    /// Object.Equals method.
    /// This method uses the Array.IndexOf method to perform the search.
    member _.IndexOf(item : 'T) =                                                            xs.IndexOf(item) 

    /// Returns the index of the first occurrence of a given value in a range of
    /// this list. The list is searched forwards, starting at index
    /// index and ending at count number of elements. The
    /// elements of the list are compared to the given value using the
    /// Object.Equals method.
    /// This method uses the Array.IndexOf method to perform the search
    member _.IndexOf(item : 'T, index : int) =                                               xs.IndexOf(item , index) 

    // Returns the index of the first occurrence of a given value in a range of
    // this list. The list is searched forwards, starting at index
    // index and upto count number of elements. The
    // elements of the list are compared to the given value using the
    // Object.Equals method.
    // This method uses the Array.IndexOf method to perform the  search.
    member _.IndexOf(item : 'T, index : int, count : int) =                                  xs.IndexOf(item , index , count) 
    
    /// Inserts an element into this list at a given index. The size of the list
    /// is increased by one. If required, the capacity of the list is doubled
    /// before inserting the new element.
    member _.Insert(index : int, item : 'T) =                                                xs.Insert(index , item) 
    
    /// Inserts the elements of the given collection at a given index. If
    /// required, the capacity of the list is increased to twice the previous
    /// capacity or the new size, whichever is larger.  Ranges may be added
    /// to the end of the list by setting index to the List's size.
    member _.InsertRange(index : int, collection : IEnumerable<'T>) =                        xs.InsertRange(index , collection) 
    
    /// Returns the index of the last occurrence of a given value in a range of
    /// this list. The list is searched backwards, starting at the end 
    /// and ending at the first element in the list. The elements of the list 
    /// are compared to the given value using the Object.Equals method.    
    /// This method uses the Array.LastIndexOf method to perform the search.
    member _.LastIndexOf(item : 'T) =                                                        xs.LastIndexOf(item) 

    /// Returns the index of the last occurrence of a given value in a range of
    /// this list. The list is searched backwards, starting at index
    /// index and ending at the first element in the list. The 
    /// elements of the list are compared to the given value using the 
    /// Object.Equals method.
    /// This method uses the Array.LastIndexOf method to perform the search.
    member _.LastIndexOf(item : 'T, index : int) =                                           xs.LastIndexOf(item , index) 

    /// Returns the index of the last occurrence of a given value in a range of
    /// this list. The list is searched backwards, starting at index
    /// index and upto count elements. The elements of
    /// the list are compared to the given value using the Object.Equals
    /// method.
    /// This method uses the Array.LastIndexOf method to perform the search.
    member _.LastIndexOf(item : 'T, index : int, count : int) =                              xs.LastIndexOf(item , index , count) 
    
    /// Removes the element at the given index. The size of the list is
    /// decreased by one.
    member _.Remove(item : 'T) =                                                            xs.Remove(item ) 
    
    /// This method removes all items which matches the predicate.
    /// The complexity is O(n).
    member _.RemoveAll(matchValue : Predicate<'T>) =                                         xs.RemoveAll(matchValue) 

    /// Removes the element at the given index. The size of the list is
    /// decreased by one.
    member _.RemoveAt(index : int) =                                                         xs.RemoveAt(index) 

    member _.RemoveRange(index : int, count : int) =                                         xs.RemoveRange(index , count) 
    
    // Reverses the elements in this list.
    member _.Reverse() =                                                                     xs.Reverse() 
    
    // Reverses the elements in a range of this list. Following a call to this
    // method, an element in the range given by index and count
    // which was previously located at index i will now be located at
    // index index + (index + count - i - 1).    // 
    // This method uses the Array.Reverse method to reverse the elements.
    member _.Reverse(index : int, count : int) =                                             xs.Reverse(index , count) 
    
    // Sorts the elements in this list.  Uses the default comparer and  Array.Sort.
    member _.Sort() =                                                                        xs.Sort() 
    
    // Sorts the elements in this list.  Uses Array.Sort with the
    // provided comparer.
    member _.Sort(comparer : IComparer<'T>) =                                                xs.Sort(comparer) 

    // Sorts the elements in a section of this list. The sort compares the
    // elements to each other using the given IComparer interface. If
    // comparer is null, the elements are compared to each other using
    // the IComparable interface, which in that case must be implemented by all
    // elements of the list.
    // This method uses the Array.Sort method to sort the elements.
    member _.Sort(index : int, count : int, comparer : IComparer<'T>) =                      xs.Sort(index , count , comparer) 
    
    // Sorts the elements in this list.  Uses Array.Sort with the
    // provided comparison.
    member _.Sort(comparison : Comparison<'T>) =                                             xs.Sort(comparison) 
    
    // ToArray returns a new Object array containing the contents of the List.
    // This requires copying the List, which is an O(n) operation.
    member _.ToArray() =                                                                     xs.ToArray() 
    
    // Sets the capacity of this list to the size of the list. This method can
    // be used to minimize a list's memory overhead once it is known that no
    // new elements will be added to the list. To completely clear a list and
    // release all memory referenced by the list, execute the following
    // statements:
    // list.Clear();
    // list.TrimExcess();
    member _.TrimExcess() =                                                                  xs.TrimExcess() 
    
    ///Determines whether every element in the List 
    ///matches the conditions defined by the specified predicate.
    member _.TrueForAll(matchValue : Predicate<'T>) =                                        xs.TrueForAll(matchValue) 
    

    
    //---------------------------------------Interfaces of  System.Collections.Generic.List-------------------------------------
    
    // TODO add XML doc str

    interface IEnumerable<'T> with
        member _.GetEnumerator() = (xs:>IEnumerable<'T>).GetEnumerator()

    interface Collections.IEnumerable with // Non generic needed too ? 
        member _.GetEnumerator() = xs.GetEnumerator():> System.Collections.IEnumerator
    
    interface Collections.ICollection with // Non generic needed too ? 
        member _.Count = xs.Count        
        member _.CopyTo(arr, i) = (xs:>Collections.ICollection).CopyTo(arr, i)
        member _.IsSynchronized = (xs:>Collections.ICollection).IsSynchronized
        member _.SyncRoot = (xs:>Collections.ICollection).SyncRoot
    
    interface ICollection<'T> with 
        member _.Add(x) = (xs:>ICollection<'T>).Add(x)
        member _.Clear() = xs.Clear()
        member _.Remove x = (xs:>ICollection<'T>).Remove x
        member _.Contains x = (xs:>ICollection<'T>).Contains x
        member _.CopyTo(arr, i) = (xs:>ICollection<'T>).CopyTo(arr, i)
        member _.IsReadOnly = false
        member _.Count = xs.Count

    interface IReadOnlyCollection<'T> with 
        member _.Count = xs.Count

    interface IList<'T> with         
        member _.IndexOf(item) =            xs.IndexOf(item)
        member _.Insert(index,item) =       xs.Insert(index,item)
        member _.RemoveAt(index) =          xs.RemoveAt(index)
        member _.Item
            with get index = Rarr.get index xs          
            and  set index value = Rarr.set index value xs     
    
    interface IReadOnlyList<'T> with         
        member _.Item
            with get index = Rarr.get index xs  
            
    interface Collections.IList with // Non generic needed too ? 
        member _.Add(x) = (xs:>Collections.IList).Add(x)
        member _.IndexOf(item) =            (xs:>Collections.IList).IndexOf(item)
        member _.Insert(index,item) =       (xs:>Collections.IList).Insert(index,item)
        member _.RemoveAt(index) =          (xs:>Collections.IList).RemoveAt(index)
        member _.Item
            with get index = 
                if index >= xs.Count then ArgumentOutOfRangeException.Raise "Cant get index %d from Rarr non Generic IList of %d items: %A" index xs.Count xs
                (xs:>Collections.IList).[index]
            and set index value = 
                if index >= xs.Count then ArgumentOutOfRangeException.Raise "Cant set index %d to %A in  Rarr non Generic IList of %d items: %A " index value xs.Count  xs
                (xs:>Collections.IList).[index] <- value
        
        member _.Remove x =   (xs:>Collections.IList).Remove x
        member _.Clear() =    (xs:>Collections.IList).Clear()
        member _.Contains x = (xs:>Collections.IList).Contains x
        member _.IsReadOnly = false
        member _.IsFixedSize = false