namespace FsEx

open System.Collections.Generic
open System
open System.Collections.Generic

// Of course it would be simpler to just override the .Item method on System.Collections.Generic.List<'T> but unfortunatly it is sealed

/// A mutable list like Collections.Generic.List<'T> but with nicer error messages on bad indices.
/// It's just a very thin wrapper over a System.Collections.Generic.List<'T> 
/// and has all its members and interfaces implemented.
/// The name Rarr is derived from of the F# type ResizeArray
type Rarr<'T>(xs:List<'T>) = 
    
    static member inline get index (xs:List<'T>) = 
        if index >= xs.Count then ArgumentOutOfRangeException.Raise "Cant get index %d from Rarr List of %d items: %A" index xs.Count xs
        xs.[index]

    static member inline set index value (xs:List<'T>) = 
        if index >= xs.Count then ArgumentOutOfRangeException.Raise "Cant set index %d to %A in  Rarr List of %d items: %A " index value xs.Count  xs
        xs.[index] <- value
    
    /// Access the underlying Collections.Generic.List<'T>
    /// ATTENTION! this is not even a shallow copy, mutating it will also change this Instance of Rarr!
    member inline _.List:List<'T> = xs
    
    /// Access the underlying Collections.Generic.List<'T>
    /// ATTENTION! This is not even a shallow copy, mutating it will also change this Instance of Rarr!
    member inline _.ResizeArray:ResizeArray<'T> = xs



    /// Gets the index of the last item in the ResizeArray.
    /// equal to this.Count - 1  
    member inline _.LastIndex = 
        if xs.Count = 0 then IndexOutOfRangeException.Raise "Rarr.LastIndex: Can not get LastIndex of empty List"
        xs.Count - 1

    /// Gets the last item in the ResizeArray.
    /// equal to this.[this.Count - 1]
    member inline this.Last = 
        if xs.Count = 0 then IndexOutOfRangeException.Raise "Rarr.Last: Can not get Last item of empty List"
        xs.[xs.Count - 1]
    
    /// Gets the second last item in the ResizeArray.
    /// equal to this.[this.Count - 2]
    member inline this.SecondLast = 
        if xs.Count < 2 then 
            IndexOutOfRangeException.Raise "Rarr.SecondLast: Can not get SecondLast item of %s"   (NiceString.toNiceStringFull this)
        xs.[xs.Count - 2]

    /// Gets the third last item in the ResizeArray.
    /// equal to this.[this.Count - 3]
    member inline this.ThirdLast = 
        if xs.Count < 3 then 
            IndexOutOfRangeException.Raise "Rarr.ThirdLast: Can not get ThirdLast item of %s"  (NiceString.toNiceStringFull this)
        xs.[xs.Count - 3]
                
    /// Gets the first item in the ResizeArray.
    /// equal to this.[0]
    member inline _.First = 
        if xs.Count = 0 then IndexOutOfRangeException.Raise "Rarr.First: Can not get First item of empty Rarr List"
        xs.[0]

    /// Gets the second item in the ResizeArray.
    /// equal to this.[1]
    member inline this.Second = 
        if xs.Count < 2 then 
            IndexOutOfRangeException.Raise  "Rarr.Second: Can not get Second item of %s"   (NiceString.toNiceStringFull this)
        xs.[1]

    /// Gets the third item in the ResizeArray.
    /// equal to this.[2]
    member inline this.Third = 
        if xs.Count < 3 then 
            IndexOutOfRangeException.Raise "Rarr.Third: Can not get Third item of %s"  (NiceString.toNiceStringFull this)
        xs.[2]

    /// Checks if this.Count = 0 
    member inline _.IsEmpty = 
        xs.Count = 0 
    
    /// Checks if this.Count > 0 
    member inline _.IsNotEmpty = 
        xs.Count > 0 
    
    /// Insert an item at the beginning of the list = index 0, 
    /// (moving all other items up by one index)
    member inline _.Insert0 x  = 
        xs.Insert(0,x)

    /// Gets an item in the ResizeArray by index.
    /// Allows for negtive index too ( -1 is last item,  like Python)
    /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
    member inline _.GetItem index = 
        let i = negIdx index xs.Count
        xs.[i]    
    
    /// Sets an item in the ResizeArray by index.
    /// Allows for negtive index too ( -1 is last item, like Python)
    /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
    member inline _.SetItem index value = 
        let i = negIdx index xs.Count
        xs.[i] <- value     
    
    /// Get and remove last item from ResizeArray
    member inline _.Pop()  =
        if xs.Count=0 then ArgumentOutOfRangeException.Raise "Can't pop from empty Rarr"
        let i = xs.Count - 1        
        let v = xs.[i]
        xs.RemoveAt(i)
        v
    
    /// Defines F# slicing notation operator use including negative indices. ( -1 is last item, like Python)
    /// The resulting ResizeArray includes the end index.
    /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)    
    member inline _.GetSlice(startIdx, endIdx) =    // maps onto slicing operator .[1..3]
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
     
     
     /// Allows for negative indices too. ( -1 is last item, like Python)
     /// This method only exist to be consisten with other collections extensions in FsEx. like array. 
     /// You might prefer to use the F# slicing notation on ResizeArrays.
     /// For ResizeArrays this behaves the same way as the F# slicing notation defind in FsEx too, 
     /// (Only arrays need to use this method if they want to use negative indices since the GetSlice operators cant be overwritten.) 
     /// The resulting array includes the end index.
     /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
     member inline this.Slice(startIdx, endIdx) = this.GetSlice(startIdx, endIdx)    
    
    /// A property like the ToString() method, 
    /// But with richer formationg for collections
    member this.ToNiceString = NiceString.toNiceString this



    //--------------------------------------------------------------------------------------------------------------------------
    //---------------------------- member of  System.Collections.Generic.List --------------------------------------------------
    //-------------------------https://referencesource.microsoft.com/#mscorlib/system/collections/generic/list.cs---------------
    
    // Constructs a List. The list is initially empty and has a capacity
    // of zero. Upon adding the first element to the list the capacity is
    // increased to 16, and then increased in multiples of two as required.
    new ()                               = Rarr(List())  

    // Constructs a List with a given initial capacity. The list is
    // initially empty, but will have room for the given number of elements
    // before any reallocations are required.
    new (capacity : int)                 = Rarr(List(capacity))   
    
    // Constructs a List, copying the contents of the given collection. The
    // size and capacity of the new list will both be equal to the size of the
    // given collection.
    new (collection : IEnumerable<'T>)   = Rarr(List(collection))


    // TODO add XML doc str

    member inline _.Item // ovveriding this is the main purpose off all of this class
        with get index       = Rarr.get index xs          
        and  set index value = Rarr.set index value xs            

    member inline _.Capacity =                                                                      xs.Capacity
    member inline _.Count =                                                                         xs.Count
    member inline _.Add(item : 'T) =                                                                xs.Add item
    member inline _.AddRange(collection : IEnumerable<'T>) =                                        xs.AddRange collection
    member inline _.AsReadOnly() =                                                                  xs.AsReadOnly
    member inline _.BinarySearch(index : int, count : int, item : 'T, comparer : IComparer<'T>) =   xs.BinarySearch (index, count , item , comparer )       
    member inline _.BinarySearch(item : 'T) =                                                       xs.BinarySearch item 
    member inline _.BinarySearch(item : 'T, comparer : IComparer<'T>) =                             xs.BinarySearch(item , comparer) 
    member inline _.Clear() =                                                                       xs.Clear() 
    member inline _.Contains(item : 'T) =                                                           xs.Contains(item) 
    member inline _.ConvertAll<'TOutput>(converter : Converter<'T, 'TOutput>) =                     xs.ConvertAll(converter ) 
    member inline _.CopyTo(array : 'T[]) =                                                          xs.CopyTo(array) 
    member inline _.CopyTo(array : 'T[], arrayIndex : int) =                                        xs.CopyTo(array , arrayIndex) 
    member inline _.CopyTo(index : int, array : 'T[], arrayIndex : int, count : int) =              xs.CopyTo(index , array , arrayIndex , count)    
    member inline _.Exists (matchValue : Predicate<'T>) =                                           xs.Exists (matchValue) 
    member inline _.Find   (matchValue : Predicate<'T>) =                                           xs.Find   (matchValue) 
    member inline _.FindAll(matchValue : Predicate<'T>) =                                           xs.FindAll(matchValue)
    member inline _.FindIndex(matchValue : Predicate<'T>) =                                         xs.FindIndex(matchValue)
    member inline _.FindIndex(startIndex : int, matchValue : Predicate<'T>) =                       xs.FindIndex(startIndex , matchValue)
    member inline _.FindIndex(startIndex : int, count : int, matchValue : Predicate<'T>) =          xs.FindIndex(startIndex , count , matchValue) 
    member inline _.FindLast(matchValue : Predicate<'T>) =                                          xs.FindLast(matchValue) 
    member inline _.FindLastIndex(matchValue : Predicate<'T>) =                                     xs.FindLastIndex(matchValue) 
    member inline _.FindLastIndex(startIndex : int, matchValue : Predicate<'T>) =                   xs.FindLastIndex(startIndex , matchValue) 
    member inline _.FindLastIndex(startIndex : int, count : int, matchValue : Predicate<'T>) =      xs.FindLastIndex(startIndex , count , matchValue) 
    member inline _.ForEach(action : Action<'T>) =                                                  xs.ForEach(action) 
    member inline _.GetEnumerator() =                                                               xs.GetEnumerator() 
    member inline _.GetRange(index : int, count : int) =                                            xs.GetRange(index , count) 
    member inline _.IndexOf(item : 'T) =                                                            xs.IndexOf(item) 
    member inline _.IndexOf(item : 'T, index : int) =                                               xs.IndexOf(item , index) 
    member inline _.IndexOf(item : 'T, index : int, count : int) =                                  xs.IndexOf(item , index , count) 
    member inline _.Insert(index : int, item : 'T) =                                                xs.Insert(index , item) 
    member inline _.InsertRange(index : int, collection : IEnumerable<'T>) =                        xs.InsertRange(index , collection) 
    member inline _.LastIndexOf(item : 'T) =                                                        xs.LastIndexOf(item) 
    member inline _.LastIndexOf(item : 'T, index : int) =                                           xs.LastIndexOf(item , index) 
    member inline _.LastIndexOf(item : 'T, index : int, count : int) =                              xs.LastIndexOf(item , index , count) 
    member inline _.Remove(item : 'T) =                                                             xs.Remove(item)
    member inline _.RemoveAll(matchValue : Predicate<'T>) =                                         xs.RemoveAll(matchValue) 
    member inline _.RemoveAt(index : int) =                                                         xs.RemoveAt(index) 
    member inline _.RemoveRange(index : int, count : int) =                                         xs.RemoveRange(index , count) 
    member inline _.Reverse() =                                                                     xs.Reverse() 
    member inline _.Reverse(index : int, count : int) =                                             xs.Reverse(index , count) 
    member inline _.Sort() =                                                                        xs.Sort() 
    member inline _.Sort(comparer : IComparer<'T>) =                                                xs.Sort(comparer) 
    member inline _.Sort(index : int, count : int, comparer : IComparer<'T>) =                      xs.Sort(index , count , comparer) 
    member inline _.Sort(comparison : Comparison<'T>) =                                             xs.Sort(comparison) 
    member inline _.ToArray() =                                                                     xs.ToArray() 
    member inline _.TrimExcess() =                                                                  xs.TrimExcess() 
    member inline _.TrueForAll(matchValue : Predicate<'T>) =                                        xs.TrueForAll(matchValue) 
    

    
    //---------------------------------------Interfaces of  System.Collections.Generic.List-------------------------------------
    
  

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