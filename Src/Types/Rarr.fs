namespace FsEx

open System
open System.Collections.Generic
open NiceString

#nowarn "44" // to disable the obsolete warning on accessing Rarr.List

/// A mutable list. Just like Collections.Generic.List<'T>
/// but with more detailed error messages on using bad indices
/// and some more useful methods like this.First or this.Last.
/// It's a very thin wrapper over a System.Collections.Generic.List<'T>
/// It has all members and interfaces of List<'T> implemented.
/// Just like F# Arrays and F# lists, Rarr equality is also structural.
/// The name Rarr is derived from of the F# type ResizeArray.
/// There is a hidden member called "List" to access the underlying List<'T> directly.
/// In F# use #nowarn "44" to disable the obsolete warning for this hidden member.
[<Sealed;NoComparison>]
type Rarr<'T> private (xs:List<'T>) = 

    // Rarr could potentially be turned into a struct.
    // like for FlatList: https://github.com/fsprojects/FSharpx.Collections/blob/master/src/FSharpx.Collections.Experimental/FlatList.fs
    // but then default constructor 'Rarr()' would not be possible anymore. see:
    // https://stackoverflow.com/a/29956903/969070


    /// Constructs a new FsEx.Rarr.
    /// A FsEx.Rarr is a mutable list like Collections.Generic.List<'T>
    /// but with more detailed error messages on using bad indices.
    /// It's just a very thin wrapper over a System.Collections.Generic.List<'T>
    /// and has all its members and interfaces implemented.
    /// The name Rarr is derived from of the F# type ResizeArray
    /// The list is initially empty and has a capacity
    /// of zero. Upon adding the first element to the list the capacity is
    /// increased to 16, and then increased in multiples of two as required.
    new () = 
        new Rarr<'T>(new List<'T>())

    /// Constructs a new FsEx.Rarr with a given initial capacity.
    /// A FsEx.Rarr is a mutable list like Collections.Generic.List<'T>
    /// but with more detailed error messages on using bad indices.
    /// It's just a very thin wrapper over a System.Collections.Generic.List<'T>
    /// and has all its members and interfaces implemented.
    /// The name Rarr is derived from of the F# type ResizeArray. The list is
    /// initially empty, but will have room for the given number of elements
    /// before any reallocations are required.
    new (capacity:int) = 
        if capacity < 0  then ArgumentException.RaiseBase "new Rarr(capacity): The capacity %d cannot be less than zero." capacity
        new Rarr<'T>(new List<'T>(capacity))

    /// Constructs a new FsEx.Rarr, copying the contents of the given collection.
    /// A FsEx.Rarr is a mutable list like Collections.Generic.List<'T>
    /// but with more detailed error messages on using bad indices.
    /// It's just a very thin wrapper over a System.Collections.Generic.List<'T>
    /// and has all its members and interfaces implemented.
    /// The name Rarr is derived from of the F# type ResizeArray.
    /// The size and capacity of the new list will both be equal to the size of the
    /// given collection.
    new (collection : IEnumerable<'T>)  = 
        if isNull collection then ArgumentNullException.Raise "The IEnumerable given in new FsEx.Rarr(IEnumerable) constructor is null."
        new Rarr<'T>(new List<'T>(collection))
    
    /// Copies the content of two Sequences into a new Rarr
    static member (++) (a : seq<'T>, b : seq<'T>) =
        let r = new Rarr<'T>()
        r.AddRange a
        r.AddRange b
        r   

    /// Copies the content of two Rarrs into a new Rarr
    static member (++) (a : Rarr<'T>, b : Rarr<'T>) =
        let c: int = a.Count + b.Count
        let l = new List<'T>(c)
        for i=0 to a.Count-1 do l.Add(a.[i])
        for i=0 to b.Count-1 do l.Add(b.[i])        
        Rarr(l) 


    /// Constructs a new FsEx.Rarr by using the supplied List<'T>  directly, without any copying of items.
    static member createDirectly (xs:List<'T> ) = 
        if isNull xs then ArgumentNullException.Raise "List in FsEx.Rarr.CreateDirectly is null."
        new Rarr<'T>(xs)

    /// Constructs a new FsEx.Rarr by copying each item from the IEnumerable<'T>
    static member createFromSeq (xs:seq<'T> ) = 
        if isNull xs then ArgumentNullException.Raise "Seq in FsEx.Rarr.CreateFromSeq is null."
        new Rarr<'T>(new List<'T>(xs))

    /// Access the internally used Collections.Generic.List<'T>
    /// This is NOT even a shallow copy, mutating it will also change this Instance of FsEx.Rarr!
    /// Use "#nowarn "44" to disable the obsolete warning
    [<Obsolete("It is not actually obsolete, but normally not used, so hidden from editor tools. In F# use #nowarn \"44\" to disable the obsolete warning.")>]
    member _.List:List<'T> = xs

    /// Access the internally used Collections.Generic.List<'T>
    /// This is NOT even a shallow copy, mutating it will also change this Instance of FsEx.Rarr!
    member _.InternalList:List<'T> = xs

    /// Gets the index of the last item in the FsEx.Rarr.
    /// Equal to this.Count - 1
    member this.LastIndex = 
        if xs.Count = 0 then IndexOutOfRangeException.Raise  "FsEx.Rarr.LastIndex: Failed to get LastIndex of empty Rarr<%s>" (typeof<'T>).FullName
        xs.Count - 1

    /// Get (or set) the last item in the FsEx.Rarr.
    /// Equal to this.[this.Count - 1]
    member this.Last
        with get() = 
            if xs.Count = 0 then IndexOutOfRangeException.Raise  "FsEx.Rarr.Last: Failed to get last item of empty Rarr<%s>" (typeof<'T>).FullName
            xs.[xs.Count - 1]
        and set (v:'T) = 
            if xs.Count = 0 then IndexOutOfRangeException.Raise  "FsEx.Rarr.Last: Failed to set last item of %s to %s" this.ToNiceStringLong (toNiceString v)
            xs.[xs.Count - 1] <- v

    /// Get (or set) the second last item in the FsEx.Rarr.
    /// Equal to this.[this.Count - 2]
    member this.SecondLast
        with get() = 
            if xs.Count < 2 then  IndexOutOfRangeException.Raise  "FsEx.Rarr.SecondLast: Failed to get second last item of %s" this.ToNiceStringLong
            xs.[xs.Count - 2]
        and set (v:'T) = 
            if xs.Count < 2 then  IndexOutOfRangeException.Raise  "FsEx.Rarr.SecondLast: Failed to set second last item of %s to %s" this.ToNiceStringLong (toNiceString v)
            xs.[xs.Count - 2] <- v


    /// Get (or set) the third last item in the FsEx.Rarr.
    /// Equal to this.[this.Count - 3]
    member this.ThirdLast
        with get() = 
            if xs.Count < 3 then  IndexOutOfRangeException.Raise  "FsEx.Rarr.ThirdLast: Failed to get third last item of %s." this.ToNiceStringLong
            xs.[xs.Count - 3]
        and set (v:'T) = 
            if xs.Count < 3 then  IndexOutOfRangeException.Raise  "FsEx.Rarr.ThirdLast: Failed to set third last item of %s to %s" this.ToNiceStringLong (toNiceString v)
            xs.[xs.Count - 3] <- v

    /// Get (or set) the first item in the FsEx.Rarr.
    /// Equal to this.[0]
    member _.First
        with get() = 
            if xs.Count = 0 then IndexOutOfRangeException.Raise  "FsEx.Rarr.First: Failed to get first item of empty Rarr<%s>" (typeof<'T>).FullName
            xs.[0]
        and set (v:'T) = 
            if xs.Count = 0 then IndexOutOfRangeException.Raise  "FsEx.Rarr.First: Failed to set first item of empty Rarr<%s> to %s"  (typeof<'T>).FullName (toNiceString v)
            xs.[0] <- v

    /// Gets the the only item in the FsEx.Rarr.
    /// Fails if the Rarr does not have exactly one element.
    member this.FirstAndOnly =
        if xs.Count = 0 then IndexOutOfRangeException.Raise  "FsEx.Rarr.FirstOnly: Failed to get first item of empty Rarr<%s>" (typeof<'T>).FullName
        if xs.Count > 1 then IndexOutOfRangeException.Raise  "FsEx.Rarr.FirstOnly: Rarr<%s> is expected to have only one item but has %d Rarr: %s" (typeof<'T>).FullName xs.Count this.ToNiceStringLong
        xs.[0]
        

    /// Get (or set) the second item in the FsEx.Rarr.
    /// Equal to this.[1]
    member this.Second
        with get() = 
            if xs.Count < 2 then IndexOutOfRangeException.Raise   "FsEx.Rarr.Second: Failed to get second item of %s" this.ToNiceStringLong
            xs.[1]
        and set (v:'T) = 
            if xs.Count < 2 then IndexOutOfRangeException.Raise   "FsEx.Rarr.Second: Failed to set second item of %s to %s" this.ToNiceStringLong (toNiceString v)
            xs.[1] <- v

    /// Get (or set) the third item in the FsEx.Rarr.
    /// Equal to this.[2]
    member this.Third
        with get() = 
            if xs.Count < 3 then IndexOutOfRangeException.Raise  "FsEx.Rarr.Third: Failed to get third item of %s" this.ToNiceStringLong
            xs.[2]
        and set (v:'T) = 
            if xs.Count < 3 then IndexOutOfRangeException.Raise  "FsEx.Rarr.Third: Failed to set third item of %s to %s" this.ToNiceStringLong (toNiceString v)
            xs.[2] <- v

    /// Checks if this.Count = 0
    member _.IsEmpty =  xs.Count = 0

    /// Checks if this.Count = 1
    [<Obsolete("Has a typo, use IsSingleton instead")>]
    member _.IsSingelton =  xs.Count = 1

    /// Checks if this.Count = 1
    member _.IsSingleton =  xs.Count = 1    

    /// Checks if this.Count > 0
    /// Same as xs.HasItems
    member _.IsNotEmpty =  xs.Count > 0

    /// Checks if this.Count > 0
    /// Same as xs.IsNotEmpty
    member _.HasItems =  xs.Count > 0

    /// Insert an item at the beginning of the list = index 0,
    /// (moving all other items up by one index)
    member _.InsertAtStart x  = 
        xs.Insert(0,x)

    /// Gets an item at index
    /// (Use FsEx.Rarr.GetNeg(i) member if you want to use negative indices too)
    member this.Get index = 
        if index < 0  then IndexOutOfRangeException.Raise "rarr.Get(%d) failed for FsEx.Rarr of %d items, use rarr.GetNeg method if you want negative indices too:\r\n%s" index xs.Count this.ToNiceStringLong
        if index >= xs.Count then IndexOutOfRangeException.Raise "rarr.Get(%d) failed for FsEx.Rarr of %d items:\r\n%s" index xs.Count this.ToNiceStringLong
        xs.[index]


    /// Sets an item at index
    /// (Use FsEx.Rarr.SetNeg(i) member if you want to use negative indices too)
    member _.Set index value = 
        if index < 0  then IndexOutOfRangeException.Raise "The curried function rarr.Set %d value, failed for negative number on FsEx.Rarr of %d items, use rarr.SetNeg method if you want top use negative indices too, for setting %s " index  xs.Count (toNiceString value)
        if index >= xs.Count then IndexOutOfRangeException.Raise "tThe curried function rarr.Set %d value, failed for FsEx.Rarr of %d items. for setting %s " index  xs.Count  (toNiceString value)
        xs.[index] <- value

    /// Gets an item in the FsEx.Rarr by index.
    /// Allows for negative index too ( -1 is last item,  like Python)
    /// (From the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
    member this.GetNeg index = 
        let len = xs.Count
        let ii =  if index < 0 then len + index else index
        if ii<0 || ii >= len then ArgumentException.RaiseBase  "FsEx.Rarr.GetNeg: Failed to get (negative) index %d from FsEx.Rarr of %d items: %s" index xs.Count this.ToNiceStringLong
        xs.[ii]

    /// Sets an item in the FsEx.Rarr by index.
    /// Allows for negative index too ( -1 is last item,  like Python)
    /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
    member this.SetNeg index value = 
        let len = xs.Count
        let ii =  if index < 0 then len + index else index
        if ii<0 || ii >= len then ArgumentException.RaiseBase  "FsEx.Rarr.SetNeg: Failed to set (negative) index %d to %s in %s" index (toNiceString value)  this.ToNiceStringLong
        xs.[ii] <- value

    /// Any index will return a value.
    /// FsEx.Rarr is treated as an endless loop in positive and negative direction
    member _.GetLooped index = 
        let len = xs.Count
        if len=0 then ArgumentException.RaiseBase  "FsEx.Rarr.GetLooped: Failed to get (looped) index %d from FsEx.Rarr of 0 items" index
        let t = index % len
        let ii = if t >= 0 then t  else t + len
        xs.[ii]

    /// Any index will set a value.
    /// FsEx.Rarr is treated as an endless loop in positive and negative direction
    member _.SetLooped index value  = 
        let len = xs.Count
        if len=0 then ArgumentException.RaiseBase  "FsEx.Rarr.SetLooped: Failed to set (looped) index %d to %s in FsEx.Rarr of 0 items" index (toNiceString value)
        let t = index % len
        let ii = if t >= 0 then t  else t + len
        xs.[ii] <- value

    /// Get and remove last item from FsEx.Rarr
    member this.Pop()  = 
        if xs.Count=0 then ArgumentException.RaiseBase " rarr.Pop() failed for %s" this.ToNiceStringLong
        let i = xs.Count - 1
        let v = xs.[i]
        xs.RemoveAt(i)
        v

    /// Get and remove item at index from FsEx.Rarr
    member _.Pop(index:int)  = 
        if index < 0  then ArgumentException.RaiseBase "rarr.Pop(%d) failed for FsEx.Rarr of %d items, index must be positive." index xs.Count
        if index >= xs.Count then ArgumentException.RaiseBase "rarr.Pop(%d) failed for FsEx.Rarr of %d items" index xs.Count
        let v = xs.[index]
        xs.RemoveAt(index)
        v

    /// Creates a shallow copy of the list
    /// (for a FsEx.Rarr of structs this is like a deep copy)
    member _.Clone() = 
        new Rarr<'T>(xs.GetRange(0,xs.Count)) // fastest way to create a shallow copy

    /// <summary>Get the index for the element offset elements away from the end of the collection.
    /// This member exists to support F# indexing from back: ^0 is last item, ^1 is second last</summary>
    /// <param name="rank">The rank of the index. (unused in Rarr)</param>
    /// <param name="offset">The offset from the end.</param>    ///
    /// <returns>The corresponding index from the start.</returns>    ///
    member _.GetReverseIndex( rank , offset:int) = 
        if offset < 0 then IndexOutOfRangeException.Raise " rarr.[^%d]: index from back is negative for FsEx.Rarr of %d items" offset xs.Count
        if offset >= xs.Count then  IndexOutOfRangeException.Raise " rarr.[^%d]: index from back is equal or bigger than rarr.Count %d" offset xs.Count
        xs.Count - offset - 1

    /// Defines F# slicing notation operator use including negative indices. The resulting FsEx.Rarr includes the end index.
    /// Raises an ArgumentException if indices are out of range.
    /// For indexing from the end use the ^ prefix. E.g. ^0 for the last item.
    member _.GetSlice(startIdx: option<int>, endIdx: option<int>) : Rarr<'T> = 
        //.GetSlice maps onto slicing operator .[1..3]
        let inline debugTxt (i:int option)  = match i with None -> " " |Some i -> i.ToString()
        let count = xs.Count
        let stIdx  = 
            match startIdx with
            | None -> 0
            | Some si ->
                if si < 0 || si >= count then
                    ArgumentException.RaiseBase  "FsEx.Rarr.[%s..%s], GetSlice: start index must be between 0 and %d for Rarr of %d items." (debugTxt startIdx) (debugTxt endIdx) (count-1) count
                si

        let enIdx  = 
            match endIdx with
            | None -> count-1
            | Some ei ->
                if ei < 0 || ei >= count then
                     ArgumentException.RaiseBase  "FsEx.Rarr.[%s..%s], GetSlice: end index must be between 0 and %d for Rarr of %d items." (debugTxt startIdx) (debugTxt endIdx)  (count-1) count
                else
                    ei

        // end must be same or bigger than start
        if enIdx >=0 && stIdx > enIdx then
            ArgumentException.RaiseBase "FsEx.Rarr.[%s..%s], The given start index must be smaller than or equal to the end index for Rarr of %d items." (debugTxt startIdx) (debugTxt endIdx) count

        new Rarr<'T>(xs.GetRange(stIdx,enIdx - stIdx + 1))

    /// Defines F# slicing notation operator use including negative indices. ( -1 is last item, like Python)
    /// The resulting FsEx.Rarr includes the end index.
    /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
    member _.SetSlice(startIdx: option<int>, endIdx: option<int>, newValues:IList<'T>) : unit = 
        //.SetSlice maps onto slicing operator .[1..3] <- xs
        let inline debugTxt (i:int option)  = match i with None -> " " |Some i -> i.ToString()
        let count = xs.Count
        let stIdx  = 
            match startIdx with
            | None -> 0
            | Some si ->
                if si < 0 || si >= count then
                    ArgumentException.RaiseBase  "FsEx.Rarr.[%s..%s], GetSlice: start index must be between 0 and %d for Rarr of %d items." (debugTxt startIdx) (debugTxt endIdx) (count-1) count
                si

        let enIdx  = 
            match endIdx with
            | None -> count-1
            | Some ei ->
                if ei < 0 || ei >= count then
                     ArgumentException.RaiseBase  "FsEx.Rarr.[%s..%s], GetSlice: end index must be between 0 and %d for Rarr of %d items." (debugTxt startIdx) (debugTxt endIdx)  (count-1) count
                else
                    ei

        // end must be same or bigger than start
        if enIdx >=0 && stIdx > enIdx then
            ArgumentException.RaiseBase "FsEx.Rarr.[%s..%s], The given start index must be smaller than or equal to the end index for Rarr of %d items." (debugTxt startIdx) (debugTxt endIdx) count

        let countToAdd = enIdx - stIdx + 1
        if newValues.Count <> countToAdd then
            ArgumentException.RaiseBase "FsEx.Rarr.[%s..%s], SetSlice expected %d item in newValues IList but only found %d" (debugTxt startIdx) (debugTxt endIdx) countToAdd newValues.Count
        for i = stIdx to enIdx do
            xs.[i] <- newValues.[i-stIdx]

    // LikeFsEx.Rarr.filter but modifying theFsEx.Rarr in place.
    // Removes Items formFsEx.Rarr if predicate returns true.
    //member _.FilterInPlace (predicate: 'T -> bool) :unit = 
    //    for i = xs.Count - 1 downto 0 do // reverse order, important
    //        if predicate xs.[i] then
    //            xs.RemoveAt(i)


    /// Calls NiceString.toNiceString this
    /// Listing includes the first 6 items
    override this.ToString() = 
        NiceString.toNiceString this


    /// A property like the ToString() method,
    /// But with richer formatting
    /// Listing includes the first 6 items
    member this.ToNiceString = 
        NiceString.toNiceString this

    /// A property like the ToString() method,
    /// But with richer formatting
    /// Listing includes the first 50 items
    member this.ToNiceStringLong = 
        NiceString.toNiceStringLong this

    //--------------------------------------------------------------------------------------------------------------------------
    //---------------------------- members of  System.Collections.Generic.List --------------------------------------------------
    //-------------------------https://referencesource.microsoft.com/#mscorlib/system/collections/generic/list.cs---------------

    // doc string taken from comments here: https://referencesource.microsoft.com/#mscorlib/system/collections/generic/list.cs

    /// Sets or Gets the element at the given index. With nice error messages on bad indices.
    member this.Item // overriding this and other index based members is the main purpose off all of this class
        // Of course it would be simpler to just override the .Item method on System.Collections.Generic.List<'T> but unfortunately it is sealed
        with get index  = 
            if index < 0 then IndexOutOfRangeException.Raise "Index must be positive. Failed to get rarr.[%d] from:\r\n%s" index  this.ToNiceStringLong
            if index >= xs.Count then IndexOutOfRangeException.Raise "Failed to get rarr.[%d] from:\r\n%s" index this.ToNiceStringLong
            xs.[index]
        and set index value = 
            if index < 0 then IndexOutOfRangeException.Raise "Index must be positive.Failed to set rarr.[%d] <- %s in:\r\n%s " index (toNiceString value) this.ToNiceStringLong
            if index >= xs.Count then IndexOutOfRangeException.Raise "Failed to set rarr.[%d] <- %s in:\r\n%s " index (toNiceString value) this.ToNiceStringLong
            xs.[index] <- value

    /// Gets and sets the capacity of this list.  The capacity is the size of
    /// the internal array used to hold items.  When set, the internal
    /// array of the list is reallocated to the given capacity.
    /// You can also use rarr.TrimExcess() to release all unused memory referenced by the list,
    /// Value must be same or bigger than FsEx.Rarr.Count
    member _.Capacity
        with get () = xs.Capacity
        and set c   = 
            if c < xs.Count then IndexOutOfRangeException.Raise "The Capacity of a Rarr cannot be set smaller than it's Count %d" xs.Count
            xs.Capacity <- c

    /// Read-only property describing how many elements are in the List.
    member _.Count =                                                                  xs.Count

    /// Adds the given object to the end of this list. The size of the list is
    /// increased by one. If required, the capacity of the list is doubled
    /// before adding the new element.
    member _.Add(item : 'T) =                                                         xs.Add item

    // Adds the elements of the given collection to the end of this list. If
    // required, the capacity of the list is increased to twice the previous
    // capacity or the new size, whichever is larger.
    member  _.AddRange(collection : IEnumerable<'T>) =                                xs.AddRange collection

    /// Returns a read-only System.Collections.ObjectModel.ReadOnlyCollection
    /// wrapper over the current collection.
    member _.AsReadOnly() =                                                           xs.AsReadOnly

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
    /// The method uses the Array.BinarySearch method to perform the search.
    member _.BinarySearch(index:int, count:int, item : 'T, comparer : IComparer<'T>) = 
        if index < 0  then
            ArgumentException.RaiseBase "rarr.BinarySearch: The start index %d cannot be less than zero (for Rarr of %d elements)." index xs.Count
        elif count < 0 then
            ArgumentException.RaiseBase "rarr.BinarySearch: The desired count %d cannot be less than zero. (start index %d, Rarr of %d elements)." count index xs.Count
        elif index + count > xs.Count then
            ArgumentException.RaiseBase "rarr.BinarySearch: Using 'count' %d from 'start' index %d is bigger than last index %d or Rarr." count index (xs.Count-1)
        xs.BinarySearch (index, count , item , comparer )

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
    /// The method uses the Array.BinarySearch method to perform the search.
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
    /// The method uses the Array.BinarySearch method to perform the search.
    member _.BinarySearch(item : 'T, comparer : IComparer<'T>) =                             xs.BinarySearch(item , comparer)

    /// Clears the contents of List.
    /// To also release all memory referenced by the list,
    /// execute rarr.TrimExcess() afterwards.
    member _.Clear() =                                                                       xs.Clear()

    /// Contains returns true if the specified element is in the List.
    /// It does a linear, O(n) search.  Equality is determined by calling item.Equals().
    member _.Contains(item : 'T) =                                                           xs.Contains(item)

    /// <summary>Converts the elements in the current List to another type, and returns
    /// a list containing the converted elements.</summary>
    /// <param name="converter">A System.Converter delegate that converts each element from
    /// one type to another type.</param>
    /// <typeparam name="TOutput">The type of the elements of the target array.</typeparam>
    /// <returns>A List of the target type containing the converted elements from
    /// the current List.</returns>
    member _.ConvertAll<'TOutput>(converter : Converter<'T, 'TOutput>) =                     xs.ConvertAll(converter) |> Rarr

    /// Copies a section of this list to the given array at the given index.
    /// The method uses the Array.Copy method to copy the elements.
    member _.CopyTo(array : 'T[]) =                                                          xs.CopyTo(array)

    /// <summary>Copies the elements of the FsEx.Rarr to an Array,
    /// starting at a particular Array index.</summary>
    /// <param name="array">The one-dimensional Array that is the destination of the
    /// elements copied from the List The Array must have zero-based indexing.</param>
    /// <param name="arrayIndex">The zero-based index in the Array at which copying begins.</param>
    member _.CopyTo(array : 'T[], arrayIndex:int) = 
        if arrayIndex < 0 then
            ArgumentException.RaiseBase "rarr.CopyTo: The start index %d cannot be less than zero ( for copying %d elements to array of %d)" arrayIndex  xs.Count array.Length
        if xs.Count + arrayIndex > array.Length then
            ArgumentException.RaiseBase "rarr.CopyTo: The start index %d  is to big top copy %d  elements into Array of %d" arrayIndex  xs.Count array.Length
        xs.CopyTo(array , arrayIndex)

    /// <summary>Copies a range of elements from the List to a compatible one-dimensional array, starting at the specified index of the target array.</summary>
    /// <param name="index">The zero-based index in the source List at which copying begins.</param>
    /// <param name="array">The one-dimensional <see cref="T:System.Array" /> that is the destination of the elements copied from List. The <see cref="T:System.Array" /> must have zero-based indexing.</param>
    /// <param name="arrayIndex">The zero-based index in <paramref name="array" /> at which copying begins.</param>
    /// <param name="count">The number of elements to copy.</param>
    member _.CopyTo(index:int, array : 'T[], arrayIndex:int, count:int) = 
        if arrayIndex< 0 then
            ArgumentException.RaiseBase "rarr.CopyTo(index:int, array : 'T[], arrayIndex:int, count:int): The start index of target Array %d cannot be less than zero. (for copying %d elements starting at %d from Rarr of %d into array of size %d starting at array position %d)" arrayIndex  count index xs.Count array.Length arrayIndex
        if index < 0 then
            ArgumentException.RaiseBase "rarr.CopyTo(index:int, array : 'T[], arrayIndex:int, count:int): The start index of Rarr %d cannot be less than zero. (for copying %d elements starting at %d from Rarr of %d into array of size %d starting at array position %d)" index  count index xs.Count array.Length arrayIndex
        if index + count > xs.Count then
            ArgumentException.RaiseBase "rarr.CopyTo(index:int, array : 'T[], arrayIndex:int, count:int): index + count is more than xs.Count (for copying %d elements starting at %d from Rarr of %d into array of size %d starting at array position %d)" index  count index xs.Count array.Length arrayIndex
        if count + arrayIndex > array.Length then
            ArgumentException.RaiseBase "rarr.CopyTo(index:int, array : 'T[], arrayIndex:int, count:int): count is more than array.Length - arrayIndex (for copying %d elements starting at %d from Rarr of %d into array of size %d starting at array position %d)" index  count index xs.Count array.Length arrayIndex
        xs.CopyTo(index , array , arrayIndex , count)

    /// <summary>Determines whether the List contains elements that match the conditions defined by the specified predicate.</summary>
    /// <param name="matchValue">The Predicate delegate that defines the conditions of the elements to search for.</param>
    /// <returns>true if the List contains one or more elements that match the conditions defined by the specified predicate; otherwise, false.</returns>
    member _.Exists (matchValue : Predicate<'T>) =                                           xs.Exists (matchValue)

    /// <summary>Searches for an element that matches the conditions defined by the specified predicate, and returns the first occurrence within the entire List.</summary>
    /// <param name="matchValue">The Predicate delegate that defines the conditions of the element to search for.</param>
    /// <returns>The first element that matches the conditions defined by the specified predicate, if found; otherwise, the default value for type 'T.</returns>
    member _.Find   (matchValue : Predicate<'T>) =                                           xs.Find   (matchValue)

    /// <summary>Retrieves all the elements that match the conditions defined by the specified predicate.</summary>
    /// <param name="matchValue">The Predicate delegate that defines the conditions of the elements to search for.</param>
    /// <returns>A List containing all the elements that match the conditions defined by the specified predicate, if found; otherwise, an empty List.</returns>
    member _.FindAll(matchValue : Predicate<'T>) =                                           xs.FindAll(matchValue) |> Rarr

    /// <summary>Retrieves all the elements that match the conditions defined by the specified predicate.</summary>
    /// <param name="matchValue">The Predicate delegate that defines the conditions of the elements to search for.</param>
    /// <returns>A List containing all the elements that match the conditions defined by the specified predicate, if found; otherwise, an empty List.</returns>
    member _.FindIndex(matchValue : Predicate<'T>) =                                         xs.FindIndex(matchValue)


    /// <summary>Searches for an element that matches the conditions defined by the specified predicate, and returns the zero-based index of the first occurrence within the range of elements in the List that extends from the specified index to the last element.</summary>
    /// <param name="startIndex">The zero-based starting index of the search.</param>
    /// <param name="matchValue">The Predicate delegate that defines the conditions of the element to search for.</param>
    /// <returns>The zero-based index of the first occurrence of an element that matches the conditions defined by matchValue, if found; otherwise, -1.</returns>
    member _.FindIndex(startIndex:int, matchValue : Predicate<'T>) = 
        if startIndex < 0 || startIndex >= xs.Count then
            ArgumentException.RaiseBase "rarr.FindIndex: The start index %d cannot be less than zero or bigger than %d for Rarr of %d elements." startIndex  (xs.Count-1) xs.Count
        xs.FindIndex(startIndex , matchValue)

    /// <summary>Searches for an element that matches the conditions defined by the specified predicate, and returns the zero-based index of the first occurrence within the range of elements in the List that starts at the specified index and contains the specified number of elements.</summary>
    /// <param name="startIndex">The zero-based starting index of the search.</param>
    /// <param name="count">The number of elements in the section to search.</param>
    /// <param name="matchValue">The Predicate delegate that defines the conditions of the element to search for.</param>
    /// <returns>The zero-based index of the first occurrence of an element that matches the conditions defined by matchValue, if found; otherwise, -1.</returns>
    member _.FindIndex(startIndex:int, count:int, matchValue : Predicate<'T>) = 
        if startIndex < 0  then
            ArgumentException.RaiseBase "rarr.FindIndex: The startIndex %d cannot be less than zero. (with desired count %d,for Rarr of %d elements)." startIndex  xs.Count count xs.Count
        elif count < 0 then
            ArgumentException.RaiseBase "rarr.FindIndex: The desired count %d cannot be less than zero. (startIndex %d) for Rarr of %d elements." count startIndex xs.Count
        elif startIndex + count > xs.Count then
            ArgumentException.RaiseBase "rarr.FindIndex: There are fewer than 'count' %d elements between the startIndex %d and the end. for Rarr of %d elements." count startIndex xs.Count
        xs.FindIndex(startIndex , count , matchValue)

    /// <summary>Searches for an element that matches the conditions defined by the specified predicate, and returns the last occurrence within the entire List.</summary>
    /// <param name="matchValue">The Predicate delegate that defines the conditions of the element to search for.</param>
    /// <returns>The last element that matches the conditions defined by the specified predicate, if found; otherwise, the default value for type 'T.</returns>
    member _.FindLast(matchValue : Predicate<'T>) = 
        xs.FindLast(matchValue)


    /// <summary>Searches for an element that matches the conditions defined by the specified predicate, and returns the zero-based index of the last occurrence within the entire List.</summary>
    /// <param name="matchValue">The Predicate delegate that defines the conditions of the element to search for.</param>
    /// <returns>The zero-based index of the last occurrence of an element that matches the conditions defined by matchValue, if found; otherwise, -1.</returns>
    member _.FindLastIndex(matchValue : Predicate<'T>) = 
        xs.FindLastIndex(matchValue)

    /// <summary>Searches for an element that matches the conditions defined by the specified predicate,
    /// and returns the zero-based index of the last occurrence within the range of elements in the List that extends from the first element to the specified index.</summary>
    /// <param name="startIndex">The zero-based starting index of the backward search.</param>
    /// <param name="matchValue">The Predicate delegate that defines the conditions of the element to search for.</param>
    /// <returns>The zero-based index of the last occurrence of an element that matches the conditions defined by matchValue, if found; otherwise, -1.</returns>
    member _.FindLastIndex(startIndex:int, matchValue : Predicate<'T>) = 
        if  startIndex < 0  || startIndex >= xs.Count  then
            ArgumentException.RaiseBase "rarr.FindLastIndex: The start index %d cannot be less than zero or  more or equal to xs.Count %d." startIndex xs.Count
        xs.FindLastIndex(startIndex , matchValue)

    /// <summary>Searches for an element that matches the conditions defined by the specified predicate,
    /// and returns the zero-based index of the last occurrence within the range of elements in the List that contains the specified number of elements and ends at the specified index.</summary>
    /// <param name="startIndex">The zero-based starting index of the backward search.</param>
    /// <param name="count">The number of elements in the section to search.</param>
    /// <param name="matchValue">The Predicate delegate that defines the conditions of the element to search for.</param>
    /// <returns>The zero-based index of the last occurrence of an element that matches the conditions defined by matchValue, if found; otherwise, -1.</returns>
    member _.FindLastIndex(startIndex:int, count:int, matchValue : Predicate<'T>) = 
        if startIndex >= xs.Count  then
            ArgumentException.RaiseBase "rarr.FindLastIndex: The start index %d cannot be more or equal to xs.Count %d." startIndex xs.Count
        elif count < 0 then
            ArgumentException.RaiseBase "rarr.FindLastIndex: The desired count %d cannot be less than zero. (start index %d, Rarr of %d elements)." count startIndex xs.Count
        elif startIndex - count < -1 then
            ArgumentException.RaiseBase "rarr.FindLastIndex: Using 'count' %d from 'start' index %d the search would go below index 0. (for Rarr of %d elements)" count startIndex (xs.Count-1)
        xs.FindLastIndex(startIndex , count , matchValue)

    /// <summary>Performs the specified action on each element of the List.</summary>
    /// <param name="action">The <see cref="T:System.Action`1" /> delegate to perform on each element of the List.</param>
    member _.ForEach(action : Action<'T>) = 
        xs.ForEach(action)

    /// Returns an enumerator for this list with the given
    /// permission for removal of elements. If modifications made to the list
    /// while an enumeration is in progress, the MoveNext and
    /// GetObject methods of the enumerator will throw an exception.
    member _.GetEnumerator() = 
        xs.GetEnumerator()

    /// <summary>Creates a shallow copy of a range of elements in the source List.</summary>
    /// <param name="index">The zero-based List index at which the range starts.</param>
    /// <param name="count">The number of elements in the range.</param>
    /// <returns>A shallow copy of a range of elements in the source List.</returns>
    member _.GetRange(index:int, count:int) : Rarr<'T> = 
        if index < 0  then
            ArgumentException.RaiseBase "rarr.GetRange: The start index %d cannot be less than zero . (with desired count %d,for Rarr of %d elements)." index  xs.Count count xs.Count
        elif count < 0 then
            ArgumentException.RaiseBase "rarr.GetRange: The desired count %d cannot be less than zero. (start index %d) for Rarr of %d elements." count index xs.Count
        elif index + count > xs.Count then
            ArgumentException.RaiseBase "rarr.GetRange: There are fewer than 'count' %d elements between the 'start' index %d and the end. for Rarr of %d elements." count index xs.Count
        xs.GetRange(index , count)|> Rarr


    /// Returns the index of the first occurrence of a given value in a range of
    /// this list. The list is searched forwards from beginning to end.
    /// The elements of the list are compared to the given value using the
    /// Object.Equals method.
    /// This method uses the Array.IndexOf method to perform the search.
    member _.IndexOf(item : 'T) = 
        xs.IndexOf(item)

    /// Returns the index of the first occurrence of a given value in a range of
    /// this list. The list is searched forwards, starting at given index
    /// 'index' and ending at count number of elements. The
    /// elements of the list are compared to the given value using the
    /// Object.Equals method.
    /// This method uses the Array.IndexOf method to perform the search
    member _.IndexOf(item : 'T, index:int) = 
        if index < 0 || index >= xs.Count then
            ArgumentException.RaiseBase "rarr.IndexOf: The start index %d cannot be less than zero or bigger than %d for Rarr of %d elements." index  (xs.Count-1) xs.Count
        xs.IndexOf(item , index) // the .Net List only fails if index > xs.Count

    // Returns the index of the first occurrence of a given value in a range of
    // this list. The list is searched forwards, starting at index
    // index and up to count number of elements. The
    // elements of the list are compared to the given value using the
    // Object.Equals method.
    // This method uses the Array.IndexOf method to perform the  search.
    member _.IndexOf(item : 'T, index:int, count:int) = 
        if index < 0  then
            ArgumentException.RaiseBase "rarr.IndexOf: The start index %d cannot be less than zero (for Rarr of %d elements)." index xs.Count
        elif count < 0 then
            ArgumentException.RaiseBase "rarr.IndexOf: The desired count %d cannot be less than zero. (start index %d, Rarr of %d elements)." count index xs.Count
        elif index + count > xs.Count then
            ArgumentException.RaiseBase "rarr.IndexOf: Using 'count' %d from 'start' index %d is bigger than last index %d or Rarr." count index (xs.Count-1)
        xs.IndexOf(item , index , count)

    /// Inserts an element into this list at a given index. The size of the list
    /// is increased by one. If required, the capacity of the list is doubled
    /// before inserting the new element.
    member _.Insert(index:int, item : 'T) = 
        if index < 0 || index > xs.Count then
            ArgumentException.RaiseBase "rarr.InsertRange: The start index %d cannot be less than zero or bigger than %d for Rarr of %d elements." index  xs.Count xs.Count
        xs.Insert(index , item)

    /// Inserts the elements of the given collection at a given index. If
    /// required, the capacity of the list is increased to twice the previous
    /// capacity or the new size, whichever is larger.  Ranges may be added
    /// to the end of the list by setting index to the List's size.
    member _.InsertRange(index:int, collection : IEnumerable<'T>) = 
        if index < 0 || index > xs.Count then
            ArgumentException.RaiseBase "rarr.InsertRange: The start index %d cannot be less than zero or bigger than %d for Rarr of %d elements." index  xs.Count xs.Count
        xs.InsertRange(index , collection)

    /// Returns the index of the last occurrence of a given value in a range of
    /// this list. The list is searched backwards, starting at the end
    /// and ending at the first element in the list. The elements of the list
    /// are compared to the given value using the Object.Equals method.
    /// This method uses the Array.LastIndexOf method to perform the search.
    member _.LastIndexOf(item : 'T) = 
        xs.LastIndexOf(item)

    /// Returns the index of the last occurrence of a given value in a range of
    /// this list. The list is searched backwards, starting at index
    /// index and ending at the first element in the list. The
    /// elements of the list are compared to the given value using the
    /// Object.Equals method.
    /// This method uses the Array.LastIndexOf method to perform the search.
    member _.LastIndexOf(item : 'T, index:int) = 
        if index < 0 || index >= xs.Count then
            ArgumentException.RaiseBase "rarr.LastIndexOf: The start index %d cannot be less than zero or bigger than xs.Count-1 (%d) for Rarr." index  (xs.Count-1)
        xs.LastIndexOf(item , index)

    /// Returns the index of the last occurrence of a given value in a range of
    /// this list. The list is searched backwards, starting at index
    /// and up to count elements. The elements of the list are compared to
    /// the given value using the Object.Equals method.
    /// This method uses the Array.LastIndexOf method to perform the search.
    member _.LastIndexOf(item : 'T, index:int, count:int) = 
        if index >= xs.Count  then
            ArgumentException.RaiseBase "rarr.LastIndexOf: The start index %d cannot be more or equal to xs.Count %d." index xs.Count
        elif count < 0 then
            ArgumentException.RaiseBase "rarr.LastIndexOf: The desired count %d cannot be less than zero. (start index %d, Rarr of %d elements)." count index xs.Count
        elif index - count < -1 then
            ArgumentException.RaiseBase "rarr.LastIndexOf: Using 'count' %d from 'start' index %d the search would go below index 0. (for Rarr of %d elements)" count index (xs.Count-1)
        xs.LastIndexOf(item , index , count)

    /// Removes the element at the given index. The size of the list is
    /// decreased by one.
    member _.Remove(item : 'T) = 
        xs.Remove(item )

    /// This method removes all items which matches the predicate.
    /// The complexity is O(n).
    member _.RemoveAll(matchValue : Predicate<'T>) = 
        xs.RemoveAll(matchValue)

    /// Removes the element at the given index. The size of the list is
    /// decreased by one.
    member _.RemoveAt(index:int) = 
        if index < 0 || index >= xs.Count then
            ArgumentException.RaiseBase "rarr.RemoveAt: The index to remove %d cannot be less than zero or bigger than %d for Rarr of %d elements." index  xs.Count xs.Count
        xs.RemoveAt(index)

    /// Removes a range of elements from this list.
    member _.RemoveRange(index:int, count:int) = 
        if index < 0  then
            ArgumentException.RaiseBase "rarr.RemoveRange: The start index %d cannot be less than zero (for Rarr of %d elements)." index xs.Count
        elif count < 0 then
            ArgumentException.RaiseBase "rarr.RemoveRange: The desired count %d cannot be less than zero. (start index %d, Rarr of %d elements)." count index xs.Count
        elif index + count > xs.Count then
            ArgumentException.RaiseBase "rarr.RemoveRange: Using 'count' %d from 'start' index %d is bigger than last index %d or Rarr." count index (xs.Count-1)
        xs.RemoveRange(index , count)

    /// Reverses the order of the elements in this entire list.
    member _.Reverse() = 
        xs.Reverse()

    /// Reverses the elements in a range of this list. 
    /// This method uses the Array.Reverse method to reverse the elements.
    member _.Reverse(index:int, count:int) = 
        if index < 0 || index > xs.Count then
            ArgumentException.RaiseBase "rarr.Reverse: The start index %d cannot be less than zero or bigger than %d . (with desired count %d) for Rarr of %d elements." index  xs.Count count xs.Count
        elif count < 0 then
            ArgumentException.RaiseBase "rarr.Reverse: The desired count %d cannot be less than zero. (start index %d) for Rarr of %d elements." count index xs.Count
        elif index + count > xs.Count then
            ArgumentException.RaiseBase "rarr.Reverse: There are fewer than 'count' %d elements between the 'start' index %d and the end. for Rarr of %d elements." count index xs.Count
        xs.Reverse(index , count)

    /// Sorts the elements in this list.  Uses the default comparer and Array.Sort.
    /// On strings this is not ordinal but culture sensitive.
    /// E.G. "a" is before "Z"
    /// While using ordinal sorting like in Rarr.sort or Array.sort module  "Z" is before "a"
    member _.Sort() = 
        xs.Sort()

    /// Sorts the elements in this list.  Uses Array.Sort with the provided comparer.
    member _.Sort(comparer : IComparer<'T>) = 
        xs.Sort(comparer)

    /// Sorts the elements in a section of this list. The sort compares the
    /// elements to each other using the given IComparer interface. If
    /// comparer is null, the elements are compared to each other using
    /// the IComparable interface, which in that case must be implemented by all
    /// elements of the list.
    /// This method uses the Array.Sort method to sort the elements.
    member _.Sort(index:int, count:int, comparer : IComparer<'T>) = 
        if index < 0  then
            ArgumentException.RaiseBase "rarr.Sort: The start index %d cannot be less than zero (for Rarr of %d elements)." index xs.Count
        elif count < 0 then
            ArgumentException.RaiseBase "rarr.Sort: The desired count %d cannot be less than zero. (start index %d, Rarr of %d elements)." count index xs.Count
        elif index + count > xs.Count then
            ArgumentException.RaiseBase "rarr.Sort: Using 'count' %d from 'start' index %d is bigger than last index %d or Rarr." count index (xs.Count-1)
        xs.Sort(index , count , comparer)

    /// Sorts the elements in this list.
    /// Uses Array.Sort with the provided comparison.
    member _.Sort(comparison : Comparison<'T>) = 
        xs.Sort(comparison)

    /// ToArray returns a new Object array containing the contents of the List.
    /// This requires copying the List, which is an O(n) operation.
    member _.ToArray() = 
        xs.ToArray()

    /// Sets the capacity of this list to the size of the list. This method can
    /// be used to minimize a list's memory overhead once it is known that no
    /// new elements will be added to the list. To completely clear a list and
    /// release all memory referenced by the list, execute the following
    /// statements:
    /// list.Clear()
    /// list.TrimExcess()
    member _.TrimExcess() = 
        xs.TrimExcess()

    /// Determines whether every element in the List
    /// matches the conditions defined by the specified predicate.
    member _.TrueForAll(matchValue : Predicate<'T>) = 
        xs.TrueForAll(matchValue)


    ///------------------------[<CustomEquality>]------------------------------

    member inline internal this.IsEqualTo(other:Rarr<'T>) = 
        if Object.ReferenceEquals(this,other) then true
        elif this.Count <> other.Count then false
        else
            let comparer = EqualityComparer<'T>.Default // for  structural equality to be implemented on this class without putting the <'T when 'T : equality> constraint on 'T?
            let rec eq i = 
                if i < this.Count then
                    if comparer.Equals(this.List.[i] , other.List.[i]) then
                        eq (i+1)
                    else
                        false
                else
                    true
            eq 0

    override this.GetHashCode() = 
        let combineHash x y = (x <<< 1) + y + 631 //from FSharp.Core Set
        let mutable res = 0
        for i=0 to xs.Count - 1 do
            let x = xs.[i]
            res <- combineHash res (LanguagePrimitives.GenericHash x)
        res

    override this.Equals(that:obj) = 
        match that with
        | :? Rarr<'T> as that -> this.IsEqualTo(that)
        | _ -> false

    interface IEquatable<Rarr<'T>> with
        member this.Equals(that:Rarr<'T>) = 
             this.IsEqualTo(that)


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
        member _.RemoveAt(index) = 
            if index<0 || index >= xs.Count then ArgumentException.RaiseBase "Cant rarr.RemoveAt(%d) in FsEx.Rarr (cast to IList<_>) of %d items: %s " index  xs.Count  (toNiceString xs)
            xs.RemoveAt(index)

        member _.Item
            with get index = 
                if index<0 || index >= xs.Count then ArgumentException.RaiseBase "Cant get index %d from FsEx.Rarr (cast to IList<_>) of %d items: %s" index xs.Count (toNiceString xs)
                xs.[index]
            and set index value = 
                if index<0 || index >= xs.Count then ArgumentException.RaiseBase "Cant set index %d to %s in FsEx.Rarr (cast to IList<_>) of %d items: %s " index (toNiceString value) xs.Count  (toNiceString xs)
                xs.[index] <- value

    interface IReadOnlyList<'T> with
        member _.Item
            with get index = 
                if index<0 || index >= xs.Count then ArgumentException.RaiseBase "Cant get index %d from FsEx.Rarr (cast to IReadOnlyList<_>) of %d items: %s" index xs.Count (toNiceString xs)
                xs.[index]

    interface Collections.IList with // Non generic
        member _.Add(x)              = (xs:>Collections.IList).Add(x)
        member _.IndexOf(item)       = (xs:>Collections.IList).IndexOf(item)
        member _.Insert(index,item)  = (xs:>Collections.IList).Insert(index,item)
        member _.RemoveAt(index)     = 
            if index<0 || index >= xs.Count then ArgumentException.RaiseBase "Cant rarr.RemoveAt(%d) in FsEx.Rarr (cast to non Generic IList) of %d items: %s " index  xs.Count  (toNiceString xs)
            (xs:>Collections.IList).RemoveAt(index)

        member _.Item
            with get index = 
                if index<0 || index >= xs.Count then ArgumentException.RaiseBase "Cant get index %d from FsEx.Rarr (cast to non Generic IList) of %d items: %s" index xs.Count (toNiceString xs)
                (xs:>Collections.IList).[index]
            and set index value = 
                if index<0 || index >= xs.Count then ArgumentException.RaiseBase "Cant set index %d to %s in FsEx.Rarr (cast to non Generic IList) of %d items: %s " index (toNiceString value)  xs.Count  (toNiceString xs)
                (xs:>Collections.IList).[index] <- value

        member _.Remove x =   (xs:>Collections.IList).Remove x
        member _.Clear() =    (xs:>Collections.IList).Clear()
        member _.Contains x = (xs:>Collections.IList).Contains x
        member _.IsReadOnly = false
        member _.IsFixedSize = false

(*
TODO add a version of Rarr that requires a UoM on the index.
see https://twitter.com/mccrews/status/1489269693483405315 and 
https://gist.github.com/matthewcrews/bea24372de6af4f040ec68a0640289ef
        
type TiRarr<'T, [<Measure>] 'M>(xs:seq<'T>)=
            
    let ls = ResizeArray(xs)
            
    member _.Item
        with get (i: int<'M>) =
            ls.[int i]
        
        
[<Measure>] type ChickenIdx
[<Measure>] type CowIdx
        
        
let chicks = TiRarr<string, ChickenIdx>(["Pi"; "Pu"; "Pa"]) 
        
printfn "%A" chicks.[1<ChickenIdx>]
*)