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
    member inline this.List:List<'T> = xs
    
    /// Access the underlying Collections.Generic.List<'T>
    /// ATTENTION! This is not even a shallow copy, mutating it will also change this Instance of Rarr!
    member inline this.ResizeArray:ResizeArray<'T> = xs

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

    member inline this.Item // ovveriding this is the main purpose off all of this class
        with get index       = Rarr.get index xs          
        and  set index value = Rarr.set index value xs            

    member inline this.Capacity =                                                                      xs.Capacity
    member inline this.Count =                                                                         xs.Count
    member inline this.Add(item : 'T) =                                                                xs.Add item
    member inline this.AddRange(collection : IEnumerable<'T>) =                                        xs.AddRange collection
    member inline this.AsReadOnly() =                                                                  xs.AsReadOnly
    member inline this.BinarySearch(index : int, count : int, item : 'T, comparer : IComparer<'T>) =   xs.BinarySearch (index, count , item , comparer )       
    member inline this.BinarySearch(item : 'T) =                                                       xs.BinarySearch item 
    member inline this.BinarySearch(item : 'T, comparer : IComparer<'T>) =                             xs.BinarySearch(item , comparer) 
    member inline this.Clear() =                                                                       xs.Clear() 
    member inline this.Contains(item : 'T) =                                                           xs.Contains(item) 
    member inline this.ConvertAll<'TOutput>(converter : Converter<'T, 'TOutput>) =                     xs.ConvertAll(converter ) 
    member inline this.CopyTo(array : 'T[]) =                                                          xs.CopyTo(array) 
    member inline this.CopyTo(array : 'T[], arrayIndex : int) =                                        xs.CopyTo(array , arrayIndex) 
    member inline this.CopyTo(index : int, array : 'T[], arrayIndex : int, count : int) =              xs.CopyTo(index , array , arrayIndex , count)    
    member inline this.Exists (matchValue : Predicate<'T>) =                                           xs.Exists (matchValue) 
    member inline this.Find   (matchValue : Predicate<'T>) =                                           xs.Find   (matchValue) 
    member inline this.FindAll(matchValue : Predicate<'T>) =                                           xs.FindAll(matchValue)
    member inline this.FindIndex(matchValue : Predicate<'T>) =                                         xs.FindIndex(matchValue)
    member inline this.FindIndex(startIndex : int, matchValue : Predicate<'T>) =                       xs.FindIndex(startIndex , matchValue)
    member inline this.FindIndex(startIndex : int, count : int, matchValue : Predicate<'T>) =          xs.FindIndex(startIndex , count , matchValue) 
    member inline this.FindLast(matchValue : Predicate<'T>) =                                          xs.FindLast(matchValue) 
    member inline this.FindLastIndex(matchValue : Predicate<'T>) =                                     xs.FindLastIndex(matchValue) 
    member inline this.FindLastIndex(startIndex : int, matchValue : Predicate<'T>) =                   xs.FindLastIndex(startIndex , matchValue) 
    member inline this.FindLastIndex(startIndex : int, count : int, matchValue : Predicate<'T>) =      xs.FindLastIndex(startIndex , count , matchValue) 
    member inline this.ForEach(action : Action<'T>) =                                                  xs.ForEach(action) 
    member inline this.GetEnumerator() =                                                               xs.GetEnumerator() 
    member inline this.GetRange(index : int, count : int) =                                            xs.GetRange(index , count) 
    member inline this.IndexOf(item : 'T) =                                                            xs.IndexOf(item) 
    member inline this.IndexOf(item : 'T, index : int) =                                               xs.IndexOf(item , index) 
    member inline this.IndexOf(item : 'T, index : int, count : int) =                                  xs.IndexOf(item , index , count) 
    member inline this.Insert(index : int, item : 'T) =                                                xs.Insert(index , item) 
    member inline this.InsertRange(index : int, collection : IEnumerable<'T>) =                        xs.InsertRange(index , collection) 
    member inline this.LastIndexOf(item : 'T) =                                                        xs.LastIndexOf(item) 
    member inline this.LastIndexOf(item : 'T, index : int) =                                           xs.LastIndexOf(item , index) 
    member inline this.LastIndexOf(item : 'T, index : int, count : int) =                              xs.LastIndexOf(item , index , count) 
    member inline this.Remove(item : 'T) =                                                             xs.Remove(item)
    member inline this.RemoveAll(matchValue : Predicate<'T>) =                                         xs.RemoveAll(matchValue) 
    member inline this.RemoveAt(index : int) =                                                         xs.RemoveAt(index) 
    member inline this.RemoveRange(index : int, count : int) =                                         xs.RemoveRange(index , count) 
    member inline this.Reverse() =                                                                     xs.Reverse() 
    member inline this.Reverse(index : int, count : int) =                                             xs.Reverse(index , count) 
    member inline this.Sort() =                                                                        xs.Sort() 
    member inline this.Sort(comparer : IComparer<'T>) =                                                xs.Sort(comparer) 
    member inline this.Sort(index : int, count : int, comparer : IComparer<'T>) =                      xs.Sort(index , count , comparer) 
    member inline this.Sort(comparison : Comparison<'T>) =                                             xs.Sort(comparison) 
    member inline this.ToArray() =                                                                     xs.ToArray() 
    member inline this.TrimExcess() =                                                                  xs.TrimExcess() 
    member inline this.TrueForAll(matchValue : Predicate<'T>) =                                        xs.TrueForAll(matchValue) 
    

    
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