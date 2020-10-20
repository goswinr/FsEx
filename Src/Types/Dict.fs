namespace FsEx

open System
open System.Collections.Generic

/// A type abbreviation for System.Collections.Generic.HashSet<'T> 
/// so that it is available in FsEx namespace
/// ( with a lowercase 's')
type Hashset<'T> = HashSet<'T>

/// A thin wraper over System.Collections.Generic.Dictionary<'K,'V>) with nicer Error messages on accessing missing keys
/// not the same sa lowercase 'dict'in F#
type Dict< 'K,'V when 'K:equality > private (dd : Dictionary<'K,'V>) =
    
    //using inheritance from Dictionary would not work because .Item method is seald and cant have an override

    let get k  =
         let ok, v = dd.TryGetValue(k)
         if ok then  v
         else KeyNotFoundException.Raise "Dict.Get failed to find key %A in %A of %d items" k dd dd.Count
    
    /// create a new empty Dict<'K,'V>
    new () = Dict(new Dictionary<'K,'V>())
    

    /// Constructs a new Dict by using the supplied Dictionary<'K,'V> directly, without any copying of items
    static member CreateDirectly (dic:Dictionary<'K,'V> ) = 
        if isNull dic then ArgumentNullException.Raise "Dictionary in Dict.CreateDirectly is null"
        Dict(dic)

    /// Access the underlying Collections.Generic.Dictionary<'K,'V>)
    /// ATTENTION! This is not even a shallow copy, mutating it will also change this Instance of Dict!
    member _.Dictionary = dd

    /// For Index operator .[i]: get or set the value for given key
    member _.Item 
        with get k   = get k        
        and  set k v = dd.[k] <- v
    
    /// Get value for given key
    member _.Get key = get key 

    /// Set value for given key
    member _.Set key value = dd.[key] <- value
    
    /// Get a value and remove key and value it from dictionary, like *.pop() in Python 
    /// Will fail if key does not exist
    member dd.Pop(k:'K) =
        let ok, v = dd.TryGetValue(k)
        if ok then
            dd.Remove k |>ignore
            v
        else 
            KeyNotFoundException.Raise "Dict.Pop(key): Cannot pop key %A in %A of %d items" k dd dd.Count
            

    /// Returns a (lazy) sequence of key and value tuples
    member _.Items =
        seq { for KeyValue(k, v) in dd -> k, v}
        
    //override dd.ToString() = // covered by NiceString Pretty printer ?
        //stringBuffer {
        //    yield "DefaultDict with "
        //    yield dd.Count.ToString()
        //    yield! "entries"
        //    for k, v in dd.Items  |> Seq.truncate 3 do // add sorting ? print 3 lines??
        //        yield  k.ToString()
        //        yield " : "
        //        yield! v.ToString()
        //    yield "..."
        //    }            


    // -------------------- properties: --------------------------------------

    /// Gets the IEqualityComparer<T> that is used to determine equality of keys for the dictionary.
    member _.Comparer with get() = dd.Comparer

    /// Gets the number of key/value pairs contained in the dictionary
    member _.Count with get() = dd.Count

    /// Gets a collection containing the keys in the dictionary
    member _.Keys with get() = dd.Keys

    /// Gets a collection containing the values in the dictionary
    member _.Values with get() = dd.Values

    // -------------------------------------methods:-------------------------------

    /// Adds the specified key and value to the dictionary.
    member _.Add(k, v) = dd.Add(k, v)

    /// Removes all keys and values from the dictionary
    member _.Clear() = dd.Clear()

    /// Determines whether the dictionary contains the specified key.
    member _.ContainsKey(k) = dd.ContainsKey(k)

    /// Determines whether the dictionary contains a specific value.
    member _.ContainsValue(v) = dd.ContainsValue(v)    

    /// Removes the value with the specified key from the dictionary
    /// see also .Pop(key) method to get the contained value too.
    member _.Remove(k) = dd.Remove(k)

    ///Gets the value associated with the specified key.
    ///As oppsed to Get(key) this does not creat a key if it is missing.
    member _.TryGetValue(k) = dd.TryGetValue(k)

    /// Returns an enumerator that iterates through the dictionary.
    member _.GetEnumerator() = dd.GetEnumerator()

    //---------------------------------------interfaces:-------------------------------------
    // TODO add XML doc str

    interface IEnumerable<KeyValuePair<'K ,'V>> with
        member _.GetEnumerator() = (dd:>IDictionary<'K,'V>).GetEnumerator()

    interface Collections.IEnumerable with // Non generic needed too ? 
        member __.GetEnumerator() = dd.GetEnumerator():> System.Collections.IEnumerator
    
    interface Collections.ICollection with // Non generic needed too ? 
        member _.Count = dd.Count
        
        member _.CopyTo(arr, i) = (dd:>Collections.ICollection).CopyTo(arr, i)

        member _.IsSynchronized= (dd:>Collections.ICollection).IsSynchronized

        member _.SyncRoot= (dd:>Collections.ICollection).SyncRoot
    
    interface ICollection<KeyValuePair<'K,'V>> with 
        member _.Add(x) = (dd:>ICollection<KeyValuePair<'K,'V>>).Add(x)

        member _.Clear() = dd.Clear()

        member _.Remove x = (dd:>ICollection<KeyValuePair<'K,'V>>).Remove x

        member _.Contains x = (dd:>ICollection<KeyValuePair<'K,'V>>).Contains x

        member _.CopyTo(arr, i) = (dd:>ICollection<KeyValuePair<'K,'V>>).CopyTo(arr, i)

        member _.IsReadOnly = false

        member _.Count = dd.Count

    interface IDictionary<'K,'V> with 
        member _.Item 
            with get k = get k
            and  set k v = dd.[k] <- v 
       
        member _.Keys = (dd:>IDictionary<'K,'V>).Keys 

        member _.Values = (dd:>IDictionary<'K,'V>).Values

        member _.Add(k, v) = dd.Add(k, v)

        member _.ContainsKey k = dd.ContainsKey k

        member _.TryGetValue(k, r ) = dd.TryGetValue(k, ref r) 

        member _.Remove(k) = dd.Remove(k)

    interface IReadOnlyCollection<KeyValuePair<'K,'V>> with 
        member _.Count = dd.Count

    interface IReadOnlyDictionary<'K,'V> with 
        member _.Item 
            with get k = get k
       
        member _.Keys = (dd:>IReadOnlyDictionary<'K,'V>).Keys 

        member _.Values = (dd:>IReadOnlyDictionary<'K,'V>).Values

        member _.ContainsKey k = dd.ContainsKey k

        member _.TryGetValue(k, r ) = dd.TryGetValue(k, ref r) 


    //member _.GetObjectData() = dd.GetObjectData()

    //member _.OnDeserialization() = dd.OnDeserialization()

    //member _.Equals() = dd.Equals()

    //member _.GetHashCode() = dd.GetHashCode()

    //member _.GetType() = dd.GetType()


    //interface _.ISerializable() = dd.ISerializable()

    //interface _.IDeserializationCallback() = dd.IDeserializationCallback()
    