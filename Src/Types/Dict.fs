namespace FsEx

open System
open System.Collections.Generic

/// A type abbreviation for System.Collections.Generic.HashSet<'T> 
/// so that it is available in FsEx namespace
/// ( with a lowercase 's')
type Hashset<'T> = HashSet<'T>

/// A thin wraper over System.Collections.Generic.Dictionary<'K,'V>) with nicer Error messages on accessing missing keys
/// (not the same as lowercase 'dict' in F#)
type Dict<'K,'V when 'K:equality > private (dic : Dictionary<'K,'V>) =
    
    //using inheritance from Dictionary would not work because .Item method is seald and cant have an override

    let get key  =
         let mutable out = Unchecked.defaultof<'V>         
         let found = dic.TryGetValue(key, &out)
         if found then out
         else KeyNotFoundException.Raise "Dict.Get failed to find key %A in %A of %d items" key dic dic.Count

    
    /// create a new empty Dict<'K,'V>
    /// A Dict is a thin wraper over System.Collections.Generic.Dictionary<'K,'V>) with nicer Error messages on accessing missing keys
    /// (not the same as lowercase 'dict' in F#)
    new () = Dict(new Dictionary<'K,'V>())
    
    /// create a new empty Dict<'K,'V> with an IEqualityComparer like HashIdentity.Structural
    /// A Dict is a thin wraper over System.Collections.Generic.Dictionary<'K,'V>) with nicer Error messages on accessing missing keys
    /// (not the same as lowercase 'dict' in F#)
    new (iEqCmp:IEqualityComparer<'K>) = Dict(new Dictionary<'K,'V>(iEqCmp))     

    /// Constructs a new Dict by using the supplied Dictionary<'K,'V> directly, without any copying of items
    static member CreateDirectly (dic:Dictionary<'K,'V> ) = 
        if isNull dic then ArgumentNullException.Raise "Dictionary in Dict.CreateDirectly is null"
        Dict(dic)

    /// Access the underlying Collections.Generic.Dictionary<'K,'V>)
    /// ATTENTION! This is not even a shallow copy, mutating it will also change this Instance of Dict!
    member _.Dictionary = dic

    /// For Index operator .[i]: get or set the value for given key
    member _.Item 
        with get k   = get k        
        and  set k v = dic.[k] <- v
    
    /// Get value for given key
    member _.Get key = get key 

    /// Set value for given key
    member _.Set key value = dic.[key] <- value
    
    /// Get a value and remove key and value it from dictionary, like *.pop() in Python 
    /// Will fail if key does not exist
    member _.Pop(key:'K) =
        let ok, v = dic.TryGetValue(key)
        if ok then
            dic.Remove key |>ignore
            v
        else 
            KeyNotFoundException.Raise "Dict.Pop(key): Failed to pop key %A in %A of %d items" key dic dic.Count
      
    /// Returns a (lazy) sequence of key and value tuples
    member _.Items =
        seq { for kvp in dic -> kvp.Key, kvp.Value}
        
    //override dic.ToString() = // covered by NiceString Pretty printer ?
        //stringBuffer {
        //    yield "DefaultDict with "
        //    yield dic.Count.ToString()
        //    yield! "entries"
        //    for k, v in dic.Items  |> Seq.truncate 3 do // adic sorting ? print 3 lines??
        //        yield  k.ToString()
        //        yield " : "
        //        yield! v.ToString()
        //    yield "..."
        //    }            

    /// Returns a (lazy) sequence of values 
    member _.ValuesSeq with get() =  seq { for kvp in dic -> kvp.Value}
    
    /// Returns a (lazy) sequence of Keys 
    member _.KeysSeq with get() =  seq { for kvp in dic -> kvp.Key}

    /// Determines whether the dictionary is missing the specified key.
    member _.DoesNotContain (key) =  not (dic.ContainsKey(key))

    // ------------------member to match ofSystem.Collections.Generic.Dictionary<'K,'V>-------------

    // -------------------- properties:  --------------------------------------

    /// Gets the IEqualityComparer<T> that is used to determine equality of keys for the dictionary.
    member _.Comparer with get() = dic.Comparer

    /// Gets the number of key/value pairs contained in the dictionary
    member _.Count with get() = dic.Count

    /// Gets a collection containing the keys in the dictionary
    /// same as on System.Collections.Generic.Dictionary<'K,'V>
    member _.Keys with get() = dic.Keys

    /// Gets a collection containing the values in the dictionary
    /// same as on System.Collections.Generic.Dictionary<'K,'V>
    member _.Values with get() = dic.Values
    
    // -------------------------------------methods:-------------------------------

    /// Add the specified key and value to the dictionary.
    member _.Add(key, value) = dic.Add(key, value)

    /// Removes all keys and values from the dictionary
    member _.Clear() = dic.Clear()

    /// Determines whether the dictionary contains the specified key.
    member _.ContainsKey(key) = dic.ContainsKey(key)

    /// Determines whether the dictionary contains a specific value.
    member _.ContainsValue(value) = dic.ContainsValue(value)    

    /// Removes the value with the specified key from the dictionary
    /// see also .Pop(key) method to get the contained value too.
    member _.Remove(key) = dic.Remove(key)

    /// <summary>Lookup an element in the Dict, assigning it to <c>refValue</c> if the element is in the Dict and return true. Otherwise returning <c>false</c> .</summary>
    /// <param name="key">The input key.</param>
    /// <param name="refValue">A reference to the output value.</param>
    /// <returns><c>true</c> if the value is present, <c>false</c> if not.</returns>
    member _.TryGetValue(key:'K , [<Runtime.InteropServices.Out>] refValue : byref<'V>) : bool =  
        let mutable out = Unchecked.defaultof<'V>         
        let found = dic.TryGetValue(key, &out)
        refValue <- out
        found

    /// Returns an enumerator that iterates through the dictionary.
    member _.GetEnumerator() = dic.GetEnumerator()

    //---------------------------------------interfaces:-------------------------------------
    // TODO adic XML doc str

    interface IEnumerable<KeyValuePair<'K ,'V>> with
        member _.GetEnumerator() = (dic:>IDictionary<'K,'V>).GetEnumerator()

    interface Collections.IEnumerable with // Non generic needed too ? 
        member __.GetEnumerator() = dic.GetEnumerator():> System.Collections.IEnumerator
    
    interface Collections.ICollection with // Non generic needed too ? 
        member _.Count = dic.Count
        
        member _.CopyTo(arr, i) = (dic:>Collections.ICollection).CopyTo(arr, i)

        member _.IsSynchronized= (dic:>Collections.ICollection).IsSynchronized

        member _.SyncRoot= (dic:>Collections.ICollection).SyncRoot
    
    interface ICollection<KeyValuePair<'K,'V>> with 
        member _.Add(x) = (dic:>ICollection<KeyValuePair<'K,'V>>).Add(x)

        member _.Clear() = dic.Clear()

        member _.Remove kvp = (dic:>ICollection<KeyValuePair<'K,'V>>).Remove kvp

        member _.Contains kvp = (dic:>ICollection<KeyValuePair<'K,'V>>).Contains kvp

        member _.CopyTo(arr, i) = (dic:>ICollection<KeyValuePair<'K,'V>>).CopyTo(arr, i)

        member _.IsReadOnly = false

        member _.Count = dic.Count

    interface IDictionary<'K,'V> with 
        member _.Item 
            with get k = get k
            and  set k v = dic.[k] <- v 
       
        member _.Keys = (dic:>IDictionary<'K,'V>).Keys 

        member _.Values = (dic:>IDictionary<'K,'V>).Values

        member _.Add(k, v) = dic.Add(k, v)

        member _.ContainsKey k = dic.ContainsKey k

        member _.TryGetValue(key:'K , [<Runtime.InteropServices.Out>] refValue : byref<'V>) : bool =  
            let mutable out = Unchecked.defaultof<'V>         
            let found = dic.TryGetValue(key, &out)
            refValue <- out
            found

        member _.Remove(key) = dic.Remove(key)

    interface IReadOnlyCollection<KeyValuePair<'K,'V>> with 
        member _.Count = dic.Count

    interface IReadOnlyDictionary<'K,'V> with 
        member _.Item 
            with get k = get k
       
        member _.Keys = (dic:>IReadOnlyDictionary<'K,'V>).Keys 

        member _.Values = (dic:>IReadOnlyDictionary<'K,'V>).Values

        member _.ContainsKey(key) = dic.ContainsKey(key)

        member _.TryGetValue(key:'K , [<Runtime.InteropServices.Out>] refValue : byref<'V>) : bool =  
            let mutable out = Unchecked.defaultof<'V>         
            let found = dic.TryGetValue(key, &out)
            refValue <- out
            found
    
    // TODO

    //member _.GetObjectData() = dic.GetObjectData()

    //member _.OnDeserialization() = dic.OnDeserialization()

    //member _.Equals() = dic.Equals()

    //member _.GetHashCode() = dic.GetHashCode()

    //member _.GetType() = dic.GetType()


    //interface _.ISerializable() = dic.ISerializable()

    //interface _.IDeserializationCallback() = dic.IDeserializationCallback()
    