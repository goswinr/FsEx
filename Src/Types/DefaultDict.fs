namespace FsEx

open System
open System.Collections.Generic



/// <summary>A System.Collections.Generic.Dictionary with default Values that get created upon accessing a key.
/// If accessing a non exiting key , the default function is called to create and set it. 
/// Like defaultdict in Python</summary>    
/// <param name="defaultFun">(unit-&gt;&apos;V): The function to create a default value</param>
/// <param name="dd">(Dictionary&lt;&apos;K,&apos;V&gt;): An existing dictionary that will used as DefaultDict. 
///   It will not be copied, but used directly</param>
type DefaultDict< 'K,'V when 'K:equality > private (defaultFun: unit->'V, dd : Dictionary<'K,'V>) =
    
    //using inheritance from Dictionary would not work because .Item method is seald and cant have an override


    let dGet(k) =
        let ok, v = dd.TryGetValue(k)
        if ok then 
            v
        else 
            let v = defaultFun()
            dd.[k] <- v
            v
    
    /// <summary>A System.Collections.Generic.Dictionary with default Values that get created upon accessing a key.
    /// If accessing a non exiting key , the default function is called to create and set it. 
    /// Like defaultdict in Python</summary>    
    /// <param name="defaultFun">(unit-&gt;&apos;V): The function to create a default value</param>
    new (defaultFun: unit->'V) = DefaultDict( defaultFun, new  Dictionary<'K,'V>() ) 
    
    /// Constructs a new DefaultDict by using the supplied Dictionary<'K,'V> directly, without any copying of items
    static member CreateDirectly (defaultFun: unit->'V) (di:Dictionary<'K,'V> ) =
        if isNull di then ArgumentNullException.Raise "Dictionary in DefaultDict.CreateDirectly is null"
        DefaultDict(defaultFun,di)

    /// Access the underlying Collections.Generic.Dictionary<'K,'V>
    /// ATTENTION! This is not even a shallow copy, mutating it will also change this Instance of DefaultDict!
    member _.Dictionary = dd

    /// For Index operator: get or set the value for given key
    member _.Item 
        with get k   = dGet k        
        and  set k v = dd.[k] <- v
    
    /// Get value for given key. 
    /// Calls defaultFun to get value if key not found.
    /// Also sets key to retuned value.
    /// use .TryGetValue(k) if you dont want a missing key to be created
    member _.Get k = dGet k 
    
    /// Get a value and remove key and value it from dictionary, like *.pop() in Python 
    /// Will fail if key does not exist
    /// Does not set any new key if key is missing
    member dd.Pop(k:'K) =
        let ok, v = dd.TryGetValue(k)
        if ok then
            dd.Remove k |>ignore
            v
        else 
            raise <|  KeyNotFoundException( sprintf "DefaultDict.Pop(key): Cannot pop key %A in %A of %d items" k dd dd.Count)

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
            with get k = dGet k
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
            with get k = dGet k
       
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
    