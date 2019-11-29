namespace FsEx

open System.Collections.Generic



///<summary>A System.Collections.Generic.Dictionary with default Values that get created upon accessing a key.
/// If accessing a non exiting key , the default function is called to create and set it. 
/// Like defaultdict in Python</summary>    
///<param name="defaultFun">(unit->'V): The function to create a default Value</param>
type DefaultDict< 'K,'V when 'K:equality > (defaultFun: unit->'V) =
    
    let dd = Dictionary<'K,'V>() // the internal Dictionary

    let dGet(k) =
        let ok, v = dd.TryGetValue(k)
        if ok then 
            v
        else 
            let v = defaultFun()
            dd.[k] <- v
            v

    member _.Item 
        with get k   = dGet k        
        and  set k v = dd.[k] <- v
    
    /// Get a value and remove it from Dictionary, like *.pop() in Python 
    member dd.Pop(k:'K) =
        let ok, v = dd.TryGetValue(k)
        if ok then
            dd.Remove k |>ignore
            v
        else 
            failwithf "DefaultDict: Cannot pop key '%A' from %s " k (NiceString.toNiceString dd)

    /// Returns a seq of key and value tuples
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
            

    // TODO add XML doc str

    // properties

    member _.Comparer with get() = dd.Comparer

    member _.Count with get() = dd.Count

    member _.Keys with get() = dd.Keys

    member _.Values with get() = dd.Values

    // methods

    member _.Add(k, v) = dd.Add(k, v)

    member _.Clear() = dd.Clear()

    member _.ContainsKey(k) = dd.ContainsKey(k)

    member _.ContainsValue(v) = dd.ContainsValue(v)    

    member _.Remove(k) = dd.Remove(k)

    member _.TryGetValue(k) = dd.TryGetValue(k)

    member _.GetEnumerator() = dd.GetEnumerator()

    //interfaces

    interface IEnumerable<KeyValuePair<'K ,'V>> with
        member _.GetEnumerator() = (dd:>IDictionary<'K,'V>).GetEnumerator()

    interface System.Collections.IEnumerable with // Non generic needed too ? 
        member __.GetEnumerator() = dd.GetEnumerator():> System.Collections.IEnumerator
    
    //interface System.Collections.ICollection with // Non generic needed too ? 
        
    
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

    //


    (*
    interface _.IDictionary`2() = dd.IDictionary`2()

    interface _.ICollection`1() = dd.ICollection`1()

    interface _.IEnumerable`1() = dd.IEnumerable`1()

    interface _.IEnumerable() = dd.IEnumerable()

    interface _.IDictionary() = dd.IDictionary()

    interface _.ICollection() = dd.ICollection()

    interface _.IReadOnlyDictionary`2() = dd.IReadOnlyDictionary`2()

    interface _.IReadOnlyCollection`1() = dd.IReadOnlyCollection`1()

    interface _.ISerializable() = dd.ISerializable()

    interface _.IDeserializationCallback() = dd.IDeserializationCallback()
    *)