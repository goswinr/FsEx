namespace FsEx

open System
open System.Runtime.CompilerServices
open System.Collections.Generic

/// A System.Collections.Generic.Dictionary<'K,'V> 
/// not the same sa lowercase 'dict'
type Dict<'K,'V> =  Dictionary<'K,'V> // type alias avoids the need to open System.Collections.Generic if FsEx namespace is open // dont use lowercase "dict"


[<AutoOpen>]
module TypeExtensionsDictionary =   

    //[<Extension>] //Error 3246
    type IDictionary<'K,'V> with           
        
        /// Set value at key, with nicer error messages
        [<Extension>]
        member inline  d.SetValue k v =
            try  d.[k] <-v
            with 
                | :? KeyNotFoundException  -> raise <|  KeyNotFoundException( sprintf "IDictionary.SetValue failed to find key '%A' in %A of %d items (for value: '%A')" k d d.Count v)
                | e -> raise e
                    
        /// Get value at key, with nicer error messages
        [<Extension>] 
        member inline d.GetValue k  =
             let ok, v = d.TryGetValue(k)
             if ok then  v
             else raise <|  KeyNotFoundException( sprintf "IDictionary.GetValue failed to find key %A in %A of %d items" k d d.Count)
        
        /// Get a value and remove it from Dictionary, like *.pop() in Python         
        [<Extension>] 
        member inline d.Pop k  =
            let v = d.GetValue k
            d.Remove(k)|> ignore
            v

        /// Returns a lazy seq of key and value tuples
        [<Extension>] 
        member inline d.Items =
            seq { for KeyValue(k, v) in d -> k, v}
        
        [<Extension>]  
        /// A property like the ToString() method, 
        /// But with richer formationg for collections
        member obj.ToNiceString = NiceString.toNiceString obj




/// static functions on IDictionary Interface
module Dict = 
    
    /// Get value at key from IDictionary, with nicer Error messages
    let get (k:'K) (d:IDictionary<'K,'V>) : 'V = 
        let ok, v = d.TryGetValue(k)
        if ok then  v
        else failwithf "Dict.get faild to find key %A in %A of %d items" k d d.Count
    
    