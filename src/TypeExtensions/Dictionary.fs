namespace FsEx

open System
open System.Runtime.CompilerServices
open System.Collections.Generic

/// A type alias for System.Collections.Generic.Dictionary<'K,'V> 
type Dict<'K,'V> = Dictionary<'K,'V> // type alias avoids the need to open System.Collections.Generic if FsEx namespace is open // dont use lowercase "dict"

[<AutoOpen>]
module TypeExtensionsDictionary =   

    //[<Extension>] //Error 3246
    type IDictionary<'K,'V> with           
        
        /// Set value at key
        [<Extension>]
        member inline  d.SetValue k v =
            d.[k] <-v        
        
        /// Get value at key
        [<Extension>] 
        member inline d.GetValue k  =
             d.[k]
        
        /// Get a value and remove it from Dictionary, like *.pop() in Python         
        [<Extension>] 
        member inline d.Pop k  =
            let v= d.[k]
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
    
    /// Get value at key from IDictionary
    let get (k:'K) (d:IDictionary<'K,'V>) : 'V = 
        let ok, v = d.TryGetValue(k)
        if ok then  v
        else failwithf "Dict.get faild to find key %A in %A of %d items" k d d.Count
    
    