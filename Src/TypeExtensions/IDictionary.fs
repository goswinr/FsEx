namespace FsEx

open System
open System.Runtime.CompilerServices
open System.Collections.Generic


//[<AutoOpen>]


module ExtensionsIDictionary =   //TODO add Docstring

    
    type IDictionary<'K,'V> with           
        
        /// Set value at key, with nicer error messages
        [<Extension>]
        member inline  d.SetValue k v =
            try  d.[k] <-v
            with 
                | :? KeyNotFoundException  ->  KeyNotFoundException.Raise "IDictionary.SetValue failed to find key '%A' in %A of %d items (for value: '%A')" k d d.Count v
                | e                        -> raise e
                    
        /// Get value at key, with nicer error messages
        [<Extension>] 
        member inline d.GetValue k  =
             let ok, v = d.TryGetValue(k)
             if ok then  v
             else KeyNotFoundException.Raise "IDictionary.GetValue failed to find key %A in %A of %d items" k d d.Count
        
        
        /// Get a value and remove it from Dictionary, like *.pop() in Python         
        [<Extension>] 
        member inline d.Pop k  =
            let ok, v = d.TryGetValue(k)
            if ok then
                d.Remove k |>ignore
                v
            else 
                KeyNotFoundException.Raise "IDictionary.Pop(key): Failed to pop key %A in %A of %d items" k d d.Count

        /// Returns a lazy seq of key and value tuples
        [<Extension>] 
        member inline d.Items =
            seq { for KeyValue(k, v) in d -> k, v}
        
        /// A property like the ToString() method, 
        /// But with richer formationg for collections
        [<Extension>]  
        member obj.ToNiceString = NiceString.toNiceString obj




/// Static Functions on IDictionary Interface
module Dict = 
    
    /// Get value at key from IDictionary, with nicer Error messages
    let get (key:'K) (d:IDictionary<'K,'V>) : 'V = 
        let ok, v = d.TryGetValue(key)
        if ok then  v
        else KeyNotFoundException.Raise "Dict.get faild to find key %A in %A of %d items" key d d.Count
    
    /// Set value at key from IDictionary
    // just d.[k]<-v
    let set (value:'V) (key:'K) (dict:IDictionary<'K,'V>) =  
        dict.[key]<-value

    
    let tryGet (k:'K) (d:IDictionary<'K,'V>) : 'V option= 
        let ok, v = d.TryGetValue(k)
        if ok then  Some v
        else None
        