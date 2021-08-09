namespace FsEx

open System.Runtime.CompilerServices
open System.Collections.Generic


module ExtensionsIDictionary =  
    
    // overides of existing methods are unfortunatly silently ignored and not possible. see https://github.com/dotnet/fsharp/issues/3692#issuecomment-334297164      

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
        member obj.ToNiceString = 
            NiceString.toNiceString obj


