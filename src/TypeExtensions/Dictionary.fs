namespace FsEx

open System
open System.Runtime.CompilerServices
open System.Collections.Generic

type dict<'K,'V> = Dictionary<'K,'V> // type alias

[<AutoOpen>]
module TypeExtensionsDictionary =   

    //[<Extension>] //Error 3246
    type IDictionary<'K,'V> with           
        
        [<Extension>]
        member inline  d.SetValue k v =
            d.[k] <-v        
       
        [<Extension>] 
        member inline d.GetValue k  =
             d.[k]
        
        ///Get a value and remove it from Dictionary, like *.pop() in Python         
        [<Extension>] 
        member inline d.Pop k  =
            let v= d.[k]
            d.Remove(k)|> ignore
            v

        ///Returns a lazy seq of key and value tuples
        [<Extension>] 
        member inline d.Items =
            seq { for KeyValue(k, v) in d -> k, v}

/// static functions on IDictionary Interface
module Dict = 
    
    let get (k:'K) (d:IDictionary<'K,'V>) : 'V = 
        let ok, v = d.TryGetValue(k)
        if ok then  v
        else failwithf "Dict.get faild to find key %A in %A of %d items" k d d.Count
    
    