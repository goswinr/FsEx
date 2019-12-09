namespace FsEx

open System
open System.Runtime.CompilerServices

type dict<'K,'V> = Collections.Generic.Dictionary<'K,'V> // type alias

[<AutoOpen>]
module TypeExtensionsCollections =   

    //[<Extension>] //Error 3246
    type Collections.Generic.Dictionary<'K,'V> with           
        
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

 