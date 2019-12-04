namespace FsEx

open System


type dict<'K,'V> = Collections.Generic.Dictionary<'K,'V> // type alias

[<AutoOpen>]
module TypeExtensionsCollections =   

    [<EXT>]
    type Collections.Generic.Dictionary<'K,'V> with           
        
        [<EXT>]
        member inline  d.SetValue k v =
            d.[k] <-v        
       
        [<EXT>] 
        member inline d.GetValue k  =
             d.[k]
        
        ///Get a value and remove it from Dictionary, like *.pop() in Python         
        [<EXT>] 
        member inline d.Pop k  =
            let v= d.[k]
            d.Remove(k)|> ignore
            v

        ///Returns a lazy seq of key and value tuples
        [<EXT>] 
        member inline d.Items =
            seq { for KeyValue(k, v) in d -> k, v}

 