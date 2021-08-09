namespace FsEx

open System.Collections.Generic

/// Static Functions on IDictionary Interface
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] //need this so doesn't hide Dict alias class in C# assemblies 
module Dict = 
    
    /// Get value at key from IDictionary, with nicer Error messages
    let get (key:'K) (d:IDictionary<'K,'V>) : 'V = 
        let ok, v = d.TryGetValue(key)
        if ok then  v
        else KeyNotFoundException.Raise "Dict.get faild to find key %A in %A of %d items" key d d.Count
    
    /// Set value at key from IDictionary
    // just d.[k]<-v
    let set (value:'V) (key:'K) (dict:IDictionary<'K,'V>) =  
        dict.[key] <- value
    
    /// Tries to get a value from a IDictionary
    let tryGet (k:'K) (d:IDictionary<'K,'V>) : 'V option= 
        let ok, v = d.TryGetValue(k)
        if ok then  Some v
        else None
    
    /// Create a Dict from seq of key and value pairs
    let create (xs:seq<'K * 'V>) : Dict<'K,'V>= 
        let d = Dict()
        for k,v in xs do 
            d.[k] <- v
        d