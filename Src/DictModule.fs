﻿namespace FsEx

open System
open System.Collections.Generic

/// Static Functions on IDictionary Interface
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] //need this so doesn't hide Dict alias class in C# assemblies
[<RequireQualifiedAccess>]
module Dict = 

    /// Get value at key from IDictionary, with nicer Error messages
    let get (key:'Key) (dic:IDictionary<'Key,'Value>) : 'Value = 
        let ok, v = dic.TryGetValue(key)
        if ok then  v
        else KeyNotFoundException.Raise "Dict.get failed to find key %A in %A of %d items" key dic dic.Count

    /// Set value at key from IDictionary
    /// just d.[k]<-v
    let set (key:'Key) (value:'Value) (dic:IDictionary<'Key,'Value>) = 
        dic.[key] <- value

    /// Tries to get a value from a IDictionary
    let tryGet (k:'Key) (dic:IDictionary<'Key,'Value>) : 'Value option= 
        let ok, v = dic.TryGetValue(k)
        if ok then  Some v
        else None

    /// Create a Dict from seq of key and value pairs
    let create (xs:seq<'Key * 'Value>) : Dict<'Key,'Value>= 
        let dic = Dict()
        for k,v in xs do
            dic.[k] <- v
        dic
    
    /// Set value only if key does not exist yet.
    /// Returns false if key already exist, does not set value in this case
    /// Same as <c>Dict.addOnce key value dic</c>
    let setIfKeyAbsent  (key:'Key) (value:'Value)  (dic:IDictionary<'Key,'Value>) = 
        match box key with // or https://stackoverflow.com/a/864860/969070
        | null -> ArgumentNullException.Raise "Dict.setOnce key is null "
        | _ ->
            if dic.ContainsKey key then
                false
            else
                dic.[key] <- value
                true

    /// Set value only if key does not exist yet.
    /// Returns false if key already exist, does not set value in this case
    /// Same as <c>Dict.setOnce key value dic</c>
    let addIfKeyAbsent  (key:'Key) (value:'Value)  (dic:IDictionary<'Key,'Value>) = 
        match box key with // or https://stackoverflow.com/a/864860/969070
        | null -> ArgumentNullException.Raise "Dict.addOnce key is null "
        | _ ->
            if dic.ContainsKey key then
                false
            else
                dic.[key] <- value
                true
    
    /// If the key ist not present calls the default function, set it as value at the key and return the value.
    /// This function is an alternative to the DefaultDict type. Use it if you need to provide a custom implemantation of the default function depending on the key.
    let getOrSetDefault (getDefault:'Key -> 'Value) (key:'Key)  (dic:IDictionary<'Key,'Value>)   = 
        match box key with // or https://stackoverflow.com/a/864860/969070
        | null -> ArgumentNullException.Raise "Dict.getOrSetDefault key is null "
        | _ ->
            match dic.TryGetValue(key) with
            |true, v-> v
            |false, _ ->
                let v = getDefault(key)
                dic.[key] <- v
                v
    /// If the key ist not present set it as value at the key and return the value.    
    let getOrSetDefaultValue (defaultValue: 'Value) (key:'Key)  (dic:IDictionary<'Key,'Value>)   = 
        match box key with // or https://stackoverflow.com/a/864860/969070
        | null -> ArgumentNullException.Raise "Dict.getOrSetDefaultValue key is null "
        | _ ->
            match dic.TryGetValue(key) with
            |true, v-> v
            |false, _ ->
                let v = defaultValue
                dic.[key] <- v
                v

    /// Get a value and remove key and value it from dictionary, like *.pop() in Python.
    /// Will fail if key does not exist
    let pop(key:'Key)  (dic:IDictionary<'Key,'Value>) = 
        match box key with // or https://stackoverflow.com/a/864860/969070
        | null -> ArgumentNullException.Raise "Dict.pop(key) key is null"
        | _ ->
            let ok, v = dic.TryGetValue(key)
            if ok then
                dic.Remove key |>ignore
                v
            else
                KeyNotFoundException.Raise "Dict.pop(key): Failed to pop key %A in %A of %d items" key dic dic.Count

    /// Returns a (lazy) sequence of key and value tuples
    let items(dic:IDictionary<'Key,'Value>) = 
        seq { for kvp in dic -> kvp.Key, kvp.Value}

    /// Returns a (lazy) sequence of values
    let valuesSeq (dic:IDictionary<'Key,'Value>) =  
        seq { for kvp in dic -> kvp.Value}

    /// Returns a (lazy) sequence of Keys
    let keysSeq (dic:IDictionary<'Key,'Value>) =  
        seq { for kvp in dic -> kvp.Key}
    
    /// Iterate over keys and values of a Dict
    let iter (f: 'Key -> 'Value -> unit) (dic:IDictionary<'Key,'Value>) = 
        for kvp in dic do 
            f kvp.Key kvp.Value

    /// Map over keys and values of a Dict
    let map(f: 'Key -> 'Value -> 'T) (dic:IDictionary<'Key,'Value>) = 
        seq { for kvp in dic do
                f kvp.Key kvp.Value}