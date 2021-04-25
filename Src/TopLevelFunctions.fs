﻿namespace FsEx

open System
open System.IO
open System.Runtime.CompilerServices
open System.Text
open System.Collections.Generic

type EXT = Runtime.CompilerServices.ExtensionAttribute

[<assembly:EXT>] do() // mark this assembly as extension assembly http://www.latkin.org/blog/2014/04/30/f-extension-methods-in-roslyn/


/// This module is set to auto open. 
/// Static Extension methods on Exceptions to cal Excn.Raise "%A" x with F# printf string formating
[<AutoOpen>] // so that extension become availale on opening FsEx
module ExtensionsExceptions =    
    
    // type FsExStringException is now defined in String Module

    type ArgumentException with
        /// Raise ArgumentException with F# printf string formating
        /// this is also the base class of ArgumentOutOfRangeException and ArgumentNullException
        [<Extension>] static member inline RaiseBase msg =  Printf.kprintf (fun s -> raise (ArgumentException(s))) msg 

    type ArgumentOutOfRangeException with
        /// Raise ArgumentOutOfRangeException with F# printf string formating
        [<Extension>] static member inline Raise msg =  Printf.kprintf (fun s -> raise (ArgumentOutOfRangeException(s))) msg 

    type ArgumentNullException with
        /// Raise ArgumentNullException with F# printf string formating
        [<Extension>] static member inline Raise msg =  Printf.kprintf (fun s -> raise (ArgumentNullException(s))) msg 

    type IndexOutOfRangeException with
        /// Raise IndexOutOfRangeException with F# printf string formating
        [<Extension>] static member inline Raise msg =  Printf.kprintf (fun s -> raise (IndexOutOfRangeException(s))) msg    

    type KeyNotFoundException with
        /// Raise KeyNotFoundException with F# printf string formating
        [<Extension>] static member inline Raise msg =  Printf.kprintf (fun s -> raise (KeyNotFoundException(s))) msg
                        
    type FileNotFoundException with
        /// Raise FileNotFoundException with F# printf string formating
        [<Extension>] static member inline Raise msg =  Printf.kprintf (fun s -> raise (FileNotFoundException(s))) msg

    type DirectoryNotFoundException with
        /// Raise DirectoryNotFoundException with F# printf string formating
        [<Extension>] static member inline Raise msg =  Printf.kprintf (fun s -> raise (DirectoryNotFoundException(s))) msg

    

/// This module is set to auto open. 
/// General Utility functions
[<AutoOpen>] // so that extension become availale on opening FsEx
module  Util =     
    
    /// a quick way to throw an exception.
    /// for use in temporary scripts when you are too lazy to do a proper exception.
    let inline fail() = failwith "Quick fail (inner exception should show more helpful message)" 
    
    /// throws an exception with 'msg' as Error message if 'value' is false.
    /// this function is usefull to follow up on any methods that return booleans indication sucess or failure
    let inline failIfFalse (msg:string) (value :bool) :unit = 
        if not value then failwithf "failIfFalse: %s " msg 
    
    /// throws an exception with 'msg' as Error message if 'value' is null.
    /// this function is usefull to doing many null checks without adding lots if clauses and lots of indenting
    let inline failIfNull (msg:string) (value :'T when 'T: null) : unit = 
        match value with 
        | null -> ArgumentNullException.Raise "<null> in FsEx.Util.failIfNull: %s" msg  
        | _ -> ()

    /// throws an exception with 'msg' as Error message if string is null or empty    
    let inline failIfNullOrEmpty (failMsg:string) (stringToCheck :string) :unit = 
        match stringToCheck with 
        | null -> ArgumentNullException.Raise "Null string in FsEx.Util.failIfNullOrEmpty: %s" failMsg 
        | ""   -> ArgumentException.RaiseBase "Empty string in FsEx.Util.failIfNullOrEmpty: %s" failMsg 
        | _ -> ()        
       
        /// throws an exception with 'msg' as Error message if string is null or empty    
    let inline failIfEmptyGuid (failMsg:string) (guidToCheck :Guid) :unit = 
        if guidToCheck = Guid.Empty then 
            ArgumentException.RaiseBase "Empty Guid in  FsEx.Util.failIfEmptyGuid: %s" failMsg
 
    /// returns false if the value is null.
    /// the opposit of isNull
    let inline notNull (value :'T when 'T: null) = // Fsharp core does it like this too. don't use Obejct.RefrenceEquals (because of Generics)
        match value with 
        | null -> false  
        | _ -> true    
    
    /// returns false if the Guid is Empty.
    let inline notEmptyGuid (g :Guid) = 
        g <> Guid.Empty
    
    /// returns true if the Guid is Empty.
    let inline isEmptyGuid (g :Guid) = 
        g = Guid.Empty

    /// Returns maybeNullValue if it is NOT null, else alternativeValue. 
    let inline ifNull (alternativeValue:'T) (maybeNullValue:'T)  = match maybeNullValue with null -> alternativeValue | _ -> maybeNullValue  

    /// Null coalesceing:
    /// Returns the value on the left unless it is null, then it returns the value on the right.
    let inline (|?) (a:'T) (b:'T)  = // a more fancy version: https://gist.github.com/jbtule/8477768#file-nullcoalesce-fs
        match a with 
        | null -> b  
        | _    -> a // if Object.ReferenceEquals(a, null) then b else a   

    /// use |>! instead
    /// Apply function, like |> , but ignore result. 
    /// Return original input
    /// let inline (|>>) x f =  f x |> ignore ; x 
    [<Obsolete>]
    let inline (|>>) x f =  f x |> ignore ; x 

    /// Apply function, like |> , but ignore result. 
    /// Return original input
    /// let inline (|>!) x f =  f x |> ignore ; x 
    /// be aware of correct indenting see:
    /// https://stackoverflow.com/questions/64784154/indentation-change-after-if-else-expression-not-taken-into-account
    let inline (|>!) x f =  f x |> ignore ; x  //https://twitter.com/GoswinR/status/1316988132932407296

    /// Get first element of Triple (Tuple of three elements)
    let inline t1 (a,_,_) = a

    /// Get second element of Triple (Tuple of three elements)
    let inline t2 (_,b,_) = b

    /// Get third element of Triple (Tuple of three elements)
    let inline t3 (_,_,c) = c    


    /// Converts negative indices to positive ones
    /// correctet results from -len up to len-1
    /// e.g.: -1 is  last item .
    /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
    let inline negIdx i len =
        let ii =  if i < 0 then len+i else i
        if ii<0 || ii >= len then IndexOutOfRangeException.Raise "Util.negIdx: Bad index %d for items count %d." i len
        ii
    
    /// Any int will give a valid index for given collection size.
    /// Converts negative indices to positive ones and loops to start after last index is reached
    /// returns a valid index for a colcction of 'len' items for any integer
    let inline negIdxLooped i len =        
        let t = i % len
        if t >= 0 then t 
        else           t + len 

    /// Any int will give a valid index for given collection size.
    /// division remainder will be used i % len
    /// e.g.: -1 is  last item 
    /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
    let inline saveIdx i len = negIdxLooped i len 
      
    
    /// If condition is true return f(x) else just x
    let inline ifDo condition (f:'T->'T)  (x:'T) = 
        if condition then f x else x
    
    /// Caches the results of a function in Map
    /// the arument to thge function will be used as key in the Map
    /// can be unit or null too
    let memoize f =
        let cache = ref Map.empty
        fun x ->
            match (!cache).TryFind(x) with
            | Some res -> res
            | None ->
                let res = f x
                cache := (!cache).Add(x,res)
                res


    /// generic parser that infers desired return type 
    let inline tryParse<'a when 'a: (static member TryParse: string * byref<'a> -> bool)> x =
        // https://twitter.com/mvsmal/status/1317020301046259712
        let mutable res = Unchecked.defaultof<'a> 
        if (^a: (static member TryParse: string * byref<'a> -> bool) (x, &res)) 
        then Some res  
        else None 
    
    /// generic parser that infers desired return type or fails with ArgumentException 
    let inline parse<'a when 'a: (static member TryParse: string * byref<'a> -> bool)> x =
        let mutable res = Unchecked.defaultof<'a> 
        if (^a: (static member TryParse: string * byref<'a> -> bool) (x, &res)) 
        then res  
        else ArgumentException.RaiseBase "Failed to parse %A to a %A" x (res.GetType())



/// Shadows the ignore function to only accept structs
/// This is to prevent accidetially ignoring partially aplied functions that would return struct
module SaveIgnore = 
    
    /// This ignore only work on Value types, 
    /// Objects and functions need to be ignored with 'ignoreObj'
    /// This is to prevent accidetially ignoring partially aplied functions that would return struct
    let inline ignore (x:'T when 'T: struct) = ()

    /// Ignores any object (and struct)
    /// For structs use 'ignore'
    let inline ignoreObj (x:obj) = ()

/// Functions to deal with integer ref objects
module IntRef = 

    /// Increment a ref cell by a given int
    let inline incrBy i (x:int) = i := !i + x
    
    /// Decrement a ref cell by one
    let inline decr  i = i := !i-1 
    
    /// Decrement a ref cell by a given int    
    let inline decrBy i (x:int) = i := !i - x        
    
    /// set ref cell to given int if it is bigger than current value
    let inline setMax i (x:int) = if x > !i then i := x

    /// set ref cell to given int if it is smaller than current value
    let inline setMin i (x:int) = if x < !i then i := x

    // Increment a ref cell and return new incremented integer value
    //[<Obsolete>]
    //let inline (!++)  i = incr i; !i 

    /// Increment a ref cell by two
    [<Obsolete>]
    let inline incr2 i = i := !i+2

    /// Increment a ref cell by three 
    [<Obsolete>]
    let inline incr3 i = i := !i+3

    /// Increment a ref cell by four
    [<Obsolete>]
    let inline incr4 i = i := !i+4

    //let inline incrByR (x:int) i = i := !i + x  // useful ?
    //let inline decrByR (x:int) i = i := !i - x   // useful ?

    /// Decrement a ref cell by two
    [<Obsolete>]
    let inline decr2 i = i := !i-2
    
    /// Decrement a ref cell by three
    [<Obsolete>]
    let inline decr3 i = i := !i-3
    
    /// Decrement a ref cell by four
    [<Obsolete>]
    let inline decr4 i = i := !i-4

   

/// Functions to deal with float ref objects
module FloatRef = 
        
    /// Increment a ref cell by a given int
    let inline incrBy i (x:float) = i := !i + x
        
    /// Decrement a ref cell by a given int    
    let inline decrBy i (x:float) = i := !i - x    
        
    //let inline decrByR (x:int) i = i := !i - x           // useful ?
        
    /// set ref cell to given int if it is bigger than current value
    let inline setMax i (x:float) = if x > !i then i := x
    
    /// set ref cell to given int if it is smaller than current value
    let inline setMin i (x:float) = if x < !i then i := x

