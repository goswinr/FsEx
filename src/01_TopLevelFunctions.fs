namespace FsEx

open System

type EXT = Runtime.CompilerServices.ExtensionAttribute

[<assembly:EXT>] do() //http://www.latkin.org/blog/2014/04/30/f-extension-methods-in-roslyn/

/// General Utility functions
/// module is set to auto open
[<AutoOpen>]
module  Util = 
    
    /// a quick way to throw an exeption.
    /// for use in temporary scripts when you are too lazy to do a proper exception.
    let inline fail() = failwith "Quick fail (inner exception should show more helpful message)" 
    
    /// throws an exeption with 'msg' as Error message if 'value' is false.
    /// this function is usefull to follow up on any methods that return booleans indication sucess or failure
    let inline failIfFalse (msg:string) (value :bool)  = if not value then failwithf "failIfFalse: %s " msg 
    
    /// throws an exeption with 'msg' as Error message if 'value' is null.
    /// this function is usefull to doing many null checks without adding lots if clauses and lots of indenting
    let inline failIfNull (msg:string) (value :'T) = 
        match value with 
        | null -> failwithf "<null> in %s" msg  
        | _ -> ()
    
    /// retuns false if the value is null.
    /// the opposit of isNull
    let inline notNull (value :'T when 'T: null) = // Fsharp core does it like this too. don't use Obejct.RefrenceEquals (because of Generics)
        match value with 
        | null -> false  
        | _ -> true     
    
    /// Returns the value on the left unless it is null, then it returns the value on the right.
    let inline (|?) (a:'T) (b:'T)  = // a more fancy version: https://gist.github.com/jbtule/8477768#file-nullcoalesce-fs
        match a with 
        | null -> b  
        | _    -> a // if Object.ReferenceEquals(a, null) then b else a   

    /// Apply function, like |> , but ignore result. 
    /// Return original input
    let inline (|>>) x f =  f x |> ignore ; x 

    /// Get first element of Triple (Tuple of three elements)
    let inline t1 (a,_,_) = a

    /// Get second element of Triple (Tuple of three elements)
    let inline t2 (_,b,_) = b

    /// Get third element of Triple (Tuple of three elements)
    let inline t3 (_,_,c) = c    


    /// Any int will give a valid index for given collection size.
    /// division remainder will be used i % len
    /// e.g.: -1 is  last item 
    /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
    let inline saveIdx i len =
        let rest = i % len
        if rest >= 0 then rest // does not fail on -4 for len 4
        else len + rest

    /// Converts negative indices to positive ones
    /// correctet results from -len up to len-1
    /// e.g.: -1 is  last item .
    /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
    let inline negIdx i len =
        let ii =  if i<0 then len+i else i
        if ii<0 || ii >= len then failwithf "Util.negIdx: Cannot find right index for %d (for items count %d)" i len
        ii
    
    /// If condition is true return f(x) else just x
    let inline ifDo condition (f:'T->'T)  (x:'T) = 
        if condition then f x else x


/// Shadows the ignore function to only accept structs
/// This is to prevent accidetially ignoring partially aplied functions that would return struct
module SaveIgnore = 
    
    /// This ignore only work on Value types, 
    /// Objects and functions need to be ignored with ignoreObj
    /// This is to prevent accidetially ignoring partially aplied functions that would return struct
    let inline ignore (x:'T when 'T: struct)=()

    /// Ignores any object (and struct)
    /// For structs use 'ignore'
    let inline ignoreObj (x:obj)=()

/// Functions and operator !++ to deal with integer ref objects
module IntRef = 

    /// Increment a ref cell and return new incremented integer value
    let inline (!++)  i = incr i; !i 

    /// Increment a ref cell by two 
    let inline incr2 i = i := !i+2

    /// Increment a ref cell by three 
    let inline incr3 i = i := !i+3

    /// Increment a ref cell by four
    let inline incr4 i = i := !i+4

    /// Increment a ref cell by a given int
    let inline incrBy i (x:int) = i := !i + x

    //let inline incrByR (x:int) i = i := !i + x // useful ?
    
    /// Decrement a ref cell by one
    let inline decr  i = i := !i-1 
    
    /// Decrement a ref cell by two
    let inline decr2 i = i := !i-2
    
    /// Decrement a ref cell by three
    let inline decr3 i = i := !i-3
    
    /// Decrement a ref cell by four
    let inline decr4 i = i := !i-4
    
    /// Decrement a ref cell by a given int
    let inline decrBy i (x:int) = i := !i - x    
    
    //let inline decrByR (x:int) i = i := !i - x           // useful ?
    
    /// set ref cell to given int if it is bigger than current value
    let inline setMax i (x:int) = if x > !i then i := x

    /// set ref cell to given int if it is smaller than current value
    let inline setMin i (x:int) = if x < !i then i := x


