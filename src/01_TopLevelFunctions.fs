namespace FsEx

open System

type EXT = Runtime.CompilerServices.ExtensionAttribute

[<assembly:EXT>] do() //http://www.latkin.org/blog/2014/04/30/f-extension-methods-in-roslyn/


[<AutoOpen>]
module  Util = 
    let fail() = failwith "Generic fail (inner exception should show more helpful message)"   

    let inline notNull (value : 'T) = match value with | null -> false  | _ -> true// Fsharp core does it like this too. dont use Obejct.RefrenceEquals
    
    ///Returns the value on the left unless it is null, then it returns the value on the right.
    let inline (|?) a b = if Object.ReferenceEquals(a, null) then b else a // a more fancy version: https://gist.github.com/jbtule/8477768#file-nullcoalesce-fs

    ///Apply function, like |> , but ignore result. 
    ///Return original input
    let inline (|>>) x f =  f x |> ignore ; x     

    ///this ignore only work on Value types, 
    ///Objects and functions need to be ignored with ignoreObj
    ///This is to prevent accidetially ignoreing partially aplied functions tha would returna struct
    let ignore (x:'T when ' T:struct)=()

    let ignoreObj (x:obj)=()

    ///Get first element of triple (tuple of three element)
    let inline t1 (a, _,_) = a
    ///Get second element of triple (tuple of three element)
    let inline t2 (_, b, _) = b
    ///Get third element of triple (tuple of three element)
    let inline t3 (_,_, c) = c    

///Shadows the ignore function to only accept sturucts
///This is to prevent accidetially ignoring partially aplied functions that would return struct
module SaveIgnore = 
    ///this ignore only work on Value types, 
    ///Objects and functions need to be ignored with ignoreObj
    ///This is to prevent accidetially ignoring partially aplied functions that would return struct
    let ignore (x:'T when ' T:struct)=()

    /// ignores any object 
    /// for structs use ignore
    let ignoreObj (x:obj)=()


module IntRef = 
    let inline incr2 i = i := !i+2
    let inline incr3 i = i := !i+3
    let inline incr4 i = i := !i+4
    let inline incr5 i = i := !i+5
    let inline incrBy i x = i := !i + x
    let inline incrByR x i = i := !i + x

    let inline decr  i = i := !i-1 
    let inline decr2 i = i := !i-2
    let inline decr3 i = i := !i-3
    let inline decr4 i = i := !i-4
    let inline decr5 i = i := !i-5
    let inline decrBy i x = i := !i - x
    let inline decrByR x i = i := !i - x                
    ///Increment ref cell and return new incremented integer value
    let inline (!++)  i = incr i; !i 
    let inline setMax i x = if x > !i then i := x
    let inline setMin i x = if x < !i then i := x


