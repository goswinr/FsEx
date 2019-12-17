namespace FsEx

open System

type EXT = Runtime.CompilerServices.ExtensionAttribute

[<assembly:EXT>] do() //http://www.latkin.org/blog/2014/04/30/f-extension-methods-in-roslyn/


[<AutoOpen>]
module  Util = 
    let fail() = failwith "Generic fail (inner exception should show more helpful message)"   
    let failIfFalse msg x = if not x then failwithf "failIfFalse: %s " msg 

    let inline notNull (value :'T) = match value with | null -> false  | _ -> true// Fsharp core does it like this too. dont use Obejct.RefrenceEquals
    
    ///Returns the value on the left unless it is null, then it returns the value on the right.
    let inline (|?) a b = if Object.ReferenceEquals(a, null) then b else a
    //let inline (|?) (a:'T) (b:'T)  = match a with | null -> b  | _ -> a // if Object.ReferenceEquals(a, null) then b else a // or make generic using match ?  // a more fancy version: https://gist.github.com/jbtule/8477768#file-nullcoalesce-fs

    ///Apply function, like |> , but ignore result. 
    ///Return original input
    let inline (|>>) x f =  f x |> ignore ; x 

    ///Get first element of Triple (Tuple of three elements)
    let inline t1 (a,_,_) = a
    ///Get second element of Triple (Tuple of three elements)
    let inline t2 (_,b,_) = b
    ///Get third element of Triple (Tuple of three elements)
    let inline t3 (_,_,c) = c    


    /// Any int will give a valid index for given collection size.
    /// division remainder will be used i % len
    /// e.g.: -1 is  last item 
    let inline saveIdx i len =
        let rest = i % len
        if rest >= 0 then rest // does not fail on -4 for len 4
        else len + rest

    /// Converts negative indices to positive ones
    /// correctet results from -len up to len-1
    /// e.g.: -1 is  last item .
    let inline negIdx i len =
        let ii =  if i<0 then len+i else i
        if ii<0 || ii >= len then failwithf "Cannot get (or set) index %d of %d items in array, List, string or seq." i len
        ii
    
    ///If condition is true return f(x) else just x
    let inline ifDo condition (f:'T->'T)  (x:'T) = if condition then f x else x


///Shadows the ignore function to only accept sturucts
///This is to prevent accidetially ignoring partially aplied functions that would return struct
module SaveIgnore = 
    
    ///This ignore only work on Value types, 
    ///Objects and functions need to be ignored with ignoreObj
    ///This is to prevent accidetially ignoring partially aplied functions that would return struct
    let inline ignore (x:'T when ' T:struct)=()

    /// Ignores any object (and struct)
    /// For structs use 'ignore'
    let inline ignoreObj (x:obj)=()


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


