namespace FsEx

open System
open System.Globalization


module  Util = 
    
    let inline notNull (value : 'T) = match value with | null -> false   | _ -> true// Fsharp core does it like this too. dont use RefrenceEquals
    
    ///Returns the value on the left unless it is null, then it returns the value on the right.
    let inline (|?) a b = if Object.ReferenceEquals(a, null) then b else a // a more fancy version: https://gist.github.com/jbtule/8477768#file-nullcoalesce-fs

    ///Apply function ( like |> ) but ignore result. return original input
    let inline (|>>) a f =  f a |> ignore ; a

    let fail() = failwith "Generic fail (inner exception should show more helpful message)"   
 
    ///Get first element of triple (tuple of three element)
    let inline t1 (a, _,_) = a
    ///Get second element of triple (tuple of three element)
    let inline t2 (_, b, _) = b
    ///Get third element of triple (tuple of three element)
    let inline t3 (_,_, c) = c    
     




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


