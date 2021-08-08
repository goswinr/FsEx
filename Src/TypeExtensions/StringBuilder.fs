namespace FsEx

open System
open System.Runtime.CompilerServices
open FsEx.SaveIgnore //so that  |> ignore  can only be used on value types


/// Extensions for StringBuilder
// like indexOf(str), append which retuns unit, .. 
[<AutoOpen>]
module AutoOpenExtensionsStringBuilder =  
    type Text.StringBuilder with
        
        /// Like .Append(string) but returnig unit
        member inline sb.append (s:string) = sb.Append(s) |> ignoreObj 
        
        /// Like .Append(char) but returnig unit
        member inline sb.append (c:char) = sb.Append(c) |> ignoreObj 

        /// Like .AppendLine(string) but returnig unit
        member inline sb.appendLine (s:string) = sb.AppendLine(s) |> ignoreObj 
        
        /// Like .AppendLine() but returnig unit
        member inline sb.appendLine() = sb.AppendLine() |> ignoreObj 

        // TODO: add overload with length: sb.IndexOf (c:char, from:int, length:int )

        /// Like .IndexOf for strings, returns -1 if not found        
        member sb.IndexOf (c:char, from:int ) :int = 
            let rec find i = 
                if   i = sb.Length then -1
                elif sb.[i] = c    then i
                else find (i+1)
            find(from)
        
        /// Like .IndexOf for strings, returns -1 if not found
        /// Always uses StringComparison.Ordinal
        member sb.IndexOf (t:string, from:int):int= 
            // could in theory be improved be using a rolling hash value
            // see also Arra.findArray implementation
            // or https://stackoverflow.com/questions/12261344/fastest-search-method-in-stringbuilder
            let ls = sb.Length
            let lt = t.Length
            //printfn "sb :%d t:%d" ls lt
            let rec find ib it = // index in stringbuilder and index in search string
                //printfn "Search at ib:%d %c for it:%d %c" ib sb.[ib] it  t.[it] 
                if  ib > ls-lt+it then -1 // not found! not enough chars left in stringbuilder to match remaining search string
                elif sb.[ib] = t.[it]  then 
                    if it = lt-1 then ib - lt + 1 // found !
                    else find (ib+1) (it+1)
                else find (ib+1-it) 0            
            find from 0

        /// Like .IndexOf for strings, returns -1 if not found        
        member inline sb.IndexOf (c:char) :int =
            sb.IndexOf(c,0)

        /// Like .IndexOf for strings, returns -1 if not found
        /// always StringComparison.Ordinal
        member inline sb.IndexOf (t:string):int= 
            sb.IndexOf(t,0)
        
        member inline sb.Contains (c:char) :bool = 
            sb.IndexOf c <> -1

        member inline sb.Contains (s:string) :bool = 
            sb.IndexOf s <> -1 // 
