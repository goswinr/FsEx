namespace FsEx

open System
open System.Text
open System.Runtime.CompilerServices
open FsEx.SaveIgnore //so that  |> ignore  can only be used on value types

[<AutoOpen>]
module TypeExtensionsString =      
    
    type System.String with
        
        /// returns the last valid index in the string
        /// same as: s.Length - 1
        [<Extension>]
        member inline s.LastIndex = 
            if isNull s     then String.FsExStringException.Raise "thistringToSearchIn.LastIndex: Failed to get LastIndex of null String" 
            if s.Length = 0 then String.FsExStringException.Raise "thistringToSearchIn.LastIndex: Failed to get LastIndex of empty String" // TODO or use ArgumentOutOfRangeException ?
            s.Length - 1
        
        /// returns the last character of the string
        /// fails if string is empty
        [<Extension>]
        member inline s.Last = 
            if isNull s     then String.FsExStringException.Raise "string.Last: Failed to get last character of null String"
            if s.Length = 0 then String.FsExStringException.Raise "string.Last: Failed to get last character of empty String"
            s.[s.Length - 1]  
        
        /// returns the second last character of the string
        /// fails if string has less than two characters
        [<Extension>]
        member inline s.SecondLast = 
            if isNull s     then String.FsExStringException.Raise "string.SecondLast: Failed to get second last character from null string."
            if s.Length < 2 then String.FsExStringException.Raise "string.SecondLast: Failed to get second last character of '%s'" s
            s.[s.Length - 2]
        
        /// returns the third last character of the string
        /// fails if string has less than three characters
        [<Extension>]
        member inline s.ThirdLast = 
            if isNull s     then String.FsExStringException.Raise "string.ThirdLast: Failed to get third last character from null string."
            if s.Length < 3 then String.FsExStringException.Raise "string.ThirdLast: Failed to get third last character of '%s'" s
            s.[s.Length - 3]


        /// retuns the last x(int) characters of the string
        /// same as string.LastN
        [<Extension>]
        member inline s.LastX x = 
            if isNull s     then String.FsExStringException.Raise "string.LastX: Failed to get last %d character from null string." x
            if s.Length < x then String.FsExStringException.Raise "string.LastX: Failed to get last %d character of too short String '%s' " x s
            s.Substring(s.Length-x,x) 
         
        /// returns then first character of the string
        /// fails if string is empty
        [<Extension>]
        member inline s.First = 
            if isNull s     then String.FsExStringException.Raise "string.First: Failed to get first character from null string."
            if s.Length = 0 then String.FsExStringException.Raise "string.First: Failed to get first character of empty String"
            s.[0]
        
        /// returns the second character of the string
        /// fails if string has less than two characters
        [<Extension>]
        member inline s.Second = 
            if isNull s     then String.FsExStringException.Raise "string.Second: Failed to get second character from null string."
            if s.Length < 2 then String.FsExStringException.Raise "string.Second: Failed to get second character of '%s'" s
            s.[1]

        /// returns the third character of the string
        /// fails if string has less than three characters
        [<Extension>]
        member inline s.Third = 
            if isNull s     then String.FsExStringException.Raise "string.Third: Failed to get third character from null string."
            if s.Length < 3 then String.FsExStringException.Raise "string.Third: Failed to get third character of '%s'" s
            s.[2]
        

        [<Extension>]
        /// Gets an item in the string by index.
        /// Allows for negtive index too ( -1 is last item,  like Python)
        /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
        member s.GetNeg index = 
            if isNull s then String.FsExStringException.Raise "string.GetNeg: Failed to get character at index %d from string from null string." index 
            let len = s.Length
            let ii =  if index < 0 then len + index else index
            if ii<0 || ii >= len then String.FsExStringException.Raise "string.GetNeg: Failed to get character at index %d from string of %d items: %s" index s.Length s
            s.[ii]   

        [<Extension>]
        /// Any index will return a value.
        /// Rarr is treated as an endless loop in positive and negative direction   
        member s.GetLooped index = 
            if isNull s then String.FsExStringException.Raise "string.GetLooped: Failed to get character at index %d from string from null string." index 
            let len = s.Length
            if len=0 then ArgumentOutOfRangeException.Raise "string.GetLooped: Failed to get character at index %d from string of 0 items" index
            let t = index % len
            let ii = if t >= 0 then t  else t + len 
            s.[ii] 
        
        //member s.GetSlice(startIdx, endIdx) = // overides of existing methods are unfortrunatly silently ignored and not possible. see https://github.com/dotnet/fsharp/issues/3692#issuecomment-334297164

        /// Allows for negative indices too. -1 is last character
        /// Includes end index in string 
        /// for example str.Slice(0,-3) will trim off the last two character from the string 
        [<Extension>]
        member s.Slice(startIdx:int , endIdx:int):string =
            if isNull s then String.FsExStringException.Raise "string.GetSlice: Failed to string.Slice(%d,%d) null string" startIdx  endIdx
            let count = s.Length
            let st  = if startIdx<0 then count+startIdx else startIdx
            let len = if endIdx<0 then count+endIdx-st+1 else endIdx-st+1
    
            if st < 0 || st > count-1 then 
                String.FsExStringException.Raise "string.GetSlice: Start index %d is out of range. Allowed values are -%d upto %d for String '%s' of %d chars" startIdx count (count-1) s count
                
    
            if st+len > count then 
                String.FsExStringException.Raise "string.GetSlice: End index %d is out of range. Allowed values are -%d upto %d for String '%s' of %d chars" startIdx count (count-1) s count
                
            
            if len < 0 then
                let en = if endIdx<0 then count+endIdx else endIdx
                String.FsExStringException.Raise "string.GetSlice: Start index '%A' (= %d) is bigger than end index '%A'(= %d) for String '%s' of %d chars" startIdx st endIdx en s count
              
            
            s.Substring(st,len) 
