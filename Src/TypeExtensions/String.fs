namespace FsEx

open System

open FsEx.SaveIgnore //so that  |> ignore  can only be used on value types


/// AutoOpen Extensions for String,
/// provides DoesNotContain methods
[<AutoOpen>]
module AutoOpenExtensionsString = 

    // This type extension should be alway availabe that is why it is in this Autoopen module
    type System.String with

        /// s.IndexOf(subString,StringComparison.Ordinal) = -1
        member inline s.DoesNotContain(subString:string) = 
            s.IndexOf(subString,StringComparison.Ordinal) = -1

        /// s.IndexOf(chr) = -1
        member inline s.DoesNotContain(chr:char) = 
            s.IndexOf(chr) = -1

        /// s.IndexOf(char) <> -1

        member inline s.Contains(chr:char) =  // this overload does not exist by default
            s.IndexOf(chr) <> -1

        /// Splits a string into substrings.
        /// Empty entries are included.
        /// s.Split( [|chr|] )

        member inline s.Split(chr:char) =  // this overload does not exist by default
            s.Split([|chr|])

/// Adds extension members on System.String
/// for geting first, second, last and similar indices.
/// Also adds functionality for negative indices and s.Slice(startIdx:int , endIdx: int) that works  with negative numbers
module ExtensionsString = 

    /// An Exception for the string functions defined in FsEx
    type FsExStringException(txt:string)= 
        inherit Exception(txt)
        /// Raise the exception with F# printf string formating
        static member inline Raise msg = 
            Printf.kprintf (fun s -> raise (new FsExStringException(s))) msg

    type System.String with

        /// Returns the last valid index in the string
        /// same as: s.Length - 1
        member inline s.LastIndex = 
            if isNull s     then FsExStringException.Raise "FsEx.ExtensionsString: string.LastIndex: Failed to get LastIndex of null String"
            if s.Length = 0 then FsExStringException.Raise "FsEx.ExtensionsString: string.LastIndex: Failed to get LastIndex of empty String"
            s.Length - 1

        /// Returns the last character of the string
        /// fails if string is empty
        member inline s.Last = 
            if isNull s     then FsExStringException.Raise "FsEx.ExtensionsString: string.Last: Failed to get last character of null String"
            if s.Length = 0 then FsExStringException.Raise "FsEx.ExtensionsString: string.Last: Failed to get last character of empty String"
            s.[s.Length - 1]

        /// Returns the second last character of the string
        /// fails if string has less than two characters
        member inline s.SecondLast = 
            if isNull s     then FsExStringException.Raise "FsEx.ExtensionsString: string.SecondLast: Failed to get second last character from null string."
            if s.Length < 2 then FsExStringException.Raise "FsEx.ExtensionsString: string.SecondLast: Failed to get second last character of '%s'" s
            s.[s.Length - 2]

        /// Returns the third last character of the string
        /// fails if string has less than three characters
        member inline s.ThirdLast = 
            if isNull s     then FsExStringException.Raise "FsEx.ExtensionsString: string.ThirdLast: Failed to get third last character from null string."
            if s.Length < 3 then FsExStringException.Raise "FsEx.ExtensionsString: string.ThirdLast: Failed to get third last character of '%s'" s
            s.[s.Length - 3]


        /// Returns the last x(int) characters of the string
        /// same as string.LastN
        member inline s.LastX x = 
            if isNull s     then FsExStringException.Raise "FsEx.ExtensionsString: string.LastX: Failed to get last %d character from null string." x
            if s.Length < x then FsExStringException.Raise "FsEx.ExtensionsString: string.LastX: Failed to get last %d character of too short String '%s' " x s
            s.Substring(s.Length-x,x)

        /// Returns then first character of the string
        /// fails if string is empty
        member inline s.First = 
            if isNull s     then FsExStringException.Raise "FsEx.ExtensionsString: string.First: Failed to get first character from null string."
            if s.Length = 0 then FsExStringException.Raise "FsEx.ExtensionsString: string.First: Failed to get first character of empty String"
            s.[0]

        /// Returns the second character of the string
        /// fails if string has less than two characters
        member inline s.Second = 
            if isNull s     then FsExStringException.Raise "FsEx.ExtensionsString: string.Second: Failed to get second character from null string."
            if s.Length < 2 then FsExStringException.Raise "FsEx.ExtensionsString: string.Second: Failed to get second character of '%s'" s
            s.[1]

        /// Returns the third character of the string
        /// fails if string has less than three characters
        member inline s.Third = 
            if isNull s     then FsExStringException.Raise "FsEx.ExtensionsString: string.Third: Failed to get third character from null string."
            if s.Length < 3 then FsExStringException.Raise "FsEx.ExtensionsString: string.Third: Failed to get third character of '%s'" s
            s.[2]


        /// Gets an item in the string by index.
        /// Allows for negtive index too ( -1 is last item,  like Python)
        /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
        member s.GetNeg index = 
            if isNull s then FsExStringException.Raise "FsEx.ExtensionsString: string.GetNeg: Failed to get character at index %d from string from null string." index
            let len = s.Length
            let ii =  if index < 0 then len + index else index
            if ii<0 || ii >= len then FsExStringException.Raise "FsEx.ExtensionsString: string.GetNeg: Failed to get character at index %d from string of %d items: %s" index s.Length s
            s.[ii]

        /// Any index will return a value.
        /// Rarr is treated as an endless loop in positive and negative direction
        member s.GetLooped index = 
            if isNull s then FsExStringException.Raise "FsEx.ExtensionsString: string.GetLooped: Failed to get character at index %d from string from null string." index
            let len = s.Length
            if len=0 then ArgumentOutOfRangeException.Raise "FsEx.ExtensionsString: string.GetLooped: Failed to get character at index %d from string of 0 items" index
            let t = index % len
            let ii = if t >= 0 then t  else t + len
            s.[ii]

        //member s.GetSlice(startIdx, endIdx) = // overides of existing methods are unfortrunatly silently ignored and not possible. see https://github.com/dotnet/fsharp/issues/3692#issuecomment-334297164

        /// Allows for negative indices too. -1 is last character
        /// Includes end index in string
        /// for example str.Slice(0,-3) will trim off the last two character from the string
        member s.Slice(startIdx:int , endIdx:int):string = 
             // overides of existing methods are unfortunatly silently ignored and not possible. see https://github.com/dotnet/fsharp/issues/3692#issuecomment-334297164
            if isNull s then FsExStringException.Raise "FsEx.ExtensionsString: string.GetSlice: Failed to string.Slice(%d,%d) null string" startIdx  endIdx
            let count = s.Length
            let st  = if startIdx<0 then count+startIdx else startIdx
            let len = if endIdx<0 then count+endIdx-st+1 else endIdx-st+1

            if st < 0 || st > count-1 then
                FsExStringException.Raise "FsEx.ExtensionsString: string.GetSlice: Start index %d is out of range. Allowed values are -%d upto %d for String '%s' of %d chars" startIdx count (count-1) s count


            if st+len > count then
                FsExStringException.Raise "FsEx.ExtensionsString: string.GetSlice: End index %d is out of range. Allowed values are -%d upto %d for String '%s' of %d chars" startIdx count (count-1) s count


            if len < 0 then
                let en = if endIdx<0 then count+endIdx else endIdx
                FsExStringException.Raise "FsEx.ExtensionsString: string.GetSlice: Start index '%A' (= %d) is bigger than end index '%A'(= %d) for String '%s' of %d chars" startIdx st endIdx en s count

            s.Substring(st,len)
