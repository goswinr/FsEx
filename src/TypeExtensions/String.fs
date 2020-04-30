namespace FsEx

open System
open System.Runtime.CompilerServices
open FsEx.SaveIgnore

[<AutoOpen>]
module TypeExtensionsString =   
    
    //[<Extension>] //Error 3246
    type System.String with
        
        /// like this.Length - 1
        [<Extension>]
        member inline this.LastIndex = 
            if this.Length = 0 then failwithf "this.LastIndex: Cannot get LastIndex of empty String"
            this.Length - 1
        
        
        [<Extension>]
        member inline s.Last = 
            if s.Length = 0 then failwithf "this.Last: Cannot get Last item of empty String"
            s.[s.Length - 1]  

        [<Extension>]
        member inline this.SecondLast = 
            if this.Length < 2 then failwithf "this.SecondLast: Can not get SecondLast item of '%s'" this
            this.[this.Length - 2]

        [<Extension>]
        member inline this.ThirdLast = 
            if this.Length < 3 then failwithf "this.ThirdLast: Can not get ThirdLast item of '%s'" this
            this.[this.Length - 3]

        [<Extension>]
        /// get last x characters of string
        member s.LastX x = 
            if s.Length < x then failwithf "this.LastX: Cannot get last %d item of too short String '%s' " x s
            s.Substring(s.Length-x,x) 
            
        [<Extension>]
        member inline this.First = 
            if this.Length = 0 then failwithf "this.First: Can not get First item of empty String"
            this.[0]

        [<Extension>]
        member inline this.Second = 
            if this.Length < 2 then failwithf "this.Second: Can not get Second item of '%s'" this
            this.[1]

        [<Extension>]
        member inline this.Third = 
            if this.Length < 3 then failwithf "this.Third: Can not get Third item of '%s'" this
            this.[2]
        
        [<Extension>] 
        /// Allows for negtive index too (like Python)
        member this.GetItem index = 
            let i = negIdx index this.Length
            this.[i]
    
        
        //member this.GetSlice(startIdx, endIdx) = // overides of existing methods are unfurtrunatly silently ignored and not possible. see https://github.com/dotnet/fsharp/issues/3692#issuecomment-334297164

        /// Allows for negative indices too.
        [<Extension>]
        member s.Slice(startIdx:int , endIdx:int):string =
            let count = s.Length
            let st  = if startIdx<0 then count+startIdx else startIdx
            let len = if endIdx<0 then count+endIdx-st+1 else endIdx-st+1
    
            if st < 0 || st > count-1 then 
                let err = sprintf "GetSlice: Start index %d is out of range. Allowed values are -%d upto %d for String '%s' of %d chars" startIdx count (count-1) s count
                raise (IndexOutOfRangeException(err))
    
            if st+len > count then 
                let err = sprintf "GetSlice: End index %d is out of range. Allowed values are -%d upto %d for String '%s' of %d chars" startIdx count (count-1) s count
                raise (IndexOutOfRangeException(err)) 
            
            if len < 0 then
                let en = if endIdx<0 then count+endIdx else endIdx
                let err = sprintf "GetSlice: Start index '%A' (= %d) is bigger than end index '%A'(= %d) for String '%s' of %d chars" startIdx st endIdx en s count
                raise (IndexOutOfRangeException(err)) 
            
            s.Substring(st,len) 

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] //need this so doesn't hide String class in C# assemblies (should consider for other extension modules as well)
module String =

    /// Returns everytrhing before a given splitting string.
    /// Or full string if splitter not present
    let inline before (splitter:string) (s:string) = 
        let start = s.IndexOf(splitter) 
        if start = -1 then s
        else s.Substring(0, start )
    
    /// split string, Remove Empty Entries
    /// like: string.Split([| spliter |], StringSplitOptions.RemoveEmptyEntries)
    let inline split (spliter:string) (s:string) = s.Split([|spliter|], StringSplitOptions.RemoveEmptyEntries)
    
    /// split string, Keep Empty Entries
    /// like : string.Split([| spliter |], StringSplitOptions.None)  
    let inline splitKeep (spliter:string) (s:string) = s.Split([|spliter|], StringSplitOptions.None)    
    
    /// split string into two elements, if splitter not found first string is same , second string is empty 
    /// like : string.Split( [| spliter |],2, StringSplitOptions.RemoveEmptyEntries) in if xs.Length > 1 then xs.[0],xs.[1] else s,""
    let inline split2 (spliter:string) (s:string) = let xs = s.Split( [|spliter|],2, StringSplitOptions.None) in if xs.Length > 1 then xs.[0],xs.[1] else s,""
    
    /// finds text betwween two strings
    /// between "X" "T" "cXabTk" = "c", "ab", "k"
    /// returns three empty strings if not both found 
    let between (startChar:string) (endChar:string) (s:string) =         
        let start = s.IndexOf(startChar) 
        if start = -1 then "","",""
        else 
            let ende = s.IndexOf(endChar, start + startChar.Length)
            if ende = -1 then "","",""
            else 
                s.Substring(0, start ),
                s.Substring(start + startChar.Length, ende - start - startChar.Length),// finds text betwween two chars
                s.Substring(ende + endChar.Length)
    
    /// Finds text betwween two strings, includes delimiters on middle string 
    /// betweenIncl between "X" "T" "cXabTk" = "c", "XabT", "k"
    /// returns three empty strings if not both found 
    let betweenIncl (startChar:string) (endChar:string) (s:string) =         
        let start = s.IndexOf(startChar) 
        if start = -1 then "","","" 
        else 
            let ende = s.IndexOf(endChar, start + startChar.Length)
            if ende = -1 then "","",""
            else 
                s.Substring(0, start),
                s.Substring(start, ende - start + endChar.Length),// finds text betwween two chars
                s.Substring(ende + endChar.Length)
    
  
    /// First letter of string to Uppercase
    let inline up1 (s:String)  = 
        if s="" || Char.IsUpper s.[0] then s 
        elif Char.IsLetter s.[0] then  Char.ToUpper(s.[0]).ToString() + s.Substring(1) 
        else s
    
    /// First letter of string to Lowercase
    let inline low1 (s:String) = 
        if s="" || Char.IsLower s.[0] then s 
        elif Char.IsLetter s.[0] then  Char.ToLower(s.[0]).ToString() + s.Substring(1) 
        else s
    
    /// Allows for negative indices too. -1 is last character
    /// The resulting string includes the end index.
    let slice startIdx endIdx (s:string) =
        let count = s.Length
        let st  = if startIdx<0 then count+startIdx    else startIdx
        let len = if endIdx<0   then count+endIdx-st+1 else endIdx-st+1    
        if st < 0 || st > count-1 then 
            let err = sprintf "String.slice: Start index %d is out of range. Allowed values are -%d upto %d for String '%s' of %d chars" startIdx count (count-1) s count
            raise (IndexOutOfRangeException(err))    
        if st+len > count then 
            let err = sprintf "String.slice: End index %d is out of range. Allowed values are -%d upto %d for String '%s' of %d chars" startIdx count (count-1) s count
            raise (IndexOutOfRangeException(err))         
        if len < 0 then
            let en = if endIdx<0 then count+endIdx else endIdx
            let err = sprintf "String.slice: Start index '%A' (= %d) is bigger than end index '%A'(= %d) for String '%s' of %d items" startIdx st endIdx en s count
            raise (IndexOutOfRangeException(err))         
        s.Substring(st,len) 
    

    //-------------------------------------------------------------------------
    // taken from FSharpx
    // https://raw.githubusercontent.com/fsprojects/FSharpx.Extras/master/src/FSharpx.Extras/String.fs
    //-------------------------------------------------------------------------

    /// Returns a value indicating whether a specified substring occurs within this string.
    let inline contains value (s:string) = s.Contains(value)

    /// Compares two specified String objects and returns an integer that indicates their relative position in the sort order.
    let inline compare (comparisonType:StringComparison) strA strB = String.Compare(strA, strB, comparisonType)

    /// Compares two specified String objects and returns an integer that indicates their relative position in the sort order. Compare strings using ordinal (binary) sort rules and ignoring the case of the strings being compared.
    let inline compareIgnoreCase strA strB = compare StringComparison.OrdinalIgnoreCase strA strB

    /// Determines whether the end of this string instance matches the specified string.
    let inline endsWith (value : string) (s:string) = s.EndsWith(value)

    /// Determines whether the end of this string instance matches the specified string when compared using the specified culture.
    let inline endsWith' value ignoreCase culture (s:string) = s.EndsWith(value, ignoreCase, culture)

    /// Determines whether the end of this string instance matches the specified string when compared using the specified comparison option.
    let inline endsWith'' value comparisonType (s:string) = s.EndsWith(value, comparisonType)

    /// Determines whether two specified String objects have the same value.
    let inline equals (comparisonType:StringComparison) a b = String.Equals(a, b, comparisonType)

    /// Determines whether two specified String objects have the same value. Compare strings using ordinal (binary) sort rules and ignoring the case of the strings being compared.
    let inline equalsIgnoreCase a b = equals StringComparison.OrdinalIgnoreCase a b

    /// Reports the zero-based index of the first occurrence of the specified Unicode character in this string.
    let inline indexOfChar (value:char) (s:string) = s.IndexOf(value)

    /// Reports the zero-based index of the first occurrence of the specified Unicode character in this string. The search starts at a specified character position.
    let inline indexOfChar' (value:char) startIndex (s:string) = s.IndexOf(value, startIndex)

    /// Reports the zero-based index of the first occurrence of the specified character in this instance. The search starts at a specified character position and examines a specified number of character positions.
    let inline indexOfChar'' (value:char) startIndex count (s:string) = s.IndexOf(value, startIndex, count)

    /// Reports the zero-based index of the first occurrence of the specified string in this instance.
    let inline indexOfString (value:string) (s:string) = s.IndexOf(value)

    /// Reports the zero-based index of the first occurrence of the specified string in this instance. The search starts at a specified character position.
    let inline indexOfString' (value:string) (startIndex:int) (s:string) = s.IndexOf(value, startIndex)

    /// Reports the zero-based index of the first occurrence of the specified string in this instance. The search starts at a specified character position and examines a specified number of character positions.
    let inline indexOfString'' (value:string) (startIndex:int) (count:int) (s:string) = s.IndexOf(value, startIndex, count)

    /// Reports the zero-based index of the first occurrence of the specified string in the current String object. A parameter specifies the type of search to use for the specified string.
    let inline indexOfStringWithComparison value (comparisonType:StringComparison) (s:string) = s.IndexOf(value, comparisonType)

    /// Reports the zero-based index of the first occurrence of the specified string in the current String object. Parameters specify the starting search position in the current string and the type of search to use for the specified string.
    let inline indexOfStringWithComparison' value startIndex (comparisonType:StringComparison) (s:string) = s.IndexOf(value, startIndex, comparisonType)

    /// Reports the zero-based index of the first occurrence of the specified string in the current String object. Parameters specify the starting search position in the current string, the number of characters in the current string to search, and the type of search to use for the specified string.
    let inline indexOfStringWithComparison'' value startIndex count comparisonType (s:string) = s.IndexOf(value, startIndex, count, comparisonType)

    /// Reports the zero-based index of the first occurrence in this instance of any character in a specified array of Unicode characters.
    let inline indexOfAny anyOf (s:string) = s.IndexOfAny(anyOf)

    /// Reports the zero-based index of the first occurrence in this instance of any character in a specified array of Unicode characters. The search starts at a specified character position.
    let inline indexOfAny' anyOf startIndex (s:string) = s.IndexOfAny(anyOf, startIndex)

    /// Reports the zero-based index of the first occurrence in this instance of any character in a specified array of Unicode characters. The search starts at a specified character position and examines a specified number of character positions.
    let inline indexOfAny'' anyOf startIndex count (s:string) = s.IndexOfAny(anyOf, startIndex, count)

    /// Returns a new string in which a specified string is inserted at a specified index position in this instance.
    let inline insert startIndex value (s:string) = s.Insert(startIndex, value)

    /// Indicates whether this string is in Unicode normalization form C.
    let inline isNormalized (s:string) = s.IsNormalized()

    /// Indicates whether this string is in the specified Unicode normalization form.
    let inline isNormalized' normalizationForm (s:string) = s.IsNormalized(normalizationForm)

    /// Reports the zero-based index position of the last occurrence of a specified Unicode character within this instance.
    let inline lastIndexOfChar (value:char) (s:string) = s.LastIndexOf(value)

    /// Reports the zero-based index position of the last occurrence of a specified Unicode character within this instance. The search starts at a specified character position and proceeds backward toward the beginning of the string.
    let inline lastIndexOfChar' (value:char) startIndex (s:string) = s.LastIndexOf(value, startIndex)

    /// Reports the zero-based index position of the last occurrence of the specified Unicode character in a substring within this instance. The search starts at a specified character position and proceeds backward toward the beginning of the string for a specified number of character positions.
    let inline lastIndexOfChar'' (value:char) startIndex count (s:string) = s.LastIndexOf(value, startIndex, count)

    /// Reports the zero-based index position of the last occurrence of a specified string within this instance.
    let inline lastIndexOfString (value:string) (s:string) = s.LastIndexOf(value)

    /// Reports the zero-based index position of the last occurrence of a specified string within this instance. The search starts at a specified character position and proceeds backward toward the beginning of the string.
    let inline lastIndexOfString' (value:string) (startIndex:int) (s:string) = s.LastIndexOf(value, startIndex)

    /// Reports the zero-based index position of the last occurrence of a specified string within this instance. The search starts at a specified character position and proceeds backward toward the beginning of the string for a specified number of character positions.
    let inline lastIndexOfString'' (value:string) (startIndex:int) (count:int) (s:string) = s.LastIndexOf(value, startIndex, count)

    /// Reports the zero-based index of the last occurrence of a specified string within the current String object. A parameter specifies the type of search to use for the specified string.
    let inline lastIndexOfStringWithComparison value (comparisonType:StringComparison) (s:string) = s.LastIndexOf(value, comparisonType)

    /// Reports the zero-based index of the last occurrence of a specified string within the current String object. The search starts at a specified character position and proceeds backward toward the beginning of the string. A parameter specifies the type of comparison to perform when searching for the specified string.
    let inline lastIndexOfStringWithComparison' value startIndex (comparisonType:StringComparison) (s:string) = s.LastIndexOf(value, startIndex, comparisonType)

    /// Reports the zero-based index position of the last occurrence of a specified string within this instance. The search starts at a specified character position and proceeds backward toward the beginning of the string for the specified number of character positions. A parameter specifies the type of comparison to perform when searching for the specified string.
    let inline lastIndexOfStringWithComparison'' value startIndex count (comparisonType:StringComparison) (s:string) = s.LastIndexOf(value, startIndex, count, comparisonType)

    /// Returns a new string whose textual value is the same as this string, but whose binary representation is in Unicode normalization form C.
    let inline normalize (s:string) = s.Normalize()

    /// Returns a new string whose textual value is the same as this string, but whose binary representation is in the specified Unicode normalization form.
    let inline normalize' normalizationForm (s:string) = s.Normalize(normalizationForm)

    /// Returns a new string that right-aligns the characters in this instance by padding them with spaces on the left, for a specified total length.
    let inline padLeft totalWidth (s:string) = s.PadLeft(totalWidth)

    /// Returns a new string that right-aligns the characters in this instance by padding them on the left with a specified Unicode character, for a specified total length.
    let inline padLeft' totalWidth paddingChar (s:string) = s.PadLeft(totalWidth, paddingChar)

    /// Returns a new string that left-aligns the characters in this string by padding them with spaces on the right, for a specified total length.
    let inline padRight totalWidth (s:string) = s.PadRight(totalWidth)

    /// Returns a new string that left-aligns the characters in this string by padding them on the right with a specified Unicode character, for a specified total length.
    let inline padRight' totalWidth paddingChar (s:string) = s.PadRight(totalWidth, paddingChar)

    /// Returns a new string in which all the characters in the current instance, beginning at a specified position and continuing through the last position, have been deleted.
    let inline remove startIndex (s:string) = s.Remove(startIndex)

    /// Returns a new string in which a specified number of characters in the current instance beginning at a specified position have been deleted.
    let inline remove' startIndex count (s:string) = s.Remove(startIndex, count)

    /// Returns a new string in which all occurrences of a specified Unicode character in this instance are replaced with another specified Unicode character.
    let inline replace' (oldChar:char) (newChar:char) (s:string) = s.Replace(oldChar, newChar) //modified for FsEx: use apostrophe here

    /// Returns a new string in which all occurrences of a specified string in the current instance are replaced with another specified string.
    let inline replace (oldValue:string) (newValue:string) (s:string) = s.Replace(oldValue, newValue) //modified for FsEx: dont use apostrophe here

    /// Splits a string into substrings that are based on the characters in an array.
    let inline splitChar separator (s:string) = s.Split(separator)

    /// Splits a string into a maximum number of substrings based on the characters in an array. You also specify the maximum number of substrings to return.
    let inline splitChar' separator (count:int) (s:string) = s.Split(separator, count)

    /// Splits a string into substrings based on the characters in an array. You can specify whether the substrings include empty array elements.
    let inline splitCharWithOptions (separator:char[]) (options:StringSplitOptions) (s:string) = s.Split(separator, options)

    /// Splits a string into a maximum number of substrings based on the characters in an array.
    let inline splitCharWithOptions' (separator:char[]) count (options:StringSplitOptions) (s:string) = s.Split(separator, count, options)

    /// Splits a string into substrings based on the strings in an array. You can specify whether the substrings include empty array elements.
    let inline splitString (separator:string[]) (options:StringSplitOptions) (s:string) = s.Split(separator, options)

    /// Splits a string into a maximum number of substrings based on the strings in an array. You can specify whether the substrings include empty array elements.
    let inline splitString' (separator:string[]) count (options:StringSplitOptions) (s:string) = s.Split(separator, count, options)

    /// Determines whether the beginning of this string instance matches the specified string.
    let inline startsWith (value : string) (s:string) = s.StartsWith(value)

    /// Determines whether the beginning of this string instance matches the specified string when compared using the specified comparison option.
    let inline startsWith' value comparisonType (s:string) = s.StartsWith(value, comparisonType)

    /// Determines whether the beginning of this string instance matches the specified string when compared using the specified culture.
    let inline startsWith'' value ignoreCase culture (s:string) = s.StartsWith(value, ignoreCase, culture)

    /// Retrieves a substring from this instance. The substring starts at a specified character position and continues to the end of the string.
    let inline substring startIndex (s:string) = s.Substring(startIndex)

    /// Retrieves a substring from this instance. The substring starts at a specified character position and has a specified length.
    let inline substring' startIndex length (s:string) = s.Substring(startIndex, length)

    /// Copies the characters in this instance to a Unicode character array.
    let inline toCharArray (s:string) = s.ToCharArray()

    /// Copies the characters in a specified substring in this instance to a Unicode character array.
    let inline toCharArray' startIndex length (s:string) = s.ToCharArray(startIndex, length)

    /// Returns a copy of this string converted to lowercase.
    let inline toLower (s:string) = s.ToLower()

    /// Returns a copy of this string converted to lowercase, using the casing rules of the specified culture.
    let inline toLower' culture (s:string) = s.ToLower(culture)

    /// Returns a copy of this String object converted to lowercase using the casing rules of the invariant culture.
    let inline toLowerInvariant (s:string) = s.ToLowerInvariant()

    /// Returns a copy of this string converted to uppercase.
    let inline toUpper (s:string) = s.ToUpper()

    /// Returns a copy of this string converted to uppercase, using the casing rules of the specified culture.
    let inline toUpper' culture (s:string) = s.ToUpper(culture)

    /// Returns a copy of this String object converted to uppercase using the casing rules of the invariant culture.
    let inline toUpperInvariant (s:string) = s.ToUpperInvariant()

    /// Removes all leading and trailing white-space characters from the current String object.
    let inline trim (s:string) = s.Trim()

    /// Removes all leading and trailing occurrences of a set of characters specified in an array from the current String object.
    let inline trim' (trimChars : char array) (s:string) = s.Trim(trimChars)

    /// Removes all trailing occurrences of a set of characters specified in an array from the current String object.
    let inline trimEnd (trimChars : char array) (s:string) = s.TrimEnd(trimChars)

    /// Removes all leading occurrences of a set of characters specified in an array from the current String object.
    let inline trimStart (trimChars : char array) (s:string) = s.TrimStart(trimChars)
