namespace FsEx

open System


[<AutoOpen>]
module TypeExtensionsString =   
    
    [<EXT>]
    type System.String with
        [<EXT>]
        member inline s.Last = s.[s.Length - 1] 
        
        [<EXT>]
        member s.LastX i = s.Substring(s.Length-i,i) 
        
        ///Allows for negtive index too (like Python)
        [<EXT>]         
        member this.GetItem index = if index<0 then this.[this.Length+index]   else this.[index]          
        
        //member this.GetSlice(startIdx, endIdx) = // overides of existing methods are unfurtrunatly silently ignored and not possible. see https://github.com/dotnet/fsharp/issues/3692#issuecomment-334297164

        ///Allows for negative indices too.
        [<EXT>]
        member s.Slice(startIdx,endIdx) =
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
                let err = sprintf "GetSlice: Start index '%A' (= %d) is bigger than end index '%A'(= %d) for String '%s' of %d items" startIdx st endIdx en s count
                raise (IndexOutOfRangeException(err)) 
            
            s.Substring(st,len) 


module String =

    ///Returns everytrhing before a given splitting string.
    ///Or full string if splitter not present
    let before (splitter:string) (s:string) = 
        let start = s.IndexOf(splitter) 
        if start = -1 then s
        else s.Substring(0, start )

    
    ///split string, Remove Empty Entries
    ///like: string.Split([| spliter |], StringSplitOptions.RemoveEmptyEntries)
    let split (spliter:string) (s:string) = s.Split([|spliter|], StringSplitOptions.RemoveEmptyEntries)
    
    ///split string, Keep Empty Entries
    ///like : string.Split([| spliter |], StringSplitOptions.None)  
    let splitKeep (spliter:string) (s:string) = s.Split([|spliter|], StringSplitOptions.None)    
    
    ///split string into two elements, if splitter not found first string is same , second string is empty 
    ///like : string.Split( [| spliter |],2, StringSplitOptions.RemoveEmptyEntries) in if xs.Length > 1 then xs.[0],xs.[1] else s,""
    let split2 (spliter:string) (s:string) = let xs = s.Split( [|spliter|],2, StringSplitOptions.None) in if xs.Length > 1 then xs.[0],xs.[1] else s,""
    
    ///Trims whitspace at beginnig and end, like s.Trim()
    let trim (s:string) = s.Trim()
    
    ///Trims whitspace at end, like s.TrimEnd()
    let trimEnd (s:string) = s.TrimEnd()

    ///Trims whitspace at start, like s.TrimStart()
    let trimStart (s:string) = s.TrimStart()   

    ///Replaces parts of the charcters in a string , inText.Replace(toReplace, replaceWith)
    let replace (toReplace:string) (replaceWith:string) (inText:string) = inText.Replace(toReplace, replaceWith)

    ///finds text betwween two strings
    ///between "X" "T" "cXabTk" = "c", "ab", "k"
    ///returns three empty strings if not both found 
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
    
    ///Finds text betwween two strings, includes delimiters on middle string 
    ///betweenIncl between "X" "T" "cXabTk" = "c", "XabT", "k"
    ///returns three empty strings if not both found 
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
    
  
    ///First letter of string to Uppercase
    let up1 (s:String)  = 
        if s="" || Char.IsUpper s.[0] then s 
        elif Char.IsLetter s.[0] then  Char.ToUpper(s.[0]).ToString() + s.Substring(1) 
        else s
    
    ///First letter of string to Lowercase
    let low1 (s:String) = 
        if s="" || Char.IsLower s.[0] then s 
        elif Char.IsLetter s.[0] then  Char.ToLower(s.[0]).ToString() + s.Substring(1) 
        else s
