namespace FsEx

open System


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module String =

    /// Returns everytrhing before a given splitting string
    /// Full strring if splitter not present
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
    
    // string.Trim()
    let trim (s:string) = s.Trim()
    // 
    let trimEnd (s:string) = s.TrimEnd()
    let trimStart (s:string) = s.TrimStart()   

    ///inText.Replace(toReplace, replaceWith)
    let replace (toReplace:string) (replaceWith:string) (inText:string) = inText.Replace(toReplace, replaceWith)

    ///finds text betwween two strings
    ///between "((" ")" "c((ab)c" = ("c", "ab", "c")
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
    
    /// finds text betwween two strings including delimiters on middle string 
    ///betweenIncl "((" "))" "c((ab))d" = "c", "((ab))", "d"
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
    let up1 (s:String)  = if s="" then s else Char.ToUpper(s.[0]).ToString() + s.Substring(1)
    
    ///First letter of string to Lowercase
    let low1 (s:String) = if s="" then s else Char.ToLower(s.[0]).ToString() + s.Substring(1)
