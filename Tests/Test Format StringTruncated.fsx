#r @"D:\Git\FsEx\bin\Release\netstandard2.0\FsEx.dll"

open System
open FsEx

    
do  
    /// Reduces a string length for diplay to a maxiumum Length.
    /// Shows (..) as placeholder for skiped characters if string is longer than maxCharCount.
    /// If maxChars is bigger than 35 the placeholder will include the count of skipped characters: e.g. [< ..99 more chars.. >].
    /// maxCharCount will be set to be minimum 6. 
    /// Returned strings are enclosed in quotaion marks.
    /// If input is null it returns <*null string*>
    let stringTruncated maxCharCount (s:string):string = 
        let maxChar = max 6 maxCharCount
        if isNull s then 
            if maxChar >= 15 then 
                "<*null string*>" 
            elif maxChar >= 8 then  
                "<*null*>" 
            else
                "*null*" 
        elif s.Length <= maxChar  then 
            str{ "\"" ; s ; "\"" }
        else 
            let len = s.Length
            if   maxChar <= 9 then str{ "\"" ;  s.Substring(0, maxChar-2-2)  ; "(..)"                           ; "\"" }
            elif maxChar <= 20 then str{ "\"" ;  s.Substring(0, maxChar-3-2) ; "(..)"  ; s.Substring(len-1, 1)  ; "\"" }
            elif maxChar <= 35 then str{ "\"" ;  s.Substring(0, maxChar-5-2) ; "(...)" ; s.Substring(len-2, 2)  ; "\"" }
            else     
                let suffixLen = 1 + maxChar / 20 // using 5% for end of string
                let counterLen = "[< ..99 more chars.. >]".Length 
                str{   
                    "\""
                    s.Substring(0, maxChar-counterLen-suffixLen) 
                    "[< .."; len - maxChar+counterLen  ; " more chars.. >]" 
                    s.Substring(len-suffixLen, suffixLen) 
                    "\""
                    } 
       
    
    clearSeffLog() 
    for t = 4 to 50 do   
        let mi = t-3
        let ma = t+20
        Printfn.red $"truncating to length {t} for strings from {mi} to {ma}"
        for i=mi to ma do  
            str{ for k=0 to i do k%10 }
            |> stringTruncated t 
            |> printfn "%s"
    