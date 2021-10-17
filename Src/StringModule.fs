namespace FsEx

open System
open System.Text
open FsEx.SaveIgnore //so that  |> ignore  can only be used on value types
open FsEx.ExtensionsString // for FsExStringException

/// Functions for transforming strings
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] //need this so doesn't hide String class in C# assemblies
module String = 

    /// Trims strings to 80 chars for showing in in one line.
    /// It returns the input string trimmed to 80 chars, a count of skiped characters and the last 5 characters
    /// Replace line breaks with '\r\n' or '\n' literal
    /// Does not include surrounding quotes
    /// If string is null returns "-null string-"
    let truncateFormatedInOneLine (stringToTrim:string) :string = 
        if isNull stringToTrim then FsExStringException.Raise "String.truncateFormatedInOneLine: stringToTrim is null" 
        else
            let s = 
                let maxChars = 80
                if stringToTrim.Length <= maxChars + 20 then  stringToTrim
                else
                    let len   = stringToTrim.Length
                    let st    = stringToTrim.Substring(0,maxChars)
                    let last5 = stringToTrim.Substring(len-6)
                    sprintf "%s[..%d more Chars..]%s" st (len - maxChars - 5) last5
            s.Replace("\r","\\r").Replace("\n","\\n")
    

    /// If the input string is longer than maxChars variable plus twenty tolerance then
    /// it returns the input string trimmed to maxChars, a count of skiped characters and the last 5 characters
    /// e.g. "abcde[..20 more Chars..]vwxyz"
    /// Else, if the input string is less than maxChars + 20, it is still returned in full.
    /// Not enclosed inquotes.
    /// Fails on null.
    /// Alternativly use String.formatInOneShortLine thaty will not fail on null.
    let (*inline*) truncateFormated (maxChars:int) (stringToTrim:string) = 
        if isNull stringToTrim then FsExStringException.Raise "String.truncateFormated: stringToTrim is null, maxChar: %d" maxChars
        if stringToTrim.Length <= maxChars + 20 then sprintf "%s"stringToTrim
        else
            let len   = stringToTrim.Length
            let st    = stringToTrim.Substring(0,maxChars)
            let last6 = stringToTrim.Substring(len-6)
            sprintf "%s[..%d more Chars..]%s" st (len - maxChars - 5) last6

    /// Trims strings ot maximum line count.
    /// Adds note about trimmed line count if there are more [ ... and %d more lines.]
    /// Does not include surrounding quotes
    /// If string is null returns "-null string-"
    let truncateToMaxLines (maxLineCount:int) (stringToTrim:string) :string = 
        if isNull stringToTrim then FsExStringException.Raise "String.truncateToMaxLines: stringToTrim is null, maxLineCount: %d" maxLineCount
        else
            let lns = stringToTrim.Split([|'\n'|],StringSplitOptions.None)
            let t = 
                lns
                |> Seq.truncate maxLineCount
                |> Seq.map ( fun l -> l.TrimEnd() )
                |> String.concat Environment.NewLine

            if lns.Length > maxLineCount then 
                sprintf "%s\%s[ ... and %d more lines.]" t Environment.NewLine (lns.Length - maxLineCount)
            else
                t

    /// Takes at most a given amount of chars from string.
    /// If input is shorter than truncateLength retuns input string unchanged.
    /// Code: if truncateLength >= fromString.Length then fromString else fromString.Substring(0,truncateLength)
    let (*inline*) truncate (truncateLength:int) (fromString:string) :string = 
        if isNull fromString   then FsExStringException.Raise "String.take: fromString is null (truncateLength:%d)" truncateLength
        if truncateLength < 0 then FsExStringException.Raise "String.take: truncateLength:%d cant be negative(for  '%s')" truncateLength fromString
        if truncateLength >= fromString.Length then fromString
        else fromString.Substring(0,truncateLength)

    /// For string formating in exceptions below.
    /// Including surounding quotes
    let private exnf s :string = 
        if isNull s then "-null string-"  else "\"" + truncateFormatedInOneLine s + "\"" //separate null check so null value is not in quotes

    // TODO removed inline to get compile times down in FSI ?

    /// Remove characters from the starts.
    /// fromString.Substring(skipLength)    
    let (*inline*) skip (skipLength:int) (fromString:string) :string = 
        if isNull fromString   then FsExStringException.Raise "String.skip: fromString is null (skipLength:%d)" skipLength
        if skipLength > fromString.Length then FsExStringException.Raise "String.skip: skipLength:%d is longer than string '%s'" skipLength fromString
        if skipLength < 0 then FsExStringException.Raise "String.skip: skipLength:%d cant be negative(for  '%s')" skipLength fromString
        fromString.Substring(skipLength)        

    /// Takes a given amount of chars from string.
    /// Fails if input is shorter than takeLength. Use String.truncate instead if you want to avoid failing in that case.
    /// Code: fromString.Substring(0,takeLength)
    let (*inline*) take (takeLength:int) (fromString:string) :string = 
        if isNull fromString   then FsExStringException.Raise "String.take: fromString is null (takeLength:%d)" takeLength
        if takeLength > fromString.Length then FsExStringException.Raise "String.take: takeLength:%d is longer than string '%s'. Use String.truncate instead!" takeLength fromString
        if takeLength < 0 then FsExStringException.Raise "String.take: takeLength:%d cant be negative(for  '%s')" takeLength fromString
        fromString.Substring(0,takeLength)


    /// Removes substring from a string if it exists. same as:
    /// (Will return the same string instance, if text to remove is not found)
    /// Code:fromString.Replace(textToRemove, "")
    let (*inline*) delete (textToRemove:string) (fromString:string) :string = 
        if isNull fromString   then FsExStringException.Raise "String.delete: fromString is null (textToRemove:%s)" (exnf textToRemove)
        if isNull textToRemove then FsExStringException.Raise "String.delete: textToRemove is null (fromString:%s)" (exnf fromString)
        fromString.Replace(textToRemove, "") // will return the same instance if text to remove is not found
    
    /// Removes character from a string if it exists. same as:
    /// (Will return the same string instance, if char to remove is not found)
    /// Code: fromString.Replace(charToRemove.ToString(), "")
    let (*inline*) deleteChar (charToRemove:char) (fromString:string) :string = 
        if isNull fromString   then FsExStringException.Raise "String.delete: fromString is null (charToRemove:%c)" charToRemove        
        fromString.Replace(charToRemove.ToString(), "") // will return the same instance if text to remove is not found


    /// Ensures all lines end on System.Environment.NewLine
    /// Code: StringBuilder(s).Replace("\r\n", "\n").Replace("\r", "\n").Replace("\n", Environment.NewLine).ToString()
    let (*inline*) unifyLineEndings (txt:string) = 
        if isNull txt then FsExStringException.Raise "String.unifyLineEndings: input is null"
        StringBuilder(txt).Replace("\r\n", "\n").Replace("\r", "\n").Replace("\n", Environment.NewLine).ToString() // TODO correct but not performance optimised

    /// Returns everytrhing before first occurence of a given splitting string.
    /// Or fails if splitter is not found.
    /// Uses StringComparison.Ordinal
    let (*inline*) before (splitter:string) (stringToSearchIn:string) :string = 
        if isNull stringToSearchIn then FsExStringException.Raise "String.before: stringToSearchIn is null (splitter:%s)" (exnf splitter)
        if isNull splitter         then FsExStringException.Raise "String.before: splitter is null (stringToSearchIn:%s)" (exnf stringToSearchIn)
        let start = stringToSearchIn.IndexOf(splitter, StringComparison.Ordinal)
        if start = -1 then FsExStringException.Raise "String.before: splitter %s not found in stringToSearchIn:%s" (exnf splitter) (exnf stringToSearchIn)
        else stringToSearchIn.Substring(0, start)

    /// Returns everytrhing before first occurence of a given splitting character.
    /// Or fails if splitter is not found.
    let (*inline*) beforeChar (splitter:char) (stringToSearchIn:string) :string = 
        if isNull stringToSearchIn then FsExStringException.Raise "String.beforeChar: stringToSearchIn is null (splitter:%c)" (splitter)
        let start = stringToSearchIn.IndexOf(splitter)
        if start = -1 then FsExStringException.Raise "String.before: splitter '%c' not found in stringToSearchIn:%s" splitter (exnf stringToSearchIn)
        else stringToSearchIn.Substring(0, start)

    /// Returns everytrhing before first occurence of a given splitting string.
    /// Or None if splitter is not found.
    /// Uses StringComparison.Ordinal
    let (*inline*) tryBefore (splitter:string) (stringToSearchIn:string): option<string> = 
        if isNull stringToSearchIn then FsExStringException.Raise "String.tryBefore: stringToSearchIn is null (splitter:%s)" (exnf splitter)
        if isNull splitter         then FsExStringException.Raise "String.tryBefore: splitter is null (stringToSearchIn:%s)" (exnf stringToSearchIn)
        let start = stringToSearchIn.IndexOf(splitter, StringComparison.Ordinal)
        if start = -1 then None
        else Some <| stringToSearchIn.Substring(0, start)

    /// Returns everytrhing before first occurence of a given splitting character.
    /// Or None if splitter is not found.
    let (*inline*) tryBeforeChar (splitter:char) (stringToSearchIn:string): option<string>  = 
        if isNull stringToSearchIn then FsExStringException.Raise "String.tryBeforeChar: stringToSearchIn is null (splitter:%c)" (splitter)
        let start = stringToSearchIn.IndexOf(splitter)
        if start = -1 then None
        else Some <| stringToSearchIn.Substring(0, start)

    /// Returns everytrhing before first occurence of a given splitting string.
    /// Or fails if splitter is not found.
    /// Uses StringComparison.Ordinal
    let (*inline*) after (splitter:string) (stringToSearchIn:string) :string = 
        if isNull stringToSearchIn then FsExStringException.Raise "String.after: stringToSearchIn is null (splitter:%s)" (exnf splitter)
        if isNull splitter         then FsExStringException.Raise "String.after: splitter is null (stringToSearchIn:%s)" (exnf stringToSearchIn)
        let start = stringToSearchIn.IndexOf(splitter, StringComparison.Ordinal)
        if start = -1 then FsExStringException.Raise "String.after: splitter %s not found in stringToSearchIn:%s" (exnf splitter) (exnf stringToSearchIn)
        else stringToSearchIn.Substring(start+1)//cant be out of bounds!

    /// Returns everytrhing after first occurence of a given splitting character.
    /// Or fails if splitter is not found
    let (*inline*) afterChar (splitter:char) (stringToSearchIn:string) :string  = 
        if isNull stringToSearchIn then FsExStringException.Raise "String.afterChar: stringToSearchIn is null (splitter:%c)" (splitter)
        let start = stringToSearchIn.IndexOf(splitter)
        if start = -1 then FsExStringException.Raise "String.after: splitter '%c' not found in stringToSearchIn:%s" splitter (exnf stringToSearchIn)
        else stringToSearchIn.Substring(start+1)//cant be out of bounds!

    /// Returns everytrhing after first occurence of a given splitting string.
    /// Or None if splitter is not found
    /// Uses StringComparison.Ordinal
    let (*inline*) tryAfter (splitter:string) (stringToSearchIn:string): option<string>  = 
        if isNull stringToSearchIn then FsExStringException.Raise "String.tryAfter: stringToSearchIn is null (splitter:%s)" (exnf splitter)
        if isNull splitter         then FsExStringException.Raise "String.tryAfter: splitter is null (stringToSearchIn:%s)" (exnf stringToSearchIn)
        let start = stringToSearchIn.IndexOf(splitter, StringComparison.Ordinal)
        if start = -1 then None
        else Some <|stringToSearchIn.Substring(start+1)//cant be out of bounds!

    /// Returns everytrhing after first occurence of a given splitting character.
    /// Or None if splitter is not found
    let (*inline*) tryAfterChar (splitter:char) (stringToSearchIn:string) : option<string> = 
        if isNull stringToSearchIn then FsExStringException.Raise "String.tryAfterChar: stringToSearchIn is null (splitter:%c)" (splitter)
        let start = stringToSearchIn.IndexOf(splitter)
        if start = -1 then None
        else Some <|stringToSearchIn.Substring(start+1)//cant be out of bounds!

    /// Finds text betwween two strings
    /// e.g.: between "X" "T" "cXabTk" =  "ab"
    /// Fails if not both splitters are found.
    /// Delimiters are excluded in the returned strings
    let (*inline*) between (firstSplitter:string) (secondSplitter:string) (stringToSplit:string) :string = 
        if isNull stringToSplit  then FsExStringException.Raise "String.between: stringToSplit is null (firstSplitter:%s, secondSplitter:%s) " (exnf firstSplitter) (exnf secondSplitter)
        if isNull firstSplitter  then FsExStringException.Raise "String.between: firstSplitter is null (stringToSplit:%s, secondSplitter:%s)" (exnf stringToSplit) (exnf secondSplitter)
        if isNull secondSplitter then FsExStringException.Raise "String.between: secondSplitter is null (stringToSplit:%s, firstSplitter:%s)" (exnf stringToSplit) (exnf firstSplitter)
        let start = stringToSplit.IndexOf(firstSplitter, StringComparison.Ordinal)
        if start = -1 then FsExStringException.Raise "String.between: firstSplitter: %s not found in stringToSplit: %s  (secondSplitter: %s)" (exnf firstSplitter) (exnf stringToSplit) (exnf secondSplitter)
        else
            let ende = stringToSplit.IndexOf(secondSplitter, start + firstSplitter.Length, StringComparison.Ordinal)
            if ende = -1 then FsExStringException.Raise "String.between: secondSplitter: %s not found in stringToSplit: %s  (firstSplitter: %s)" (exnf secondSplitter) (exnf stringToSplit) (exnf firstSplitter)
            else
                stringToSplit.Substring(start + firstSplitter.Length, ende - start - firstSplitter.Length)// finds text betwween two chars

    /// Finds text betwween two strings
    /// e.g.: between "X" "T" "cXabTk" =  "ab"
    /// Returns None if not both splitters are found.
    /// Delimiters are excluded in the returned strings
    let (*inline*) tryBetween (firstSplitter:string) (secondSplitter:string) (stringToSplit:string): option<string>  = 
        if isNull stringToSplit  then FsExStringException.Raise "String.between: stringToSplit is null (firstSplitter:%s, secondSplitter:%s) " (exnf firstSplitter) (exnf secondSplitter)
        if isNull firstSplitter  then FsExStringException.Raise "String.between: firstSplitter is null (stringToSplit:%s, secondSplitter:%s)" (exnf stringToSplit) (exnf secondSplitter)
        if isNull secondSplitter then FsExStringException.Raise "String.between: secondSplitter is null (stringToSplit:%s, firstSplitter:%s)" (exnf stringToSplit) (exnf firstSplitter)
        let start = stringToSplit.IndexOf(firstSplitter, StringComparison.Ordinal)
        if start = -1 then None
        else
            let ende = stringToSplit.IndexOf(secondSplitter, start + firstSplitter.Length, StringComparison.Ordinal)
            if ende = -1 then None
            else
                Some <|stringToSplit.Substring(start + firstSplitter.Length, ende - start - firstSplitter.Length)// finds text betwween two chars


    /// Split string into two elements,
    /// Fails if  splitter is not found.
    /// The splitter is not included in the two return strings.
    let (*inline*) splitOnce (splitter:string) (stringToSplit:string) : string*string = 
        if isNull stringToSplit then FsExStringException.Raise "String.splitOnce: stringToSplit is null (splitter:%s)" (exnf splitter)
        if isNull splitter         then FsExStringException.Raise "String.splitOnce: splitter is null (stringToSplit:%s)" (exnf stringToSplit)
        let start = stringToSplit.IndexOf(splitter, StringComparison.Ordinal)
        if start = -1 then FsExStringException.Raise "String.splitOnce: splitter %s not found in stringToSplit: %s" (exnf splitter) (exnf stringToSplit)
        else               stringToSplit.Substring(0, start), stringToSplit.Substring(start + splitter.Length)

    /// Finds text before, between and after  two strings.
    /// e.g.: between "X" "T" "cXabTk" = "c", "ab", "k"
    /// Fails if not both splitters are found.
    /// Delimiters are excluded in the three returned strings
    let (*inline*) splitTwice (firstSplitter:string) (secondSplitter:string) (stringToSplit:string) : string*string*string = 
        if isNull stringToSplit  then FsExStringException.Raise "String.splitTwice: stringToSplit is null (firstSplitter:%s, secondSplitter:%s) " (exnf firstSplitter) (exnf secondSplitter)
        if isNull firstSplitter  then FsExStringException.Raise "String.splitTwice: firstSplitter is null (stringToSplit:%s, secondSplitter:%s)" (exnf stringToSplit) (exnf secondSplitter)
        if isNull secondSplitter then FsExStringException.Raise "String.splitTwice: secondSplitter is null (stringToSplit:%s, firstSplitter:%s)" (exnf stringToSplit) (exnf firstSplitter)
        let start = stringToSplit.IndexOf(firstSplitter, StringComparison.Ordinal)
        if start = -1 then FsExStringException.Raise "String.splitTwice: firstSplitter: %s not found in stringToSplit: %s  (secondSplitter: %s)" (exnf firstSplitter) (exnf stringToSplit) (exnf secondSplitter)
        else
            let ende = stringToSplit.IndexOf(secondSplitter, start + firstSplitter.Length, StringComparison.Ordinal)
            if ende = -1 then FsExStringException.Raise "String.splitTwice: secondSplitter: %s not found in stringToSplit: %s  (firstSplitter: %s)" (exnf secondSplitter) (exnf stringToSplit) (exnf firstSplitter)
            else
                stringToSplit.Substring(0, start ),
                stringToSplit.Substring(start + firstSplitter.Length, ende - start - firstSplitter.Length),// finds text betwween two chars
                stringToSplit.Substring(ende + secondSplitter.Length)

    /// Split string into two elements,
    /// If splitter not found None is returned
    /// Splitter is not included in the two return strings.
    let (*inline*) trySplitOnce (splitter:string) (stringToSplit:string) : option<string*string> = 
        if isNull stringToSplit then FsExStringException.Raise "String.splitOnce: stringToSplit is null (splitter:%s)" (exnf splitter)
        if isNull splitter         then FsExStringException.Raise "String.splitOnce: splitter is null (stringToSplit:%s)" (exnf stringToSplit)
        let start = stringToSplit.IndexOf(splitter, StringComparison.Ordinal)
        if start = -1 then None
        else               Some (stringToSplit.Substring(0, start), stringToSplit.Substring(start + splitter.Length))

    /// Finds text before, between and after two strings.
    /// e.g.: between "X" "T" "cXabTk" = "c", "ab", "k"
    /// If not both splitters are found returns None
    /// Delimiters are excluded in the three returned strings
    let (*inline*) trySplitTwice (firstSplitter:string) (secondSplitter:string) (stringToSplit:string) : option<string*string*string>= 
        if isNull stringToSplit  then FsExStringException.Raise "String.splitTwice: stringToSplit is null (firstSplitter:%s, secondSplitter:%s)" (exnf firstSplitter) (exnf secondSplitter)
        if isNull firstSplitter  then FsExStringException.Raise "String.splitTwice: firstSplitter is null (stringToSplit:%s, secondSplitter:%s)" (exnf stringToSplit)(exnf secondSplitter)
        if isNull secondSplitter then FsExStringException.Raise "String.splitTwice: secondSplitter is null (stringToSplit:%s, firstSplitter:%s)" (exnf stringToSplit)(exnf firstSplitter)
        let start = stringToSplit.IndexOf(firstSplitter, StringComparison.Ordinal)
        if start = -1 then None
        else
            let ende = stringToSplit.IndexOf(secondSplitter, start + firstSplitter.Length, StringComparison.Ordinal)
            if ende = -1 then None
            else Some(
                    stringToSplit.Substring(0, start ),
                    stringToSplit.Substring(start + firstSplitter.Length, ende - start - firstSplitter.Length),// finds text betwween two chars
                    stringToSplit.Substring(ende + secondSplitter.Length)
                    )


    /// Makes First letter of string to Uppercase if possible.
    let (*inline*) up1 (txt:string)  = 
        if isNull txt then FsExStringException.Raise "String.up1: string is null"
        if txt="" || Char.IsUpper txt.[0] then txt
        elif Char.IsLetter txt.[0] then  String(Char.ToUpper(txt.[0]),1) + txt.Substring(1)
        else txt

    /// Makes First letter of string to Lowercase if possible.
    let (*inline*) low1 (txt:string) = 
        if isNull txt then FsExStringException.Raise "String.low11: string is null"
        if txt="" || Char.IsLower txt.[0] then txt
        elif Char.IsLetter txt.[0] then  String(Char.ToLower(txt.[0]),1) + txt.Substring(1)
        else txt

    /// Allows for negative indices too. -1 is last character
    /// The resulting string includes the end index.
    let (*inline*) slice startIdx endIdx (txt:string) = 
        if isNull txt then FsExStringException.Raise "String.slice: string is null! startIdx: %d endIdx: %d" startIdx  endIdx
        let count = txt.Length
        let st  = if startIdx<0 then count+startIdx    else startIdx
        let len = if endIdx<0   then count+endIdx-st+1 else endIdx-st+1
        if st < 0 || st > count-1 then
            FsExStringException.Raise "String.slice: Start index %d is out of range. Allowed values are -%d upto %d for String %s of %d chars" startIdx count (count-1) (exnf txt) count
        if st+len > count then
            FsExStringException.Raise "String.slice: End index %d is out of range. Allowed values are -%d upto %d for String %s of %d chars" startIdx count (count-1) (exnf txt) count
        if len < 0 then
            let en = if endIdx<0 then count+endIdx else endIdx
            FsExStringException.Raise "String.slice: Start index '%A' (= %d) is bigger than end index '%A'(= %d) for String %s of %d items" startIdx st endIdx en (exnf txt) count
        txt.Substring(st,len)


    /// Counts how often a substring appears in a string
    /// Uses StringComparison.Ordinal
    let (*inline*) countSubString (subString:string) (textToSerach:string) = 
        if isNull textToSerach then FsExStringException.Raise "String.countSubString: textToSerach is null, subString: %s" (exnf subString)
        if isNull subString    then FsExStringException.Raise "String.countSubString: subString is null, textToSerach: %s" (exnf textToSerach)        
        let rec find fromIdx k = 
            let r = textToSerach.IndexOf(subString, fromIdx, StringComparison.Ordinal)
            if r < 0 then k
            else find (r + subString.Length) (k + 1)
        find 0 0

    /// Counts how often a character appears in a string    
    let (*inline*) countChar (chr:char) (textToSerach:string) = 
        if isNull textToSerach then FsExStringException.Raise "String.countChar: textToSerach is null, chr: %c" chr       
        let rec find fromIdx k = 
            let r = textToSerach.IndexOf(chr, fromIdx)
            if r < 0 then k
            else find (r + 1) (k + 1)
        find 0 0


    /// Removes accents & diacritics from characters
    /// eventually returns string.Normalize(NormalizationForm.FormC)
    let normalize (txt:string ) = 
        if isNull txt then FsExStringException.Raise "String.normalize: txt is null"
        // better: https://github.com/apache/lucenenet/blob/master/src/Lucene.Net.Analysis.Common/Analysis/Miscellaneous/ASCIIFoldingFilter.cs
        // https://stackoverflow.com/questions/249087/how-do-i-remove-diacritics-accents-from-a-string-in-net
        txt.Normalize(System.Text.NormalizationForm.FormD)
        |> Seq.filter ( fun c -> Globalization.CharUnicodeInfo.GetUnicodeCategory(c) <> Globalization.UnicodeCategory.NonSpacingMark  )
        |> String.Concat
        |> fun s -> txt.Normalize(NormalizationForm.FormC)


    /// replaces new lines with custom string
    let (*inline*) inOneLine (newLineReplacment:string) (stringToMakeOneLine:string) = 
        if isNull stringToMakeOneLine then FsExStringException.Raise "String.inOneLine: stringToMakeOneLine is null, newLineReplacment: %s" (exnf newLineReplacment)
        if isNull newLineReplacment   then FsExStringException.Raise "String.inOneLine: newLineReplacment is null, stringToMakeOneLine: %s" (exnf stringToMakeOneLine)
        if stringToMakeOneLine.Contains("\n") || stringToMakeOneLine.Contains("\r") then
            StringBuilder(stringToMakeOneLine).Replace("\r", "").Replace("\n", newLineReplacment).ToString()
        else stringToMakeOneLine

    //-------------------------------------------------------------------------
    //-------------------------------------------------------------------------
    // taken and adapted from from FSharpx
    // https://raw.githubusercontent.com/fsprojects/FSharpx.Extras/master/src/FSharpx.Extras/String.fs
    //-------------------------------------------------------------------------
    //-------------------------------------------------------------------------

    /// Returns true if a specified substring occurs within this string.
    let (*inline*) contains (stringToFind:string) (stringToSearchIn:string) = 
        if isNull stringToSearchIn then FsExStringException.Raise "String.contains: stringToSearchIn is null, stringToFind: %s"     (exnf stringToFind)
        if isNull stringToFind     then FsExStringException.Raise "String.contains: stringToFind     is null, stringToSearchIn: %s" (exnf stringToSearchIn)
        stringToSearchIn.Contains(stringToFind)

    /// Returns true if a specified substring does NOT occurs within this string.
    let (*inline*) notContains (stringToFind:string) (stringToSearchIn:string) = 
        if isNull stringToSearchIn then FsExStringException.Raise "String.notContains: stringToSearchIn is null, stringToFind: %s"     (exnf stringToFind)
        if isNull stringToFind     then FsExStringException.Raise "String.notContains: stringToFind     is null, stringToSearchIn: %s" (exnf stringToSearchIn)
        not (stringToSearchIn.Contains(stringToFind))

    /// Returns true if specified character occurs within this string.
    let (*inline*) containsChar (charToFind:char) (stringToSearchIn:string) = 
        if isNull stringToSearchIn then FsExStringException.Raise "String.containsChar: stringToSearchIn is null, char: '%c'" charToFind
        stringToSearchIn.IndexOf(charToFind) <> -1

    /// Returns true if specified charcter does NOT occurs within this string.
    let (*inline*) notContainsChar (charToFind:char) (stringToSearchIn:string) = 
        if isNull stringToSearchIn then FsExStringException.Raise "String.notContainsChar: stringToSearchIn is null, char: '%c'" charToFind
        stringToSearchIn.IndexOf(charToFind) = -1

    /// Compares two specified String objects and returns an integer that indicates their relative position in the sort order, u
    /// Uses StringComparison.Ordinal.
    let (*inline*) compare strA strB = 
        if isNull strA then FsExStringException.Raise "String.compare: strA is null, strB: %s" (exnf strB)
        if isNull strB then FsExStringException.Raise "String.compare: strB is null, strA: %s" (exnf strA)
        String.Compare(strA, strB, StringComparison.Ordinal)

    /// Compares two specified String objects and returns an integer that indicates their relative position in the sort order,
    /// Uses StringComparison.OrdinalIgnoreCase.
    let (*inline*) compareIgnoreCase strA strB = 
        if isNull strA then FsExStringException.Raise "String.compareIgnoreCase: strA is null, strB: %s" (exnf strB)
        if isNull strB then FsExStringException.Raise "String.compareIgnoreCase: strB is null, strA: %s" (exnf strA)
        String.Compare(strA, strB, StringComparison.OrdinalIgnoreCase )

    /// Determines whether the end of this string instance matches the specified string, using StringComparison.Ordinal.
    let (*inline*) endsWith (stringToFindAtEnd : string) (stringSerachInAtEnd:string)  = 
        if isNull stringToFindAtEnd then FsExStringException.Raise "String.endsWith: stringToFindAtEnd is null. (stringSerachInAtEnd:%s) " (exnf stringSerachInAtEnd)
        if isNull stringSerachInAtEnd then FsExStringException.Raise "String.endsWith: stringSerachInAtEnd is null. (stringToFindAtEnd:%s) " (exnf stringToFindAtEnd)
        stringSerachInAtEnd.EndsWith(stringToFindAtEnd, StringComparison.Ordinal)

    /// Determines whether the end of this string instance matches the specified string, using StringComparison.OrdinalIgnoreCase.
    let (*inline*) endsWithIgnoreCase (stringToFindAtEnd : string) (stringSerachInAtEnd:string)  = 
        if isNull stringToFindAtEnd then FsExStringException.Raise "String.endsWithIgnoreCase: stringToFindAtEnd is null. (stringSerachInAtEnd:%s) " (exnf stringSerachInAtEnd)
        if isNull stringSerachInAtEnd then FsExStringException.Raise "String.endsWithIgnoreCase: stringSerachInAtEnd is null. (stringToFindAtEnd:%s) " (exnf stringToFindAtEnd)
        stringSerachInAtEnd.EndsWith(stringToFindAtEnd, StringComparison.OrdinalIgnoreCase)

    /// Determines whether the beginning of this string instance matches the specified string, using StringComparison.Ordinal..
    let (*inline*) startsWith (stringToFindAtStart:string) (stringToSearchIn:string)  = 
        if isNull stringToFindAtStart then FsExStringException.Raise "String.startsWith: stringToFindAtStart is null. (stringToSearchIn:%s) " (exnf stringToSearchIn)
        if isNull stringToSearchIn then FsExStringException.Raise "String.startsWith: stringToSearchIn is null. (stringToFindAtStart:%s) " (exnf stringToFindAtStart)
        stringToSearchIn.StartsWith(stringToFindAtStart, StringComparison.Ordinal)

    /// Determines whether the beginning of this string instance matches the specified string when compared using StringComparison.OrdinalIgnoreCase.
    let (*inline*) startsWithIgnoreCase (stringToFindAtStart:string) (stringToSearchIn:string)  = 
        if isNull stringToFindAtStart then FsExStringException.Raise "String.startsWithIgnoreCase: stringToFindAtStart is null.  (stringToSearchIn:%s) "  (exnf stringToSearchIn)
        if isNull stringToSearchIn then FsExStringException.Raise "String.startsWithIgnoreCase: stringToSearchIn is null. (stringToFindAtStart:%s)  " (exnf stringToFindAtStart)
        stringToSearchIn.StartsWith(stringToFindAtStart, StringComparison.OrdinalIgnoreCase)

    /// Determines whether two specified String objects have the same value, using StringComparison.Ordinal.(=fastest comparison)
    let (*inline*) equals strA strB  = 
        String.Equals(strA, strB, StringComparison.Ordinal)

    /// Determines whether two specified String objects have the same value, using StringComparison.OrdinalIgnoreCase.
    let (*inline*) equalsIgnoreCase strA strB = 
        String.Equals(strA, strB, StringComparison.OrdinalIgnoreCase )

    /// Reports the zero-based index of the first occurrence of the specified Unicode character in this string.
    let (*inline*) indexOfChar (charToFind:char) (stringToSearchIn:string)  = 
        if isNull stringToSearchIn then FsExStringException.Raise "String.indexOfChar: stringToSearchIn is null. (charToFind:%c) " charToFind
        stringToSearchIn.IndexOf(charToFind)

    /// Reports the zero-based index of the first occurrence of the specified Unicode character in this string. The search starts at a specified character position.
    let (*inline*) indexOfCharFrom (charToFind:char) startIndex (stringToSearchIn:string)  = 
        if isNull stringToSearchIn then FsExStringException.Raise "String.indexOfCharFrom: stringToSearchIn is null. (charToFind:%c)  (startIndex:%d) " charToFind startIndex
        stringToSearchIn.IndexOf(charToFind, startIndex)

    /// Reports the zero-based index of the first occurrence of the specified character in this instance. The search starts at a specified character position and examines a specified number of character positions.
    let (*inline*) indexOfCharFromFor (charToFind:char) startIndex count (stringToSearchIn:string)  = 
        if isNull stringToSearchIn then FsExStringException.Raise "String.indexOfCharFromFor : stringToSearchIn is null. (charToFind:%c)  (startIndex:%d)  (count:%d) " charToFind startIndex count
        stringToSearchIn.IndexOf(charToFind, startIndex, count)

    /// Reports the zero-based index of the first occurrence of the specified string in this instance, using StringComparison.Ordinal.
    let (*inline*) indexOfString (stringToFind:string) (stringToSearchIn:string)  = 
        if isNull stringToFind then FsExStringException.Raise "String.indexOfString: stringToFind is null. (stringToSearchIn:%s) " (exnf stringToSearchIn)
        if isNull stringToSearchIn then FsExStringException.Raise "String.indexOfString: stringToSearchIn is null. (stringToFind:%s) " (exnf stringToFind)
        stringToSearchIn.IndexOf(stringToFind, StringComparison.Ordinal)

    /// Reports the zero-based index of the first occurrence of the specified string in this instance.
    /// The search starts at a specified character position, using StringComparison.Ordinal.
    let (*inline*) indexOfStringFrom (stringToFind:string) (startIndex:int) (stringToSearchIn:string)  = 
        if isNull stringToFind then FsExStringException.Raise "String.indexOfStringFrom: stringToFind is null. (startIndex:%d)  (stringToSearchIn:%s) " startIndex (exnf stringToSearchIn)
        if isNull stringToSearchIn then FsExStringException.Raise "String.indexOfStringFrom: stringToSearchIn is null. (stringToFind:%s)  (startIndex:%d) " (exnf stringToFind) startIndex
        stringToSearchIn.IndexOf(stringToFind, startIndex,StringComparison.Ordinal)

    /// Reports the zero-based index of the first occurrence of the specified string in this instance.
    /// The search starts at a specified character position and examines a specified number of character positions, using StringComparison.Ordinal.
    let (*inline*) indexOfStringFromFor (stringToFind:string) (startIndex:int) (count:int) (stringToSearchIn:string)  = 
        if isNull stringToFind then FsExStringException.Raise "String.indexOfStringFromFor: stringToFind is null. (startIndex:%d)  (count:%d)  (stringToSearchIn:%s) " startIndex count (exnf stringToSearchIn)
        if isNull stringToSearchIn then FsExStringException.Raise "String.indexOfStringFromFor: stringToSearchIn is null. (stringToFind:%s)  (startIndex:%d)  (count:%d) " (exnf stringToFind) startIndex count
        stringToSearchIn.IndexOf(stringToFind, startIndex, count, StringComparison.Ordinal)

    /// Reports the zero-based index of the first occurrence in this instance of any character in a specified array of Unicode characters.
    let (*inline*) indexOfAny (anyOf:char[]) (stringToSearchIn:string)  = 
        if isNull stringToSearchIn then FsExStringException.Raise "String.indexOfAny: stringToSearchIn is null. (anyOf:%A) " anyOf
        stringToSearchIn.IndexOfAny(anyOf)

    /// Reports the zero-based index of the first occurrence in this instance of any character in a specified array of Unicode characters.
    /// The search starts at a specified character position.
    let (*inline*) indexOfAnyFrom (anyOf:char[]) startIndex (stringToSearchIn:string)  = 
        if isNull stringToSearchIn then FsExStringException.Raise "String.indexOfAnyFrom: stringToSearchIn is null. (anyOf:%A)  (startIndex:%d) " anyOf startIndex
        stringToSearchIn.IndexOfAny(anyOf, startIndex)

    /// Reports the zero-based index of the first occurrence in this instance of any character in a specified array of Unicode characters.
    /// The search starts at a specified character position and examines a specified number of character positions.
    let (*inline*) indexOfAnyFromFor (anyOf:char[]) startIndex count (stringToSearchIn:string)  = 
        if isNull stringToSearchIn then FsExStringException.Raise "String.indexOfAnyFromFor: stringToSearchIn is null. (anyOf:%A)  (startIndex:%d)  (count:%d) " anyOf startIndex count
        stringToSearchIn.IndexOfAny(anyOf, startIndex, count)

    /// Returns a new string in which a specified string is inserted at a specified index position in this instance.
    let (*inline*) insert startIndex (stringToInsert:string) (insertIntoThisString:string)  = 
        if isNull stringToInsert then FsExStringException.Raise "String.insert: stringToInsert is null. (startIndex:%d)  (insertIntoThisString:%s) " startIndex (exnf insertIntoThisString)
        if isNull insertIntoThisString then FsExStringException.Raise "String.insert: insertIntoThisString is null. (startIndex:%d)  (stringToInsert:%s) " startIndex (exnf stringToInsert)
        insertIntoThisString.Insert(startIndex, stringToInsert)

    /// Reports the zero-based index position of the last occurrence of a specified Unicode character within this instance.
    let (*inline*) lastIndexOfChar (charToFind:char) (stringToSearchIn:string)  = 
        if isNull stringToSearchIn then FsExStringException.Raise "String.lastIndexOfChar: stringToSearchIn is null. (charToFind:%c) " charToFind
        stringToSearchIn.LastIndexOf(charToFind)

    /// Reports the zero-based index position of the last occurrence of a specified Unicode character within this instance.
    /// The search starts at a specified character position and proceeds backward toward the beginning of the string.
    let (*inline*) lastIndexOfCharFrom (charToFind:char) startIndex (stringToSearchIn:string) = 
        if isNull stringToSearchIn then FsExStringException.Raise "String.lastIndexOfCharFrom : stringToSearchIn is null. (charToFind:%c)  (startIndex:%d) " charToFind startIndex
        stringToSearchIn.LastIndexOf(charToFind, startIndex)

    /// Reports the zero-based index position of the last occurrence of the specified Unicode character in a substring within this instance.
    /// The search starts at a specified character position and proceeds backward toward the beginning of the string for a specified number of character positions.
    let (*inline*) lastIndexOfCharFromFor  (charToFind:char) startIndex count (stringToSearchIn:string)  = 
        if isNull stringToSearchIn then FsExStringException.Raise "String.lastIndexOfCharFromFor: stringToSearchIn is null. (charToFind:%c)  (startIndex:%d)  (count:%d) " charToFind startIndex count
        stringToSearchIn.LastIndexOf(charToFind, startIndex, count)

    /// Reports the zero-based index position of the last occurrence of a specified string within this instance, using StringComparison.Ordinal.
    let (*inline*) lastIndexOfString (stringToFind:string) (stringToSearchIn:string)  = 
        if isNull stringToFind then FsExStringException.Raise "String.lastIndexOfString: stringToFind is null. (stringToSearchIn:%s) " (exnf stringToSearchIn)
        if isNull stringToSearchIn then FsExStringException.Raise "String.lastIndexOfString: stringToSearchIn is null. (stringToFind:%s) " (exnf stringToFind)
        stringToSearchIn.LastIndexOf(stringToFind, StringComparison.Ordinal)

    /// Reports the zero-based index position of the last occurrence of a specified string within this instance.
    /// The search starts at a specified character position and proceeds backward toward the beginning of the string, using StringComparison.Ordinal.
    let (*inline*) lastIndexOfStringFrom (stringToFind:string) (startIndex:int) (stringToSearchIn:string)  = 
        if isNull stringToFind then FsExStringException.Raise "String.lastIndexOfStringFrom: stringToFind is null. (startIndex:%d)  (stringToSearchIn:%s) " startIndex (exnf stringToSearchIn)
        if isNull stringToSearchIn then FsExStringException.Raise "String.lastIndexOfString': stringToSearchIn is null. (stringToFind:%s)  (startIndex:%d) " (exnf stringToFind) startIndex
        stringToSearchIn.LastIndexOf(stringToFind, startIndex, StringComparison.Ordinal)

    /// Reports the zero-based index position of the last occurrence of a specified string within this instance.
    /// The search starts at a specified character position and proceeds backward toward the beginning of the string for a specified number of character positions, using StringComparison.Ordinal.
    let (*inline*) lastIndexOfStringFromFor  (stringToFind:string) (startIndex:int) (count:int) (stringToSearchIn:string)  = 
        if isNull stringToFind then FsExStringException.Raise "String.lastIndexOfStringFromFor : stringToFind is null. (startIndex:%d)  (count:%d)  (stringToSearchIn:%s) " startIndex count (exnf stringToSearchIn)
        if isNull stringToSearchIn then FsExStringException.Raise "String.lastIndexOfStringFromFor : stringToSearchIn is null. (stringToFind:%s)  (startIndex:%d)  (count:%d) " (exnf stringToFind) startIndex count
        stringToSearchIn.LastIndexOf(stringToFind, startIndex, count, StringComparison.Ordinal)


    /// Returns a new string that right-aligns the characters in this instance by padding them with spaces on the left, for a specified total length.
    let (*inline*) padLeft totalWidth (txt:string)  = 
        if isNull txt then FsExStringException.Raise "String.padLeft: txt is null. (totalWidth:%d) " totalWidth
        txt.PadLeft(totalWidth)

    /// Returns a new string that right-aligns the characters in this instance by padding them on the left with a specified Unicode character, for a specified total length.
    let (*inline*) padLeftWith totalWidth (paddingChar:char) (txt:string)  = 
        if isNull txt then FsExStringException.Raise "String.padLeftWith: txt is null. (totalWidth:%d)  (paddingChar:%c) " totalWidth paddingChar
        txt.PadLeft(totalWidth, paddingChar)

    /// Returns a new string that left-aligns the characters in this string by padding them with spaces on the right, for a specified total length.
    let (*inline*) padRight totalWidth (txt:string)  = 
        if isNull txt then FsExStringException.Raise "String.padRight: txt is null. (totalWidth:%d) " totalWidth
        txt.PadRight(totalWidth)

    /// Returns a new string that left-aligns the characters in this string by padding them on the right with a specified Unicode character, for a specified total length.
    let (*inline*) padRightWith totalWidth (paddingChar:char) (txt:string)  = 
        if isNull txt then FsExStringException.Raise "String.padRightWith: txt is null. (totalWidth:%d)  (paddingChar:%c) " totalWidth paddingChar
        txt.PadRight(totalWidth, paddingChar)

    /// Returns a new string in which all the characters in the current instance, beginning at a specified position and continuing through the last position, have been deleted.
    let (*inline*) remove startIndex (txt:string)  = 
        if isNull txt then FsExStringException.Raise "String.remove: txt is null. (startIndex:%d) " startIndex
        txt.Remove(startIndex)

    /// Returns a new string in which a specified number of characters in the current instance beginning at a specified position have been deleted.
    let (*inline*) removeFrom startIndex count (txt:string)  = 
        if isNull txt then FsExStringException.Raise "String.removeFrom : txt is null. (startIndex:%d)  (count:%d) " startIndex count
        txt.Remove(startIndex, count)

    /// Returns a new string in which all occurrences of a specified Unicode character in this instance are replaced with another specified Unicode character.
    let (*inline*) replaceChar (oldChar:char) (newChar:char) (txt:string)  = 
        if isNull txt then FsExStringException.Raise "String.replace': txt is null. (oldChar:%c)  (newChar:%c) " oldChar newChar
        txt.Replace(oldChar, newChar) // will return the same instance if char to replace is not found

    /// Returns a new string in which all occurrences of a specified string in the current instance are replaced with another specified string.
    /// (Will return the same instance if text to replace is not found)
    let (*inline*) replace (oldValue:string) (newValue:string) (txt:string)  = 
        if isNull oldValue then FsExStringException.Raise "String.replace: oldValue is null. (newValue:%s)  (txt:%s) " (exnf newValue) (exnf txt)
        if isNull newValue then FsExStringException.Raise "String.replace: newValue is null. (oldValue:%s)  (txt:%s) " (exnf oldValue) (exnf txt)
        if isNull txt then FsExStringException.Raise "String.replace: txt is null. (oldValue:%s)  (newValue:%s) " (exnf oldValue) (exnf newValue)
        txt.Replace(oldValue, newValue) // will return the same instance if text to replace is not found

    /// Concats string with Environment.NewLine
    let inline concatLines  (lines:string seq) = 
        String.concat Environment.NewLine lines

    //-----------split by string overloads --------------

    /// Split string by "\r\n", "\r" and "\n"
    let (*inline*) splitLines  (stringToSplit:string) = 
        if isNull stringToSplit then FsExStringException.Raise "String.splitLines: stringToSplit is null"
        stringToSplit.Split( [| "\r\n"; "\r"; "\n" |] , StringSplitOptions.None)


    /// Split string, Remove Empty Entries
    /// Like: string.Split([| splitter |], StringSplitOptions.RemoveEmptyEntries)
    let (*inline*) split (splitter:string) (stringToSplit:string) = 
        if isNull stringToSplit then FsExStringException.Raise "String.split: stringToSplit is null (splitter:%s)" (exnf splitter)
        if isNull splitter      then FsExStringException.Raise "String.split: splitter is null (stringToSplit:%s)" (exnf stringToSplit)
        stringToSplit.Split([|splitter|], StringSplitOptions.RemoveEmptyEntries)

    /// Split string, Keep Empty Entries
    /// Like : string.Split([| splitter |], StringSplitOptions.None)
    let (*inline*) splitKeep (splitter:string) (stringToSplit:string) = 
        if isNull stringToSplit then FsExStringException.Raise "String.splitKeep: stringToSplit is null (splitter:%s)" (exnf splitter)
        if isNull splitter      then FsExStringException.Raise "String.splitKeep: splitter is null (stringToSplit:%s)" (exnf stringToSplit)
        stringToSplit.Split([|splitter|], StringSplitOptions.None)

    //-----------split by Char overloads --------------

    /// Split string by a Char, Remove Empty Entries
    /// Like: string.Split([| splitter |], StringSplitOptions.RemoveEmptyEntries)
    let (*inline*) splitChar (separator:char) (stringToSplit:string)  = 
        if isNull stringToSplit then FsExStringException.Raise "String.splitChar: stringToSplit is null. (separator:%c) " separator
        stringToSplit.Split([| separator|] , StringSplitOptions.RemoveEmptyEntries)

    /// Split string by any of multiple Chars, Remove Empty Entries
    /// Like: string.Split([| splitter |], StringSplitOptions.RemoveEmptyEntries)
    let (*inline*) splitChars(separators:char[]) (stringToSplit:string)  = 
        if isNull stringToSplit then FsExStringException.Raise "String.splitByChars: stringToSplit is null. (separators:%A)  " separators
        stringToSplit.Split(separators, StringSplitOptions.RemoveEmptyEntries)

    /// Split string by any of multiple Chars, Keep Empty Entries
    /// Like : string.Split([| splitter |], StringSplitOptions.None)
    let (*inline*) splitCharKeep (separator:char) (stringToSplit:string)  = 
        if isNull stringToSplit then FsExStringException.Raise "String.splitCharKeep: stringToSplit is null. (separator:%c) " separator
        stringToSplit.Split(separator)

    /// Split string by a Char, Keep Empty Entries
    /// Like : string.Split([| splitter |], StringSplitOptions.None)
    let (*inline*) splitCharsKeep (separators:char[]) (stringToSplit:string)  = 
        if isNull stringToSplit then FsExStringException.Raise "String.splitCharsKeep: stringToSplit is null. (separators:%A)" separators
        stringToSplit.Split(separators)

    //---------------------------- end split -----------------


    /// Retrieves a substring from this instance. The substring starts at a specified character position and continues to the end of the string.
    let (*inline*) substringFrom startIndex (txt:string)  = 
        if isNull txt then FsExStringException.Raise "String.substringFrom: txt is null. (startIndex:%d) " startIndex
        txt.Substring(startIndex)

    /// Retrieves a substring from this instance. The substring starts at a specified character position and has a specified length.
    let (*inline*) substringFromFor startIndex length (txt:string)  = 
        if isNull txt then FsExStringException.Raise "String.substringFromFor : txt is null. (startIndex:%d)  (length:%d) " startIndex length
        txt.Substring(startIndex, length)

    /// Copies the characters in this instance to a Unicode character array.
    let (*inline*) toCharArray (txt:string)  = 
        if isNull txt then FsExStringException.Raise "String.toCharArray: txt is null."
        txt.ToCharArray()

    /// Copies the characters in a specified substring in this instance to a Unicode character array.
    let (*inline*) toCharArrayFromFor  startIndex length (txt:string)  = 
        if isNull txt then FsExStringException.Raise "String.toCharArrayFromFor : txt is null. (startIndex:%d)  (length:%d) " startIndex length
        txt.ToCharArray(startIndex, length)

    /// Returns a copy of this String object converted to lowercase using the casing rules of the invariant culture.
    let (*inline*) toLower (txt:string)  = 
        if isNull txt then FsExStringException.Raise "String.toLower: txt is null."
        txt.ToLowerInvariant()

    /// Returns a copy of this String object converted to uppercase using the casing rules of the invariant culture.
    let (*inline*) toUpper (txt:string)  = 
        if isNull txt then FsExStringException.Raise "String.toUpper: txt is null."
        txt.ToUpperInvariant()

    // -------------------------trim familly-------------

    /// Removes all leading and trailing white-space characters from the current String object.
    let (*inline*) trim (txt:string)  = 
        if isNull txt then FsExStringException.Raise "String.trim: txt is null."
        txt.Trim()

    /// Removes all leading and trailing occurrences of a set of characters specified in an array from the current String object.
    let (*inline*) trimChar (trimChar:char) (txt:string)  = 
        if isNull txt then FsExStringException.Raise "String.trimChar: txt is null."
        txt.Trim([|trimChar|])

    /// Removes all leading and trailing occurrences of a set of characters specified in an array from the current String object.
    let (*inline*) trimChars (trimChars:char[]) (txt:string)  = 
        if isNull txt then FsExStringException.Raise "String.trimChars: txt is null. (trimChars:%A) " trimChars
        txt.Trim(trimChars)

    /// Removes all trailing whitespace.
    let (*inline*) trimEnd (txt:string)  = 
        if isNull txt then FsExStringException.Raise "String.trimEnd: txt is null. (trimChars:%A) " trimChars
        txt.TrimEnd()

    /// Removes all trailing occurrences of a  characters specified in an array from the current String object.
    let (*inline*) trimEndChar (trimChar:char) (txt:string)  = 
        if isNull txt then FsExStringException.Raise "String.trimEndChar: txt is null. (trimChars:%A) " trimChars
        txt.TrimEnd([|trimChar|])

    /// Removes all trailing occurrences of a set of characters specified in an array from the current String object.
    let (*inline*) trimEndChars (trimChars:char[]) (txt:string)  = 
        if isNull txt then FsExStringException.Raise "String.trimEndChars: txt is null. (trimChars:%A) " trimChars
        txt.TrimEnd(trimChars)

    /// Removes all leading whitespace.
    let (*inline*) trimStart (txt:string)  = 
        if isNull txt then FsExStringException.Raise "String.trimStart: txt is null. (trimChars:%A) " trimChars
        txt.TrimStart()

    /// Removes all leading occurrences of a characters specified in an array from the current String object.
    let (*inline*) trimStartChar (trimChar:char) (txt:string)  = 
        if isNull txt then FsExStringException.Raise "String.trimStartChar: txt is null. (trimChars:%A) " trimChars
        txt.TrimStart(trimChar)

    /// Removes all leading occurrences of a set of characters specified in an array from the current String object.
    let (*inline*) trimStartChars (trimChars:char[]) (txt:string)  = 
        if isNull txt then FsExStringException.Raise "String.trimStartChars: txt is null. (trimChars:%A) " trimChars
        txt.TrimStart(trimChars)


