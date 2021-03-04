namespace FsEx

open System
open System.Text
open System.Runtime.CompilerServices
open FsEx.SaveIgnore //so that  |> ignore  can only be used on value types


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] //need this so doesn't hide String class in C# assemblies (should consider for other extension modules as well)
module String =   
           
    /// An Exception for the string functions defined in FsEx
    type FsExStringException(txt:string)=
        inherit Exception(txt)
            
        /// Raise the exception with F# printf string formating
        static member inline Raise msg = Printf.kprintf (fun s -> raise (new FsExStringException(s))) msg 

    
    /// Trimm strings to 30 chars for showing in in one line 
    //  it returns the input string trimmed to 30 Chars, a count of skiped characters and the last 5 characters
    /// Replace line breaks with \r\n literal
    /// Does not include sourrounding quotes
    /// If stirng is null returns "-null string-"
    let truncateFormatedInOneLine (stringToTrim:string) =
        if isNull stringToTrim then "-null string-"
        else
            let s = 
                let maxChars = 30
                if stringToTrim.Length <= maxChars + 20 then sprintf "\"%s\"" stringToTrim
                else 
                    let len   = stringToTrim.Length
                    let st    = stringToTrim.Substring(0,maxChars) 
                    let last5 = stringToTrim.Substring(len-6) 
                    sprintf "\"%s[..%d more Chars..]%s\"" st (len - maxChars - 5) last5
            s.Replace("\r","\\r").Replace("\n","\\n")
    
    /// for  string formating in exceptions below.
    /// Inclouding sourounding quotes
    let internal exnf s = 
        if isNull s then "-null string-"  else "\"" + truncateFormatedInOneLine s + "\"" //separate null check so noull value is not in quotes

    /// TODO remove inline to get compile times down in FSI ?
   
    /// removes substring from a string. same as:
    /// fromString.Replace(textToRemove, "")
    let (*inline*) delete (textToRemove:string) (fromString:string)=
        if isNull fromString   then FsExStringException.Raise "String.delete: fromString is null (textToRemove:%s)" (exnf textToRemove)
        if isNull textToRemove then FsExStringException.Raise "String.delete: textToRemove is null (fromString:%s)"  (exnf fromString)
        fromString.Replace(textToRemove, "")

    // Ensures all lines end on System.Environment.NewLine
    // code: StringBuilder(s).Replace("\r\n", "\n").Replace("\r", "\n").Replace("\n", Environment.NewLine).ToString()
    let (*inline*) unifyLineEndings (txt:string) =             
        if isNull txt then FsExStringException.Raise "String.unifyLineEndings: input is null "
        StringBuilder(txt).Replace("\r\n", "\n").Replace("\r", "\n").Replace("\n", Environment.NewLine).ToString()

    /// Returns everytrhing before first occurence of a given splitting string.
    /// Or fails if splitter is not found
    /// using StringComparison.Ordinal
    let (*inline*) before (splitter:string) (stringToSearchIn:string) = 
        if isNull stringToSearchIn then FsExStringException.Raise "String.before: stringToSearchIn is null (splitter:%s)" (exnf splitter)
        if isNull splitter         then FsExStringException.Raise "String.before: splitter is null (stringToSearchIn:%s)" (exnf stringToSearchIn)
        let start = stringToSearchIn.IndexOf(splitter, StringComparison.Ordinal) 
        if start = -1 then FsExStringException.Raise "String.before: splitter %s not found in stringToSearchIn:%s" (exnf splitter) (exnf stringToSearchIn)
        else stringToSearchIn.Substring(0, start)

    /// Returns everytrhing before first occurence of a given splitting character.
    /// Or fails if splitter is not found
    let (*inline*) beforeChar (splitter:char) (stringToSearchIn:string) = 
        if isNull stringToSearchIn then FsExStringException.Raise "String.beforeChar: stringToSearchIn is null (splitter:%c)" (splitter)
        let start = stringToSearchIn.IndexOf(splitter) 
        if start = -1 then FsExStringException.Raise "String.before: splitter '%c' not found in stringToSearchIn:%s" splitter (exnf stringToSearchIn)
        else stringToSearchIn.Substring(0, start)
    
    /// Returns everytrhing before first occurence of a given splitting string.
    /// Or full string if splitter is not found
    /// using StringComparison.Ordinal
    let (*inline*) beforeMaybe (splitter:string) (stringToSearchIn:string) = 
        if isNull stringToSearchIn then FsExStringException.Raise "String.before: stringToSearchIn is null (splitter:%s)" (exnf splitter)
        if isNull splitter         then FsExStringException.Raise "String.before: splitter is null (stringToSearchIn:%s)" (exnf stringToSearchIn)
        let start = stringToSearchIn.IndexOf(splitter, StringComparison.Ordinal) 
        if start = -1 then stringToSearchIn
        else stringToSearchIn.Substring(0, start)

    /// Returns everytrhing before first occurence of a given splitting character.
    /// Or full string if splitter is not found
    let (*inline*) beforeCharMaybe (splitter:char) (stringToSearchIn:string) = 
        if isNull stringToSearchIn then FsExStringException.Raise "String.beforeChar: stringToSearchIn is null (splitter:%c)" (splitter)
        let start = stringToSearchIn.IndexOf(splitter) 
        if start = -1 then stringToSearchIn
        else stringToSearchIn.Substring(0, start)

    /// Returns everytrhing before first occurence of a given splitting string.
    /// Or fails if splitter is not found
    /// using StringComparison.Ordinal
    let (*inline*) after (splitter:string) (stringToSearchIn:string) = 
        if isNull stringToSearchIn then FsExStringException.Raise "String.after: stringToSearchIn is null (splitter:%s)" (exnf splitter)
        if isNull splitter         then FsExStringException.Raise "String.after: splitter is null (stringToSearchIn:%s)" (exnf stringToSearchIn)
        let start = stringToSearchIn.IndexOf(splitter, StringComparison.Ordinal) 
        if start = -1 then FsExStringException.Raise "String.after: splitter %s not found in stringToSearchIn:%s" (exnf splitter) (exnf stringToSearchIn)
        else stringToSearchIn.Substring(start+1)//cant be out of bounds!

    /// Returns everytrhing after first occurence of a given splitting character.
    /// Or fails if splitter is not found
    let (*inline*) afterChar (splitter:char) (stringToSearchIn:string) = 
        if isNull stringToSearchIn then FsExStringException.Raise "String.afterChar: stringToSearchIn is null (splitter:%c)" (splitter)
        let start = stringToSearchIn.IndexOf(splitter) 
        if start = -1 then FsExStringException.Raise "String.after: splitter '%c' not found in stringToSearchIn:%s" splitter (exnf stringToSearchIn)
        else stringToSearchIn.Substring(start+1)//cant be out of bounds!
    
    /// Returns everytrhing after first occurence of a given splitting string.
    /// Or full string if splitter is not found
    /// using StringComparison.Ordinal
    let (*inline*) afterMaybe (splitter:string) (stringToSearchIn:string) = 
        if isNull stringToSearchIn then FsExStringException.Raise "String.after: stringToSearchIn is null (splitter:%s)" (exnf splitter)
        if isNull splitter         then FsExStringException.Raise "String.after: splitter is null (stringToSearchIn:%s)" (exnf stringToSearchIn)
        let start = stringToSearchIn.IndexOf(splitter, StringComparison.Ordinal) 
        if start = -1 then stringToSearchIn
        else stringToSearchIn.Substring(start+1)//cant be out of bounds!

    /// Returns everytrhing after first occurence of a given splitting character.
    /// Or full string if splitter is not found
    let (*inline*) afterCharMaybe (splitter:char) (stringToSearchIn:string) = 
        if isNull stringToSearchIn then FsExStringException.Raise "String.afterChar: stringToSearchIn is null (splitter:%c)" (splitter)
        let start = stringToSearchIn.IndexOf(splitter) 
        if start = -1 then stringToSearchIn
        else stringToSearchIn.Substring(start+1)//cant be out of bounds!

    /// split string, Remove Empty Entries
    /// like: string.Split([| splitter |], StringSplitOptions.RemoveEmptyEntries)
    let (*inline*) split (splitter:string) (stringToSplit:string) = 
        if isNull stringToSplit then FsExStringException.Raise "String.split: stringToSplit is null (splitter:%s)" (exnf splitter)
        if isNull splitter      then FsExStringException.Raise "String.split: splitter is null (stringToSplit:%s)" (exnf stringToSplit)
        stringToSplit.Split([|splitter|], StringSplitOptions.RemoveEmptyEntries)
    
    /// split string, Keep Empty Entries
    /// like : string.Split([| splitter |], StringSplitOptions.None)  
    let (*inline*) splitKeep (splitter:string) (stringToSplit:string) = 
        if isNull stringToSplit then FsExStringException.Raise "String.splitKeep: stringToSplit is null (splitter:%s)" (exnf splitter)
        if isNull splitter      then FsExStringException.Raise "String.splitKeep: splitter is null (stringToSplit:%s)" (exnf stringToSplit)
        stringToSplit.Split([|splitter|], StringSplitOptions.None)    
    
    /// Split string into two elements, 
    /// Fails if  splitter is not found.
    /// The splitter is not included in the two return strings.
    let (*inline*) splitOnce (splitter:string) (stringToSplit:string) = 
        if isNull stringToSplit then FsExStringException.Raise "String.splitOnce: stringToSplit is null (splitter:%s)" (exnf splitter)
        if isNull splitter         then FsExStringException.Raise "String.splitOnce: splitter is null (stringToSplit:%s)" (exnf stringToSplit)
        let start = stringToSplit.IndexOf(splitter, StringComparison.Ordinal) 
        if start = -1 then FsExStringException.Raise "String.splitOnce: splitter %s not found in stringToSplit: %s" (exnf splitter) (exnf stringToSplit)
        else               stringToSplit.Substring(0, start), stringToSplit.Substring(start + splitter.Length)
    
    /// Finds text betwween two strings
    /// e.g.: between "X" "T" "cXabTk" = "c", "ab", "k"
    /// Failsn if not both splitters are found.
    /// Delimiters are excluded in the three returned strings
    let (*inline*) splitTwice (firstSplitter:string) (secondSplitter:string) (stringToSplit:string) =         
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
    /// If splitter not found first string is same as input, second string is empty 
    /// Splitter is not included in the two return strings.
    let (*inline*) splitMaybeOnce (splitter:string) (stringToSplit:string) = 
        if isNull stringToSplit then FsExStringException.Raise "String.splitOnce: stringToSplit is null (splitter:%s)" (exnf splitter)
        if isNull splitter         then FsExStringException.Raise "String.splitOnce: splitter is null (stringToSplit:%s)" (exnf stringToSplit)
        let start = stringToSplit.IndexOf(splitter, StringComparison.Ordinal) 
        if start = -1 then stringToSplit,""
        else               stringToSplit.Substring(0, start), stringToSplit.Substring(start + splitter.Length)
    
    /// Finds text betwween two strings
    /// e.g.: between "X" "T" "cXabTk" = "c", "ab", "k"
    /// If not both splitters are found returns original string and two empty strings 
    /// Delimiters are excluded in the three returned strings
    let (*inline*) splitMaybeTwice (firstSplitter:string) (secondSplitter:string) (stringToSplit:string) =         
        if isNull stringToSplit  then FsExStringException.Raise "String.splitTwice: stringToSplit is null (firstSplitter:%s, secondSplitter:%s)" (exnf firstSplitter) (exnf secondSplitter)
        if isNull firstSplitter  then FsExStringException.Raise "String.splitTwice: firstSplitter is null (stringToSplit:%s, secondSplitter:%s)" (exnf stringToSplit)(exnf secondSplitter)
        if isNull secondSplitter then FsExStringException.Raise "String.splitTwice: secondSplitter is null (stringToSplit:%s, firstSplitter:%s)" (exnf stringToSplit)(exnf firstSplitter)        
        let start = stringToSplit.IndexOf(firstSplitter, StringComparison.Ordinal) 
        if start = -1 then stringToSplit,"",""
        else 
            let ende = stringToSplit.IndexOf(secondSplitter, start + firstSplitter.Length, StringComparison.Ordinal)
            if ende = -1 then stringToSplit,"",""
            else 
                stringToSplit.Substring(0, start ),
                stringToSplit.Substring(start + firstSplitter.Length, ende - start - firstSplitter.Length),// finds text betwween two chars
                stringToSplit.Substring(ende + secondSplitter.Length)

  
    /// First letter of string to Uppercase
    let (*inline*) up1 (txt:string)  = 
        if isNull txt then FsExStringException.Raise "String.up1: string is null" 
        if txt="" || Char.IsUpper txt.[0] then txt 
        elif Char.IsLetter txt.[0] then  Char.ToUpper(txt.[0]).ToString() + txt.Substring(1) 
        else txt
    
    /// First letter of string to Lowercase
    let (*inline*) low1 (txt:string) = 
        if isNull txt then FsExStringException.Raise "String.low11: string is null" 
        if txt="" || Char.IsLower txt.[0] then txt 
        elif Char.IsLetter txt.[0] then  Char.ToLower(txt.[0]).ToString() + txt.Substring(1) 
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
    
    /// Fills the beginning of a string with the filler character 
    /// until it has reached the desired length
    /// (usefull to add zeros at the beginning of numbers for sorting)
    let (*inline*) prefixToLength desiredLength (fillerChar:char) strToFill = 
        if isNull strToFill then FsExStringException.Raise "String.prefixToLength: strToFill is null" 
        let len = String.length strToFill
        if len>desiredLength then 
            strToFill // FsExStringException.Raise "String.prefixToLength %s cant be filled to length %d because it is already %d long." strToFill desiredLength len
        elif len = desiredLength then 
            strToFill
        else 
            new String(fillerChar, desiredLength-len) + strToFill

    /// Counts how offten a substring appears in a string
    /// (using StringComparison.Ordinal)
    let (*inline*) countSubString (subString:string) (textToSerach:string) =
        if isNull textToSerach then FsExStringException.Raise "String.countSubString: textToSerach is null,   subString: %s" (exnf subString)
        if isNull subString    then FsExStringException.Raise "String.countSubString: subString is null,   textToSerach: %s" (exnf textToSerach)
        let rec find fromIdx k = 
            let r = textToSerach.IndexOf(subString, fromIdx, StringComparison.Ordinal)
            if r < 0 then k 
            else find (r + subString.Length) (k + 1)
        find 0 0
    
    /// removes accents & diacritics from characters
    /// eventually returns string.Normalize(NormalizationForm.FormC)
    let normalize (txt:string ) = 
        if isNull txt then FsExStringException.Raise "String.normalize: txt is null" 
        // better: https://github.com/apache/lucenenet/blob/master/src/Lucene.Net.Analysis.Common/Analysis/Miscellaneous/ASCIIFoldingFilter.cs
        // https://stackoverflow.com/questions/249087/how-do-i-remove-diacritics-accents-from-a-string-in-net
        txt.Normalize(System.Text.NormalizationForm.FormD)
        |> Seq.filter ( fun c -> Globalization.CharUnicodeInfo.GetUnicodeCategory(c) <> Globalization.UnicodeCategory.NonSpacingMark  )
        |> String.Concat
        |> fun s -> txt.Normalize(NormalizationForm.FormC)
    
    /// Returns true if specified character occurs within this string.
    let (*inline*) containsChar (charToFind:char) (stringToSearchIn:string) = 
        if isNull stringToSearchIn then FsExStringException.Raise "String.containsChar: stringToSearchIn is null, char: '%c'" charToFind 
        stringToSearchIn.IndexOf(charToFind) <> -1

    /// Returns true if specified charcter does NOT occurs within this string.
    let (*inline*) notContainsChar (charToFind:char) (stringToSearchIn:string) = 
        if isNull stringToSearchIn then FsExStringException.Raise "String.notContainsChar: stringToSearchIn is null, char: '%c'" charToFind 
        stringToSearchIn.IndexOf(charToFind) = -1

    /// Ensures string is maximum maxChars long
    let (*inline*) truncate (maxChars:int) (stringToTrim:string) =
        if isNull stringToTrim then FsExStringException.Raise "String.truncate: stringToSearchIn is null, maxChar: %d" maxChars 
        if stringToTrim.Length <= maxChars then stringToTrim
        else stringToTrim.Substring(0,maxChars)
       
    /// If the input string is longer than maxChars variable plus twenty tolerance then 
    /// it returns the input string trimmed to maxChars, a count of skiped characters and the last 5 characters
    /// e.g. "abcde[..20 more Chars..]vwxyz"
    /// Else, if the input string is less than maxChars + 20, it is still returned in full. 
    /// Not enclosed inquotes.
    /// Fails on null.
    /// Alternativly use String.formatInOneShortLine thaty will not fail on null.
    let (*inline*) truncateFormated (maxChars:int) (stringToTrim:string) =
        if isNull stringToTrim then FsExStringException.Raise "String.truncateFormated: stringToSearchIn is null, maxChar: %d" maxChars 
        if stringToTrim.Length <= maxChars + 20 then sprintf "%s"stringToTrim
        else 
            let len   = stringToTrim.Length
            let st    = stringToTrim.Substring(0,maxChars) 
            let last6 = stringToTrim.Substring(len-6) 
            sprintf "%s[..%d more Chars..]%s" st (len - maxChars - 5) last6
            

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

    /// Compares two specified String objects and returns an integer that indicates their relative position in the sort order, u
    /// using StringComparison.Ordinal.
    let (*inline*) compare strA strB = 
        if isNull strA then FsExStringException.Raise "String.compare: strA is null, strB: %s" (exnf strB)
        if isNull strB then FsExStringException.Raise "String.compare: strB is null, strA: %s" (exnf strA)    
        String.Compare(strA, strB, StringComparison.Ordinal)

    /// Compares two specified String objects and returns an integer that indicates their relative position in the sort order, 
    /// using StringComparison.OrdinalIgnoreCase.
    let (*inline*) compareIgnoreCase strA strB = 
        if isNull strA then FsExStringException.Raise "String.compareIgnoreCase: strA is null, strB: %s" (exnf strB)
        if isNull strB then FsExStringException.Raise "String.compareIgnoreCase: strB is null, strA: %s" (exnf strA)  
        String.Compare(strA, strB, StringComparison.OrdinalIgnoreCase )

    /// Determines whether the end of this string instance matches the specified string, using StringComparison.Ordinal.
    let (*inline*) endsWith (stringToFindAtEnd : string) (stringSerachInAtEnd:string) =  stringSerachInAtEnd.EndsWith(stringToFindAtEnd, StringComparison.Ordinal)

    /// Determines whether the end of this string instance matches the specified string, using StringComparison.OrdinalIgnoreCase.
    let (*inline*) endsWithIgnoreCase (stringToFindAtEnd : string) (stringSerachInAtEnd:string) =   stringSerachInAtEnd.EndsWith(stringToFindAtEnd, StringComparison.OrdinalIgnoreCase)

    // Determines whether the end of this string instance matches the specified string when compared using the specified culture.
    // let (*inline*) endsWith' stringToFindAtEnd ignoreCase culture (txt:string) = txt.EndsWith(stringToFindAtEnd, ignoreCase, culture)
    // Determines whether the end of this string instance matches the specified string when compared using the specified comparison option.
    // let (*inline*) endsWith'' stringToFindAtEnd comparisonType (txt:string) = txt.EndsWith(charToFind, comparisonType)
    
    /// Determines whether two specified String objects have the same value, using StringComparison.Ordinal.(=fastest comparison)
    let (*inline*) equals strA strB = String.Equals(strA, strB, StringComparison.Ordinal)

    /// Determines whether two specified String objects have the same value, using StringComparison.OrdinalIgnoreCase.
    let (*inline*) equalsIgnoreCase strA strB= String.Equals(strA, strB, StringComparison.OrdinalIgnoreCase )

    /// Reports the zero-based index of the first occurrence of the specified Unicode character in this string.
    let (*inline*) indexOfChar (charToFind:char) (stringToSearchIn:string) = stringToSearchIn.IndexOf(charToFind)

    /// Reports the zero-based index of the first occurrence of the specified Unicode character in this string. The search starts at a specified character position.
    let (*inline*) indexOfChar' (charToFind:char) startIndex (stringToSearchIn:string) = stringToSearchIn.IndexOf(charToFind, startIndex)

    /// Reports the zero-based index of the first occurrence of the specified character in this instance. The search starts at a specified character position and examines a specified number of character positions.
    let (*inline*) indexOfChar'' (charToFind:char) startIndex count (stringToSearchIn:string) = stringToSearchIn.IndexOf(charToFind, startIndex, count)

    /// Reports the zero-based index of the first occurrence of the specified string in this instance, using StringComparison.Ordinal.
    let (*inline*) indexOfString (stringToFind:string) (stringToSearchIn:string) = stringToSearchIn.IndexOf(stringToFind, StringComparison.Ordinal)

    /// Reports the zero-based index of the first occurrence of the specified string in this instance. 
    /// The search starts at a specified character position, using StringComparison.Ordinal.
    let (*inline*) indexOfString' (stringToFind:string) (startIndex:int) (stringToSearchIn:string) = stringToSearchIn.IndexOf(stringToFind, startIndex,StringComparison.Ordinal)

    /// Reports the zero-based index of the first occurrence of the specified string in this instance. 
    /// The search starts at a specified character position and examines a specified number of character positions, using StringComparison.Ordinal.
    let (*inline*) indexOfString'' (stringToFind:string) (startIndex:int) (count:int) (stringToSearchIn:string) = stringToSearchIn.IndexOf(stringToFind, startIndex, count, StringComparison.Ordinal)

    // Reports the zero-based index of the first occurrence of the specified string in the current String object. 
    // A parameter specifies the type of search to use for the specified string.
    // let (*inline*) indexOfStringWithComparison value (comparisonType:StringComparison) (stringToSearchIn:string) = stringToSearchIn.IndexOf(value, comparisonType)    
    // Reports the zero-based index of the first occurrence of the specified string in the current String object. 
    // Parameters specify the starting search position in the current string and the type of search to use for the specified string.
    // let (*inline*) indexOfStringWithComparison' value startIndex (comparisonType:StringComparison) (stringToSearchIn:string) = stringToSearchIn.IndexOf(value, startIndex, comparisonType)

    /// Reports the zero-based index of the first occurrence of the specified string in the current String object. 
    /// Parameters specify the starting search position in the current string, the number of characters in the current string to search, and the type of search to use for the specified string.
    let (*inline*) indexOfStringWithComparison'' (stringToFind:string) startIndex count comparisonType (stringToSearchIn:string) = stringToSearchIn.IndexOf(stringToFind, startIndex, count, comparisonType)

    /// Reports the zero-based index of the first occurrence in this instance of any character in a specified array of Unicode characters.
    let (*inline*) indexOfAny anyOf (stringToSearchIn:string) = stringToSearchIn.IndexOfAny(anyOf)

    /// Reports the zero-based index of the first occurrence in this instance of any character in a specified array of Unicode characters. 
    /// The search starts at a specified character position.
    let (*inline*) indexOfAny' anyOf startIndex (stringToSearchIn:string) = stringToSearchIn.IndexOfAny(anyOf, startIndex)

    /// Reports the zero-based index of the first occurrence in this instance of any character in a specified array of Unicode characters. The search starts at a specified character position and examines a specified number of character positions.
    let (*inline*) indexOfAny'' anyOf startIndex count (stringToSearchIn:string) = stringToSearchIn.IndexOfAny(anyOf, startIndex, count)

    /// Returns a new string in which a specified string is inserted at a specified index position in this instance.
    let (*inline*) insert startIndex (stringToInsert:string) (insertIntoThisString:string) = insertIntoThisString.Insert(startIndex, stringToInsert)

    /// Reports the zero-based index position of the last occurrence of a specified Unicode character within this instance.
    let (*inline*) lastIndexOfChar (charToFind:char) (stringToSearchIn:string) = stringToSearchIn.LastIndexOf(charToFind)

    /// Reports the zero-based index position of the last occurrence of a specified Unicode character within this instance. The search starts at a specified character position and proceeds backward toward the beginning of the string.
    let (*inline*) lastIndexOfChar' (charToFind:char) startIndex (stringToSearchIn:string)= stringToSearchIn.LastIndexOf(charToFind, startIndex)

    /// Reports the zero-based index position of the last occurrence of the specified Unicode character in a substring within this instance. The search starts at a specified character position and proceeds backward toward the beginning of the string for a specified number of character positions.
    let (*inline*) lastIndexOfChar'' (charToFind:char) startIndex count (stringToSearchIn:string) = stringToSearchIn.LastIndexOf(charToFind, startIndex, count)

    /// Reports the zero-based index position of the last occurrence of a specified string within this instance, using StringComparison.Ordinal.
    let (*inline*) lastIndexOfString (stringToFind:string) (stringToSearchIn:string) = stringToSearchIn.LastIndexOf(stringToFind, StringComparison.Ordinal)

    /// Reports the zero-based index position of the last occurrence of a specified string within this instance. 
    /// The search starts at a specified character position and proceeds backward toward the beginning of the string, using StringComparison.Ordinal.
    let (*inline*) lastIndexOfString' (stringToFind:string) (startIndex:int) (stringToSearchIn:string) = stringToSearchIn.LastIndexOf(stringToFind, startIndex, StringComparison.Ordinal)

    /// Reports the zero-based index position of the last occurrence of a specified string within this instance. 
    /// The search starts at a specified character position and proceeds backward toward the beginning of the string for a specified number of character positions, using StringComparison.Ordinal.
    let (*inline*) lastIndexOfString'' (stringToFind:string) (startIndex:int) (count:int) (stringToSearchIn:string) = stringToSearchIn.LastIndexOf(stringToFind, startIndex, count, StringComparison.Ordinal)

    // Reports the zero-based index of the last occurrence of a specified string within the current String object. A parameter specifies the type of search to use for the specified string.
    // let (*inline*) lastIndexOfStringWithComparison (stringToFind:string) (comparisonType:StringComparison) (stringToSearchIn:string) = stringToSearchIn.LastIndexOf(stringToFind, comparisonType)
    // Reports the zero-based index of the last occurrence of a specified string within the current String object. The search starts at a specified character position and proceeds backward toward the beginning of the string. A parameter specifies the type of comparison to perform when searching for the specified string.
    // let (*inline*) lastIndexOfStringWithComparison' (stringToFind:string) startIndex (comparisonType:StringComparison) (stringToSearchIn:string) = stringToSearchIn.LastIndexOf(stringToFind, startIndex, comparisonType)
    // Reports the zero-based index position of the last occurrence of a specified string within this instance. The search starts at a specified character position and proceeds backward toward the beginning of the string for the specified number of character positions. A parameter specifies the type of comparison to perform when searching for the specified string.
    // let (*inline*) lastIndexOfStringWithComparison'' (stringToFind:string) startIndex count (comparisonType:StringComparison) (stringToSearchIn:string) = stringToSearchIn.LastIndexOf(stringToFind, startIndex, count, comparisonType)


    // Returns a new string whose textual value is the same as this string, but whose binary representation is in Unicode normalization form C.
    // let (*inline*) normalize (txt:string) = txt.Normalize() // see more advanced implementation at top
    // Returns a new string whose textual value is the same as this string, but whose binary representation is in the specified Unicode normalization form.
    // let (*inline*) normalize' normalizationForm (txt:string) = txt.Normalize(normalizationForm)
    // Indicates whether this string is in Unicode normalization form C.
    // let (*inline*) isNormalized (txt:string) = txt.IsNormalized()
    // Indicates whether this string is in the specified Unicode normalization form.
    // let (*inline*) isNormalized' normalizationForm (txt:string) = txt.IsNormalized(normalizationForm)


    /// Returns a new string that right-aligns the characters in this instance by padding them with spaces on the left, for a specified total length.
    let (*inline*) padLeft totalWidth (txt:string) = txt.PadLeft(totalWidth)

    /// Returns a new string that right-aligns the characters in this instance by padding them on the left with a specified Unicode character, for a specified total length.
    let (*inline*) padLeft' totalWidth paddingChar (txt:string) = txt.PadLeft(totalWidth, paddingChar)

    /// Returns a new string that left-aligns the characters in this string by padding them with spaces on the right, for a specified total length.
    let (*inline*) padRight totalWidth (txt:string) = txt.PadRight(totalWidth)

    /// Returns a new string that left-aligns the characters in this string by padding them on the right with a specified Unicode character, for a specified total length.
    let (*inline*) padRight' totalWidth paddingChar (txt:string) = txt.PadRight(totalWidth, paddingChar)

    /// Returns a new string in which all the characters in the current instance, beginning at a specified position and continuing through the last position, have been deleted.
    let (*inline*) remove startIndex (txt:string) = txt.Remove(startIndex)

    /// Returns a new string in which a specified number of characters in the current instance beginning at a specified position have been deleted.
    let (*inline*) remove' startIndex count (txt:string) = txt.Remove(startIndex, count)

    /// Returns a new string in which all occurrences of a specified Unicode character in this instance are replaced with another specified Unicode character.
    let (*inline*) replace' (oldChar:char) (newChar:char) (txt:string) = txt.Replace(oldChar, newChar) //modified for FsEx: use apostrophe here

    /// Returns a new string in which all occurrences of a specified string in the current instance are replaced with another specified string.
    let (*inline*) replace (oldValue:string) (newValue:string) (txt:string) = txt.Replace(oldValue, newValue) //modified for FsEx: dont use apostrophe here

    /// Splits a string into substrings that are based on the characters in an array.
    let (*inline*) splitChar separator (stringToSplit:string) = stringToSplit.Split(separator)

    /// Splits a string into a maximum number of substrings based on the characters in an array. You also specify the maximum number of substrings to return.
    let (*inline*) splitChar' separator (count:int) (stringToSplit:string) = stringToSplit.Split(separator, count)

    /// Splits a string into substrings based on the characters in an array. You can specify whether the substrings include empty array elements.
    let (*inline*) splitCharWithOptions (separator:char[]) (options:StringSplitOptions) (stringToSplit:string) = stringToSplit.Split(separator, options)

    /// Splits a string into a maximum number of substrings based on the characters in an array.
    let (*inline*) splitCharWithOptions' (separator:char[]) count (options:StringSplitOptions) (stringToSplit:string) = stringToSplit.Split(separator, count, options)

    /// Splits a string into substrings based on the strings in an array. You can specify whether the substrings include empty array elements.
    let (*inline*) splitString (separator:string[]) (options:StringSplitOptions) (stringToSplit:string) = stringToSplit.Split(separator, options)

    /// Splits a string into a maximum number of substrings based on the strings in an array. You can specify whether the substrings include empty array elements.
    let (*inline*) splitString' (separator:string[]) count (options:StringSplitOptions) (stringToSplit:string) = stringToSplit.Split(separator, count, options)

    /// Determines whether the beginning of this string instance matches the specified string, using StringComparison.Ordinal..
    let (*inline*) startsWith (stringToFindAtStart:string) (stringToSearchIn:string) = stringToSearchIn.StartsWith(stringToFindAtStart, StringComparison.Ordinal)

    /// Determines whether the beginning of this string instance matches the specified string when compared using the specified comparison option.
    let (*inline*) startsWith' stringToFindAtStart comparisonType (stringToSearchIn:string) = stringToSearchIn.StartsWith(stringToFindAtStart, comparisonType)

    /// Determines whether the beginning of this string instance matches the specified string when compared using the specified culture.
    let (*inline*) startsWith'' stringToFindAtStart ignoreCase culture (stringToSearchIn:string) = stringToSearchIn.StartsWith(stringToFindAtStart, ignoreCase, culture)

    /// Retrieves a substring from this instance. The substring starts at a specified character position and continues to the end of the string.
    let (*inline*) substring startIndex (txt:string) = txt.Substring(startIndex)

    /// Retrieves a substring from this instance. The substring starts at a specified character position and has a specified length.
    let (*inline*) substring' startIndex length (txt:string) = txt.Substring(startIndex, length)

    /// Copies the characters in this instance to a Unicode character array.
    let (*inline*) toCharArray (txt:string) = txt.ToCharArray()

    /// Copies the characters in a specified substring in this instance to a Unicode character array.
    let (*inline*) toCharArray' startIndex length (txt:string) = txt.ToCharArray(startIndex, length)

    /// Returns a copy of this string converted to lowercase.
    let (*inline*) toLower (txt:string) = txt.ToLower()

    /// Returns a copy of this string converted to lowercase, using the casing rules of the specified culture.
    let (*inline*) toLower' culture (txt:string) = txt.ToLower(culture)

    /// Returns a copy of this String object converted to lowercase using the casing rules of the invariant culture.
    let (*inline*) toLowerInvariant (txt:string) = txt.ToLowerInvariant()

    /// Returns a copy of this string converted to uppercase.
    let (*inline*) toUpper (txt:string) = txt.ToUpper()

    /// Returns a copy of this string converted to uppercase, using the casing rules of the specified culture.
    let (*inline*) toUpper' culture (txt:string) = txt.ToUpper(culture)

    /// Returns a copy of this String object converted to uppercase using the casing rules of the invariant culture.
    let (*inline*) toUpperInvariant (txt:string) = txt.ToUpperInvariant()

    /// Removes all leading and trailing white-space characters from the current String object.
    let (*inline*) trim (txt:string) = txt.Trim()

    /// Removes all leading and trailing occurrences of a set of characters specified in an array from the current String object.
    let (*inline*) trim' (trimChars:char[]) (txt:string) = txt.Trim(trimChars)

    /// Removes all trailing occurrences of a set of characters specified in an array from the current String object.
    let (*inline*) trimEnd (trimChars:char[]) (txt:string) = txt.TrimEnd(trimChars)

    /// Removes all leading occurrences of a set of characters specified in an array from the current String object.
    let (*inline*) trimStart (trimChars:char[]) (txt:string) = txt.TrimStart(trimChars)


