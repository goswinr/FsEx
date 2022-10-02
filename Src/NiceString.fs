namespace FsEx

open System
open System.Globalization

open FsEx.SaveIgnore //so that  |> ignore  can only be used on value types


/// Exposes the settings used in toNiceString pretty printing function
module NiceStringSettings = 
    

    /// |Head of string * seq<Lines>
    /// |Element of string
    /// |EarlyEnd // "this is just the string "..." or similar;  for not fully enumerated sequences
    [<NoComparison>]
    type Lines = 
        |Header  of Head
        |Element of string
        |EarlyEnd  // "this is just the string "..." or similar 

    
    and [<NoComparison>] Head = {
        title : string
        openBracket:string
        items:ResizeArray<int*Lines>
        closingBracket:string
        }
    
    [<NoComparison;NoEquality>]
    type NicePrintSettings = {
        /// Set this to change how deep the content of nested seq is printed (printFull ignores this).
        maxDepth:int

        /// maximum items when printing vertically
        /// how many items per seq are printed (printFull ignores this).        
        maxVertItems:int

        /// maximum total characters when printing horizontally in one line 
        /// how  many items can be in one line before switching to vertical sequence.
        maxHorChars:int 
       
        /// how many characters of a string might be printed at once.
        maxCharsInString :int

        /// print top level numbers with full precision ?
        fullPrecision:bool
        
        }

    /// Default NicePrintSettings:
    /// maxDepth          = 3
    /// maxVertItems      = 6
    /// maxHorChars       = 120  
    /// maxCharsInString  = 2000    
    let defaultNicePrintSettings = {
        maxDepth          = 3
        maxVertItems      = 6
        maxHorChars       = 120
        maxCharsInString  = 2000 
        fullPrecision     = false
        }
    
    /// Allows to inject an optional formatter that gets called before main formatter
    /// This formatter shall return None if the main formatter should be used
    /// use externalFormatter for types defined in other assemblies
    let mutable externalFormatter : obj -> option<Lines>  = fun _ -> None

    /// Set this to change the printing of floats larger than 10'000
    let mutable thousandSeparator = '\'' // = just one quote '
    
    /// If the absolute value of a float is below this, display ±0.0
    /// Default = 1e-24. 
    /// Double.Epsilon = no rounding down
    /// This value can be set for example by hosting apps that have a build in absolute tolerance like Rhino3d
    let mutable userZeroTolerance = 1e-24 // Double.Epsilon

/// For formatting simple types like int, float, very long strings for nice printing
/// Used by NiceString Module
module NiceFormat  =  // used by Rhino.Scripting

    module private Literals = 

        /// string for RhinoMath.UnsetDouble -1.23432101234321e+308
        [<Literal>]
        let RhinoMathUnsetDouble = "RhinoMath.UnsetDouble" // for https://developer.rhino3d.com/api/RhinoCommon/html/F_Rhino_RhinoMath_UnsetValue.htm

        /// string for RhinoMath.UnsetSingle -1.234321e+38f
        [<Literal>]
        let RhinoMathUnsetSingle = "RhinoMath.UnsetSingle" // for https://developer.rhino3d.com/api/RhinoCommon/html/F_Rhino_RhinoMath_UnsetSingle.htm

        [<Literal>]
        let PositiveInfinity = "∞"

        [<Literal>]
        let NegativeInfinity = "-∞"

        [<Literal>]
        let NaN = "NaN"

        [<Literal>]
        let CloseToZeroPositive = "≈+0.0"
    
        [<Literal>]
        let CloseToZeroNegative = "≈-0.0"
    
        [<Literal>]
        let BelowUserZeroTolerance = "~0.0"

    open NiceStringSettings

    //let internal deDE = Globalization.CultureInfo("de-DE")
    let internal invC = Globalization.CultureInfo.InvariantCulture

    /// Assumes a string that represent a float or int with '.' as decimal separator and no other input formatting
    let internal addThousandSeparators (s:string) =         
        let b = Text.StringBuilder(s.Length + s.Length / 3 + 1)
        let inline add (c:char) = b.Append(c) |> ignoreObj

        let inline doBeforeComma st en = 
            for i=st to en-1 do // don't go to last one because it shall never get a separator
                let rest = en-i
                add s.[i]
                if rest % 3 = 0 then add thousandSeparator
            add s.[en] //add last (never with sep)

        let inline doAfterComma st en = 
            add s.[st] //add fist (never with sep)
            for i=st+1 to en do // don't go to last one because it shall never get a separator
                let pos = i-st
                if pos % 3 = 0 then add thousandSeparator
                add s.[i]

        let start = 
            if s.[0] = '-' then  add '-'; 1 // add minus if present and move start location
            else                          0

        match s.IndexOf('.') with
        | -1 -> 
            match s.IndexOf("e",StringComparison.OrdinalIgnoreCase) with 
            | -1 -> doBeforeComma start (s.Length-1)
            | e -> // if float is in scientific notation don't insert comas into it too:
                doBeforeComma start (s.Length-1)
                for ei = e to s.Length-1 do add s.[ei]
        | i ->
            if i>start then 
                doBeforeComma start (i-1)
            add '.'
            if i < s.Length then 
                match s.IndexOf("e",StringComparison.OrdinalIgnoreCase) with
                | -1 -> doAfterComma (i+1) (s.Length-1)
                | e -> // if float is in scientific notation don't insert comas into it too:
                    doAfterComma (i+1) (e-1)
                    for ei = e to s.Length-1 do add s.[ei]

        b.ToString()

    /// NaN -> None
    /// PositiveInfinity -> None
    /// NegativeInfinity-> None
    /// RhinoMathUnsetDouble -> None
    /// RhinoMathUnsetSingle -> None
    /// AlmostZero ->    Some 0.0
    /// AlmostZeroNeg->  Some 0.0
    /// RoundedToZero -> Some 0.0
    let tryParseNiceFloat (s:string)= 
        match s with
        |Literals.NaN -> None
        |Literals.PositiveInfinity -> None
        |Literals.NegativeInfinity-> None
        |Literals.RhinoMathUnsetDouble -> None //Some -1.23432101234321e+308  // for https://developer.rhino3d.com/api/RhinoCommon/html/F_Rhino_RhinoMath_UnsetValue.htm
        |Literals.RhinoMathUnsetSingle -> None //Some -1.234321e+38f // for https://developer.rhino3d.com/api/RhinoCommon/html/F_Rhino_RhinoMath_UnsetSingle.htm
        |Literals.CloseToZeroPositive ->    Some 0.0
        |Literals.CloseToZeroNegative->  Some 0.0
        |Literals.BelowUserZeroTolerance -> Some 0.0
        | _ ->
            let cleanFloat = s.Replace(string(thousandSeparator),"") // no need to take care of decimal comma here. nice string never has one
            match Double.TryParse(cleanFloat, NumberStyles.Float, invC) with
            | true, v -> Some v
            | _ -> None


    let int (x:int) = 
        // because abs(Int32.MinValue)  fails !!
        if x = Int32.MinValue || abs (x) >= 1000 then x.ToString() |> addThousandSeparators
        else                                          x.ToString()

    let int64 (x:int64) = 
        // because abs(Int64.MinValue) fails ??
        if x = Int64.MinValue || abs (x) >= 1000L then x.ToString() |> addThousandSeparators
        else                                           x.ToString()

    /// Formatting with automatic precision
    /// e.g.: 0 digits behind comma if above 1000
    /// if there are more than 15 zeros behind the comma just '~0.0' will be displayed
    /// if the value is smaller than NiceStringSettings.roundToZeroBelow '0.0' will be shown.
    /// this is Double.Epsilon by default
    let float  (x:float) = 
        if   Double.IsNaN x then Literals.NaN
        elif x = Double.NegativeInfinity then Literals.NegativeInfinity
        elif x = Double.PositiveInfinity then Literals.PositiveInfinity
        elif x = -1.23432101234321e+308  then Literals.RhinoMathUnsetDouble // for https://developer.rhino3d.com/api/RhinoCommon/html/F_Rhino_RhinoMath_UnsetValue.htm
        elif x = 0.0 then "0.0" // not "0" as in sprintf "%g"
        else
            let  a = abs x
            if   a <  userZeroTolerance then Literals.BelowUserZeroTolerance // do this check up here, value might be very high
            elif a >= 10000.          then x.ToString("#")|> addThousandSeparators
            elif a >= 1000.           then x.ToString("#")
            elif a >= 100.            then x.ToString("#.#" , invC)
            elif a >= 10.             then x.ToString("0.0#" , invC)
            elif a >= 1.              then x.ToString("0.0##" , invC)
            elif a >= 0.1             then x.ToString("0.####" , invC)
            elif a >= 0.01            then x.ToString("0.#####" , invC)
            elif a >= 0.001           then x.ToString("0.######" , invC)|> addThousandSeparators
            elif a >= 0.000_1         then x.ToString("0.#######" , invC)|> addThousandSeparators
            elif a >= 0.000_01        then x.ToString("0.########" , invC)|> addThousandSeparators
            elif a >= 0.000_001       then x.ToString("0.#########" , invC)|> addThousandSeparators
            elif a >= 0.000_0001      then x.ToString("0.##########" , invC)|> addThousandSeparators
            elif a >= 0.000_000_001   then x.ToString("0.#############" , invC)|> addThousandSeparators            
            elif x >= 0.0             then Literals.CloseToZeroPositive
            else                           Literals.CloseToZeroNegative
    
    /// prints float via x.ToString("R"), then still adds Thousand Separators
    /// The round-trip ("R") format specifier attempts to ensure that a numeric value that is converted to a string is parsed back into the same numeric value.
    let floatFull  (x:float) = 
        if   Double.IsNaN x then Literals.NaN
        elif x = Double.NegativeInfinity then Literals.NegativeInfinity
        elif x = Double.PositiveInfinity then Literals.PositiveInfinity
        elif x = -1.23432101234321e+308 then Literals.RhinoMathUnsetDouble 
        elif x = 0.0 then "0.0" // not "0" as in sprintf "%g"
        else             
            // R for round tripping has poor performance but still prints nicer than G17:
            // https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-numeric-format-strings?redirectedfrom=MSDN#round-trip-format-specifier-r
            // 0.3.ToString("R")   |> printfn "%s" // "0.3""
            // 0.3.ToString("G17") |> printfn "%s" // "0.29999999999999999"
            x.ToString("R", invC) |> addThousandSeparators            


    /// Formatting with automatic precision
    /// e.g.: 0 digits behind comma if above 1000
    let decimal  (x:Decimal) = 
        if x = 0M then "0.0" // not "0" as in sprintf "%g"
        else
            let  a = abs x
            if   a <  decimal userZeroTolerance then Literals.BelowUserZeroTolerance // do this check up here, value might be very high
            elif a >= 10000M       then x.ToString("#")|> addThousandSeparators
            elif a >= 1000M        then x.ToString("#")
            elif a >= 100M         then x.ToString("#.#" , invC)
            elif a >= 10M          then x.ToString("0.0#" , invC)
            elif a >= 1M           then x.ToString("0.0##" , invC)
            elif a >= 0.1M         then x.ToString("0.####" , invC)
            elif a >= 0.01M        then x.ToString("0.#####" , invC) 
            elif a >= 0.001M       then x.ToString("0.######" , invC) |> addThousandSeparators
            elif a >= 0.000_1M     then x.ToString("0.#######" , invC) |> addThousandSeparators
            elif a >= 0.000_01M    then x.ToString("0.########" , invC)|> addThousandSeparators
            elif a >= 0.000_001M   then x.ToString("0.#########" , invC)|> addThousandSeparators
            elif a >= 0.000_0001M  then x.ToString("0.##########" , invC)|> addThousandSeparators            
            elif x >= 0.0M         then Literals.CloseToZeroPositive
            else                        Literals.CloseToZeroNegative
    
    /// prints decimal via x.ToString("R"), then still adds Thousand Separators
    /// The round-trip ("R") format specifier attempts to ensure that a numeric value that is converted to a string is parsed back into the same numeric value.
    let decimalFull  (d:Decimal) = 
        d.ToString("R") |> addThousandSeparators

    /// Formatting with automatic precision
    /// e.g.: 0 digits behind comma if above 1000
    let single (x:float32) = 
        if   Single.IsNaN x then Literals.NaN
        elif x = Single.NegativeInfinity then Literals.PositiveInfinity
        elif x = Single.PositiveInfinity then Literals.NegativeInfinity
        elif x = -1.234321e+38f then Literals.RhinoMathUnsetSingle // for https://developer.rhino3d.com/api/RhinoCommon/html/F_Rhino_RhinoMath_UnsetSingle.htm
        elif x = 0.0f then "0.0" // not "0" as in sprintf "%g"
        else
            let  a = abs x
            if   a <  float32 userZeroTolerance then Literals.BelowUserZeroTolerance // do this check up here, value might be very high
            elif a >= 10000.f       then x.ToString("#")|> addThousandSeparators         
            elif a >= 1000.f       then x.ToString("#")
            elif a >= 100.f        then x.ToString("#.#" , invC)
            elif a >= 10.f         then x.ToString("0.0#" , invC)
            elif a >= 1.f          then x.ToString("0.0##" , invC)
            elif a >= 0.1f         then x.ToString("0.####" , invC)
            elif a >= 0.01f        then x.ToString("0.#####" , invC) 
            elif a >= 0.001f       then x.ToString("0.######" , invC) |> addThousandSeparators
            elif a >= 0.000_1f     then x.ToString("0.#######" , invC) |> addThousandSeparators
            elif a >= 0.000_01f    then x.ToString("0.########" , invC)|> addThousandSeparators            
            elif x >= 0.0f         then Literals.CloseToZeroPositive
            else                        Literals.CloseToZeroNegative
    
    /// prints single via x.ToString("R"), then still adds Thousand Separators
    /// The round-trip ("R") format specifier attempts to ensure that a numeric value that is converted to a string is parsed back into the same numeric value.
    let singleFull (x:float32) = 
        if   Single.IsNaN x then Literals.NaN
        elif x = Single.NegativeInfinity then Literals.PositiveInfinity
        elif x = Single.PositiveInfinity then Literals.NegativeInfinity
        elif x = -1.234321e+38f then Literals.RhinoMathUnsetSingle 
        elif x = 0.0f then "0.0" // not "0" as in sprintf "%g"
        else 
            // R for round tripping has poor performance but still prints nicer than G9:
            // https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-numeric-format-strings?redirectedfrom=MSDN#round-trip-format-specifier-r
            // 0.3.ToString("R")   |> printfn "%s" // "0.3""
            // 0.3.ToString("G17") |> printfn "%s" // "0.29999999999999999"
            x.ToString("R",invC)|> addThousandSeparators 
            


    /// Reduces a string length for display to a maximum Length.
    /// Shows (..) as placeholder for skipped characters if string is longer than maxCharCount.
    /// If maxChars is bigger than 35 the placeholder will include the count of skipped characters: e.g. [< ..99 more chars.. >].
    /// maxCharCount will be set to be minimum 6. 
    /// Returned strings are enclosed in quotation marks.
    /// If input is null it returns <*null string*>
    let stringTruncated maxCharCount (s:string) : string = 
        let maxChar = max 8 maxCharCount
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
            if   maxChar <= 10 then str{ "\"" ;  s.Substring(0, maxChar-2-2) ; "(..)"                           ; "\"" }
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

    /// Joins string into one line. 
    /// Replaces line break with space character.
    /// Skips leading whitespace on each line.
    /// Does not include surrounding quotes.
    /// If string is null returns "<*null string*>"
    let stringInOneLine(s:string) =
        if isNull s then  "<*null string*>"
        else
            let sb = Text.StringBuilder(s.Length)
            let rec loop isBeginning i = 
                if i<s.Length then 
                    match s.[i] with 
                    |'\n' ->  
                        if sb.Length>0 && sb.Chars(sb.Length-1) <> ' ' then 
                            sb.Append(' ') |> ignoreObj // to have at least on space separating new lines
                        loop true  (i+1)
                    |'\r' ->  
                        loop true  (i+1)
                    |' '  ->   
                        if not isBeginning then  
                            sb.Append(' ')  |> ignoreObj
                        loop true  (i+1)
                    | c  ->  
                        sb.Append(c) |> ignoreObj
                        loop false (i+1)
            loop true 0
            sb.ToString()
    
    /// Adds a note about trimmed line count if there are more [< ... and %d more lines.>].
    /// Does not include surrounding quotes.
    /// If string is null returns "<*null string*>" .
    let stringTruncatedToMaxLines (maxLineCount:int) (s:string) :string = 
        let maxLines = max 1 maxLineCount
        if isNull s then  
            "<*null string*>"
        elif s.Length < 2 then  
            s
        else
            let mutable found = if s.[0]= '\n' then 1 else 0
            let mutable i = 0
            let mutable stopPos = 0
            while i >= 0 do
                if i+1=s.Length then // end of string reached with a '\n'
                    i<- -1
                else
                    i <- s.IndexOf('\n', i+1)
                    found <- found + 1
                    if found = maxLines then 
                        stopPos <- i

            if stopPos > 0  && found - maxLines > 1 then // if there is just one more lien print it instead of the note
                str{ 
                    s.Substring(0,stopPos+1)
                    "[< ... and "
                    found - maxLines
                    " more lines.>]"
                    }
            else
                s  

    let date (d:DateTime) = 
        if d.Hour=0 && d.Minute=0 && d.Second = 0 then d.ToString("yyyy-MM-dd") else d.ToString("yyyy-MM-dd HH:mm:ss")
    
    let dateWithOffset (d:DateTimeOffset) = 
        d.ToString("yyyy-MM-dd HH:mm:ss K") 

module internal NiceStringImplementation  =     
    open Microsoft.FSharp.Reflection
    open NiceStringSettings
    open NiceFormat
    open System.Text

    type StringBuilder with 
        member inline this.append(s:string)     = this.Append(s)     |> ignoreObj
        member inline this.appendLine(s:string) = this.AppendLine(s) |> ignoreObj

    // TODO test alternative implementations:
    // https://github.com/eiriktsarpalis/TypeShape/blob/main/samples/TypeShape.Samples/printer.fs
    // https://eiriktsarpalis.wordpress.com/2016/08/05/typeshape-practical-generic-programming-in-f/
    // and use :
    // http://www.fssnip.net/cV/title/A-Generic-PrettyPrinter-for-Record-types

    type SeqCount = Counted of int | More of int

    /// reduces the maxCharsInString by a factor of 10 for each nesting level
    let internal formatStringByDepth (nps:NicePrintSettings) (depth:int) (s:String) =
        match depth with 
        | 0 -> stringTruncated  nps.maxCharsInString     s
        | 1 -> stringTruncated (nps.maxCharsInString/2)  s
        | 2 -> stringTruncated (nps.maxCharsInString/4)  s
        | 3 -> stringTruncated (nps.maxCharsInString/8)  s
        | _ -> stringTruncated (nps.maxCharsInString/16) s
        //| 1 -> stringTruncated (nps.maxCharsInString/10)    s
        //| 2 -> stringTruncated (nps.maxCharsInString/100)   s
        //| 3 -> stringTruncated (nps.maxCharsInString/1000)  s
        //| _ -> stringTruncated (nps.maxCharsInString/10000) s
    
    
    /// return the string before a splitter
    /// if splitter is missing return full string
    let inline private before (splitter:Char) (s:string) = 
        let start = s.IndexOf(splitter)
        if start = -1 then s
        else s.Substring(0, start )

    /// Includes GenericArguments types if available
    /// remove tick at end e.g. List`1  to  Collections.Generic.List
    let rec internal typeName (typ:Type) = 
        let name = typ.Name |> before '`' |> before '@'
        let fullPath = if String.IsNullOrEmpty typ.Namespace then name else typ.Namespace   + "." + name
        let cleaned0 = 
            if fullPath.StartsWith "System." then
                fullPath.Substring(7)            
            else
                fullPath
                    .Replace("Microsoft.FSharp.Collections.mkSeq","seq")
                    .Replace("Microsoft.FSharp.Collections.","")
                    .Replace("Microsoft.FSharp.Core.","")
        
        let cleaned =             
            match cleaned0 with 
            | "Boolean"   -> "bool"  
            | "Byte"      -> "byte"            
            | "Int32"     -> "int"   
            | "UInt32"    -> "uint"  
            | "Int64"     -> "int64" 
            | "UInt64"    -> "uint64"
            | "Double"    -> "float"
            | "Single"    -> "float32"
            | "FSharpRef" -> "ref"
            | _ -> cleaned0
        
        let param = 
            match typ.GetGenericArguments() with
            | null   -> ""
            | [| |]  -> ""
            | ts     -> "<" + (ts |> Array.map typeName |> String.concat "," ) + ">" // recursive call
        cleaned+param

    let getNiceOrDefault(typ:Type, x:obj) : Lines =
        let nStr = typ.GetProperty("ToNiceString")
        if notNull nStr then 
            match nStr.GetValue(x) with 
            |null -> sprintf "%A" x 
            | :? string as s -> s   
            | _ -> sprintf "%A" x   
        else                 
            sprintf "%A" x         
        |> Element
    

    /// returns if the returned list is trimmed to maxCount,  input has more elements then maxCount
    let rec getItemsInSeq (titleLength, nps:NicePrintSettings, depth, xs:Collections.IEnumerable) : ResizeArray<int*Lines>=  // non generic IEnumerable
        let rs = ResizeArray<int*Lines>()
        let enum = xs.GetEnumerator()
        let mutable chars = titleLength 
        let mutable i=0
        // Important: enum.MoveNext() needs to be evaluated last 
        while  (i < nps.maxVertItems-1 || chars < nps.maxHorChars) && enum.MoveNext() do // don't iter full sequence if only the first few items are printed
            let ln = getLines nps depth (box enum.Current)
            match ln with
            |Element s  -> chars <- chars + s.Length + 2 // +2 for separator "; "
            |EarlyEnd   -> chars <- chars + 4
            |Header _   -> chars <- max 99_999 chars //  don't do nested seq in one line !, avoid overflow by using max
            rs.Add(i, ln)            
            i <- i+1
        if enum.MoveNext() then // check if there is one more           
            let x = box enum.Current
            if enum.MoveNext() then // check if there are two more  
                rs.Add(-1, EarlyEnd)
            else // there is just one more. add it instead of EarlyEnd
                let ln = getLines nps depth x                
                rs.Add(i, ln) 
        rs


    and getIList (nps:NicePrintSettings) depth (x:obj) (xs:Collections.IList ) :Lines = // non generic ICollection
        // count is always available        
        let typ = x.GetType()
        let isArray =  typ.Name.EndsWith "[]"
        let name = typeName (typ) + if isArray then "[]" else ""
        let desc = 
            if xs.Count = 0 then sprintf "empty %s "  name
            else                 sprintf "%s with %d items"  name xs.Count
        if depth = nps.maxDepth then 
            Element desc 
        else 
            let lines = getItemsInSeq (desc.Length, nps, depth+1, xs)
            
            if xs.Count > nps.maxVertItems then   // add last element 
                lines.Add(xs.Count-1, getLines nps (depth+1) xs.LastObj)
            // in special case where xs.Count=nps.maxVertItems+1 replace the early end with the actual item
            if xs.Count=nps.maxVertItems+1 then 
                lines.[lines.Count-2] <- (xs.Count-2, getLines nps (depth+1) xs.[xs.Count-2])
            
            
            if isArray then Header {title=desc; openBracket="[|"; items=lines; closingBracket="|]" }
            else            Header {title=desc; openBracket="[" ; items=lines; closingBracket= "]" }


    and getSeq (nps:NicePrintSettings) depth (x:obj) (xs:System.Collections.IEnumerable) :Lines = // non generic IEnumerable
        // count may not be available , sequences are not only iterated till NiceStringSettings.maxItemsPerSeq
        let name =   typeName(x.GetType())
        let ls = getItemsInSeq (name.Length + 16, nps, depth+1, xs) // +16 for "00 with 00 items"
        let desc = 
            if ls.Count = 0 then             sprintf "empty %s " name
            elif snd ls.Last = EarlyEnd then sprintf "%s with more than %d items" name  (ls.Count)
            else                             sprintf "%s with %d items" name  ls.Count
        if depth = nps.maxDepth then  Element desc 
        else                          Header {title=desc; openBracket="[" ; items=ls; closingBracket="]" }
        

    
    /// tries to put the record in one line
    and getRecord (nps:NicePrintSettings) depth (x:obj) (typ:Type):Lines=
        let props = FSharp.Reflection.FSharpType.GetRecordFields(typ)
        let title =  typeName (typ)        
        let ls = ResizeArray<int*Lines>()
        for p in props do 
            let n = p.Name
            let vo = p.GetValue(x)
            match getLines (nps:NicePrintSettings) (depth+1) vo with 
            |Element s -> ls.Add(-5 ,Element ( n+"="+s))                        // use a negative number on index to skip numbering of items 
            |EarlyEnd ->  ls.Add(-5 ,EarlyEnd )                                 // use a negative number on index to skip numbering of items 
            |Header h ->  ls.Add(-5 ,Header {h with title = n+"="+h.title})    // use a negative number on index to skip numbering of items     
        if depth = nps.maxDepth then  Element title 
        else                          Header {title=title; openBracket="{"; items=ls; closingBracket="}"}

    

    ///  x is boxed already
    and getLines  (nps:NicePrintSettings) (depth:int) (x:obj) : Lines = 
        match NiceStringSettings.externalFormatter x with // first check if externalFormatter provides a string , this is used e.g. for types from RhinoCommon.dll
        | Some ln -> ln
        | None -> 
            match x with // boxed already
            | null -> "'null' (or Option.None)"                                                           |> Element
            | :? int     as i -> i |> NiceFormat.int                                                      |> Element
            | :? int64   as i -> i |> NiceFormat.int64                                                    |> Element
            | :? float   as v -> (if depth=0 && nps.fullPrecision then v |> NiceFormat.floatFull   else  v |> NiceFormat.float  ) |> Element
            | :? single  as v -> (if depth=0 && nps.fullPrecision then v |> NiceFormat.singleFull  else  v |> NiceFormat.single ) |> Element
            | :? decimal as d -> (if depth=0 && nps.fullPrecision then d |> NiceFormat.decimalFull else  d |> NiceFormat.decimal) |> Element
            | :? Ref<int>   as r ->  "ref " + NiceFormat.int   r.Value                                    |> Element
            | :? Ref<int64> as r ->  "ref " + NiceFormat.int64 r.Value + "L"                              |> Element
            | :? Ref<float> as r ->  "ref " + NiceFormat.float r.Value                                    |> Element
            | :? Char    as c -> c.ToString()                                                             |> Element // "'" + c.ToString() + "'" // or add quotes?
            | :? string  as s -> formatStringByDepth nps depth s                                          |> Element                                  
            | :? Guid    as g -> sprintf "Guid[%O]" g                                                     |> Element
            | :? DateTime       as d -> date d                                                            |> Element
            | :? DateTimeOffset as d -> dateWithOffset d                                                  |> Element
            | :? Collections.IList       as xs -> getIList nps depth x xs
            | :? Collections.IEnumerable as xs -> getSeq   nps depth x xs
            | _ ->
                let typ = x.GetType()
                if FSharpType.IsRecord(typ)then 
                    getRecord nps depth x typ  
                
                elif depth = 0 then
                    if FSharpType.IsTuple(typ) then     // just to trim brackets off
                        let tyStr = typeName typ
                        let t = sprintf "%A" x
                        let formatA =  t.[1 .. t.Length-2] // trim enclosing in brackets. good idea ?                        
                        Element (sprintf "%s: %s" tyStr formatA)
                        //let fields = FSharpValue.GetTupleFields(x)
                        //let desc =  sprintf "%s of %d items" tyStr fields.Length
                        //if depth = maxDepth then Element desc else Head (desc, getItemsInSeq (depth+1) fields)

                    elif FSharpType.IsUnion(typ) then // just to include declaring type
                        let union, fields =  FSharpValue.GetUnionFields(x, typ)
                        let declTy = union.DeclaringType
                        let mainType = typeName  declTy
                        Element (sprintf "%s.%A" mainType x )
                        //let desc =  sprintf "a %s of case %s" mainType union.Name
                        //if depth = maxDepth then Element desc else Head (desc, getItemsInSeq (depth+1) fields) // TODO test what are the fields

                    else
                        getNiceOrDefault(typ,x)
                else
                    if FSharpType.IsTuple(typ) then
                        let t = sprintf "%A" x
                        Element t.[1 .. t.Length-2] // trim enclosing in brackets. good idea ?                     
                    else
                        getNiceOrDefault(typ,x)


    (*
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.DerivedPatterns
    open Microsoft.FSharp.Quotations.Patterns

    let (|UC|_|) e o = 
        match e with /// https://stackoverflow.com/questions/3151099/is-there-a-way-in-f-to-type-test-against-a-generic-type-without-specifying-the
        | Lambdas(_,NewUnionCase(uc,_)) | NewUnionCase(uc,[]) ->
            if isNull (box o) then
            // Need special case logic in case null is a valid value (e.g. Option.None)
            let attrs = uc.DeclaringType.GetCustomAttributes(typeof<CompilationRepresentationAttribute>, false)
            if attrs.Length = 1
                && (attrs.[0] :?> CompilationRepresentationAttribute).Flags &&& CompilationRepresentationFlags.UseNullAsTrueValue <> enum 0
                && uc.GetFields().Length = 0
            then Some []
            else None
            else
            let t = o.GetType()
            if FSharpType.IsUnion t then
                let uc2, fields = FSharpValue.GetUnionFields(o,t)
                let getGenType (t:System.Type) = if t.IsGenericType then t.GetGenericTypeDefinition() else t
                if uc2.Tag = uc.Tag && getGenType (uc2.DeclaringType) = getGenType (uc.DeclaringType) then
                Some(fields |> List.ofArray)
                else None
            else None
        | _ -> failwith "toNiceStringRec: the UC pattern can only be used against simple union cases"

    *)

    
    let appendIfFitInOneLine (sb:StringBuilder, nps:NicePrintSettings, h:Head) = 
        let els = ResizeArray<string>()
        let mutable earlyEnd = false
        let mutable headsToo = false
        for _,x in h.items do 
            match x with 
            | Element s -> els.Add s
            | EarlyEnd  -> earlyEnd <- true 
            | Header _  -> headsToo <- true
        if headsToo then 
            false
        else
            let lenOfString = (els.Count-1)*2 + (els |> Seq.sumBy (fun s-> s.Length)) + if headsToo then 4 else 0
            if lenOfString > nps.maxHorChars then 
                false
            else
                sb.append(" = "+h.openBracket+" ")  
                for i,s in els |> Seq.indexed  do 
                    sb.append(s)
                    if i < els.Count-1 then sb.append("; ")  
                if earlyEnd then sb.append("...")
                sb.appendLine(" "+h.closingBracket)  
                true 
        
    // TODO implement this formatter with colorful lines for Seff:
    
    let formatLines  (nps:NicePrintSettings) (lines:Lines) : string = 
        let indent = 4 // indent size
        let colonAfterNumber = ": "
        let sb = Text.StringBuilder()
        let inline prefix(pos,depth)= // add indent with number
            if depth > 0 then
                if pos >=0 then  
                    sb.append(pos.ToString().PadLeft(indent * depth,' ')) 
                    sb.append(colonAfterNumber) 
                else // to be able to skip pos number on "..." or for records
                    sb.append(String(' ', indent * depth - 1 ))


        let rec loop (numbering:int) depth (lns:Lines) = 
            if depth <= nps.maxDepth then                
                match lns with
                |Element s    -> prefix(numbering,depth); sb.appendLine(s)       
                |EarlyEnd     -> prefix(-1       ,depth); sb.appendLine("...")   
                |Header h  ->
                    prefix(numbering,depth)
                    // header:
                    sb.append(h.title) 
                    // items:
                    if h.items.Count=0 then
                        sb.appendLine("")   // don't add colon, no items follow
                    elif appendIfFitInOneLine(sb,nps, h) then
                        ()
                    else // list items vertically
                        sb.appendLine(" = "+h.openBracket)    // only add colon only if items follow
                        for i,x in h.items do
                            loop i (depth+1) x
                        // ad indent and closing bracket
                        sb.append(String(' ', indent * (depth+1)-1 ))
                        sb.appendLine(h.closingBracket)    
        loop 0 0 lines

        //finally trim of line return at end
        let len = sb.Length
        if sb.Chars(len-2) = '\r' then sb.Remove(len-2 , 2) |> ignoreObj
        let len = sb.Length
        if sb.Chars(len-1) = '\n' then sb.Remove(len-1 , 1) |> ignoreObj //in case there is no '\r'
        sb.ToString()


module NiceString  = 
    open NiceStringImplementation
    open NiceStringSettings

    //TODO sync the docstring here with print and printFull in module Print and the shadowing one in Rhino.Scripting

    /// Nice formatting for numbers including thousand Separator, records and (nested) sequences, 
    /// NicePrintSettings:
    /// maxDepth          = 3     , how deep the content of nested seq is printed (printFull ignores this)
    /// maxVertItems      = 6     , how many items per seq are printed in vertical list(printFull ignores this)
    /// maxHorChars       = 120   , how  many chars can be in one line before switching to vertical sequencing of items in collection.
    /// maxCharsInString  = 2000  , how many characters of a string might be printed at once.
    /// Settings are exposed in FsEx.NiceString.NiceStringSettings:
    /// .thousandSeparator       = '     ; set this to change the printing of floats and integers larger than 10'000
    let toNiceString (x:'T) :string = 
        let nps = NiceStringSettings.defaultNicePrintSettings
        x |> box |> getLines nps 0 |> formatLines nps


    /// Nice formatting for numbers including thousand Separator, records and (nested) sequences, 
    /// NicePrintSettings:
    /// maxDepth          = 3     , how deep the content of nested seq is printed (printFull ignores this)
    /// maxVertItems      = 50    , how many items per seq are printed in vertical list(printFull ignores this)
    /// maxHorChars       = 120   , how  many chars can be in one line before switching to vertical sequencing of items in collection.
    /// maxCharsInString  = 2000  , how many characters of a string might be printed at once.
    /// Settings that are exposed in FsEx.NiceString.NiceStringSettings:
    /// .thousandSeparator = '     , set this to change the printing of floats and integers larger than 10'000
    /// .roundToZeroBelow  = 1e-24 , if the absolute value of a float is below this, display ±0.0
    let toNiceStringLong (x:'T) :string = 
        let nps = {NiceStringSettings.defaultNicePrintSettings with maxVertItems = 50 }
        x |> box |> getLines nps 0 |> formatLines nps


    /// Nice formatting for numbers including thousand Separator, records and (nested) sequences, 
    /// NicePrintSettings:
    /// maxDepth          = 24        , how deep the content of nested seq is printed (printFull ignores this)
    /// maxVertItems      = 100'000   , how many items per seq are printed in vertical list(printFull ignores this)
    /// maxHorChars       = 240       , how  many chars can be in one line before switching to vertical sequencing of items in collection.
    /// maxCharsInString  = 1'000'000 , how many characters of a string might be printed at once.
    /// Settings that are exposed in FsEx.NiceString.NiceStringSettings:
    /// .thousandSeparator = '     , set this to change the printing of floats and integers larger than 10'000
    /// .roundToZeroBelow  = 1e-24 , if the absolute value of a float is below this, display ±0.0
    let toNiceStringFull (x:'T) :string = 
        let nps = 
            {
            maxDepth          = 24
            maxVertItems      = 100_000
            maxHorChars       = 240
            maxCharsInString  = 1_000_000 
            fullPrecision     = true
            }
        x |> box |> getLines nps 0 |> formatLines nps


    /// Nice formatting for numbers including thousand Separator, records and (nested) sequences, 
    /// Provide custom NicePrintSettings:
    /// maxDepth          : how deep the content of nested seq is printed (printFull ignores this)
    /// maxVertItems      : how many items per seq are printed in vertical list(printFull ignores this)
    /// maxHorChars       : how  many chars can be in one line before switching to vertical sequencing of items in collection.
    /// maxCharsInString  : how many characters of a string might be printed at once.
    /// Settings that are exposed in FsEx.NiceString.NiceStringSettings:
    /// .thousandSeparator = '     , set this to change the printing of floats and integers larger than 10'000
    /// .roundToZeroBelow  = 1e-24 , if the absolute value of a float is below this, display ±0.0
    let toNiceStringCustom (nicePrintSettings:NicePrintSettings) (x:'T) :string =         
        x |> box |> getLines nicePrintSettings 0 |> formatLines nicePrintSettings

 