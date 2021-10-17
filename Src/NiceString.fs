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
        |Head of string * ResizeArray<Lines>
        |Element of string
        |EarlyEnd // "this is just the string "..." or similar

    /// Set this to change the printing of floats larger than 10'000
    let mutable thousandSeparator = '\'' // = just one quote '

    /// Set this to change how deep the content of nested seq is printed (printFull ignores this).
    /// default = 3
    let mutable maxNestingDepth = 3

    /// Set this to change how how many items per seq are printed (printFull ignores this).
    /// default = 6
    let mutable maxItemsPerSeq = 6

    /// Set this to change how how many items can be in one line before switching to vertical sequence.
    /// default = 80
    let mutable maxCharsPerLine = 80

    /// Set this to change how many characters of a string might be printed at onece.
    /// default = 2000
    let mutable maxCharsInString = 2000

    /// If the absolut value of a float is below this, display ±0.0
    /// default = Double.Epsilon = no rounding down
    /// This value can be set for exmaple by hosting apps that have a build in absolute tolerance like Rhion3d
    let mutable roundToZeroBelow = Double.Epsilon

    /// Allows to inject an optional formater that gets called befor main formater
    /// This formater shall return None if the main formater should be used
    /// use externalFormater for types defined in other assemblies
    let mutable externalFormater : obj -> option<Lines> = 
        fun _ -> None

    [<Struct>]
    type NicePrintSettings = {
        /// maximum nesting depth
        maxDepth:int
        /// maximum items when printing vertically
        maxVertItems:int
        /// maximum total characters when printing horizontally
        maxHorChars:int
        }

/// For formating simple types like int, float, very long strings for nice printing
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
        let AlmostZero = "≈0.0"

        [<Literal>]
        let AlmostZeroNeg = "-≈0.0"

        [<Literal>]
        let RoundedToZero = "~0.0"

    open NiceStringSettings

    //let internal deDE = Globalization.CultureInfo("de-DE")
    let private invC = Globalization.CultureInfo.InvariantCulture

    /// Assumes a string that represent a float or int with '.' as decimal serapator and no other input formating
    let private addThousandSeparators (s:string) = 
        let b = Text.StringBuilder(s.Length + s.Length / 3 + 1)
        let inline add (c:char) = b.Append(c) |> ignoreObj

        let inline doBeforeComma st en = 
            for i=st to en-1 do // dont go to last one becaus it shal never get a separator
                let rest = en-i
                add s.[i]
                if rest % 3 = 0 then add thousandSeparator
            add s.[en] //add last (never with sep)

        let inline doAfterComma st en = 
            add s.[st] //add fist (never with sep)
            for i=st+1 to en do // dont go to last one becaus it shal never get a separator
                let pos = i-st
                if pos % 3 = 0 then add thousandSeparator
                add s.[i]

        let start = 
            if s.[0] = '-' then  add '-'; 1 /// add minus if present and move start location
            else                          0

        match s.IndexOf('.') with
        | -1 -> doBeforeComma start (s.Length-1)
        | i ->
            if i>start then doBeforeComma start (i-1)
            add '.'
            if i < s.Length then doAfterComma (i+1) (s.Length-1)

        b.ToString()

    /// NaN -> None
    /// PositiveInfinity -> None
    /// NegativeInfinity-> None
    /// RhinoMathUnsetDouble -> None
    /// RhinoMathUnsetSingle -> None
    /// AlmostZero ->    Some 0.0
    /// AlmostZeroNeg->  Some 0.0
    /// RoundedToZero -> Some 0.0
    let private tryParseNiceFloat (s:string)= 
        match s with
        |Literals.NaN -> None
        |Literals.PositiveInfinity -> None
        |Literals.NegativeInfinity-> None
        |Literals.RhinoMathUnsetDouble -> None //Some -1.23432101234321e+308  // for https://developer.rhino3d.com/api/RhinoCommon/html/F_Rhino_RhinoMath_UnsetValue.htm
        |Literals.RhinoMathUnsetSingle -> None //Some -1.234321e+38f // for https://developer.rhino3d.com/api/RhinoCommon/html/F_Rhino_RhinoMath_UnsetSingle.htm
        |Literals.AlmostZero ->    Some 0.0
        |Literals.AlmostZeroNeg->  Some 0.0
        |Literals.RoundedToZero -> Some 0.0
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

    /// Formating with automatic precision
    /// e.g.: 0 digits behind comma if above 1000
    /// if there are more than 15 zeros behind the comma just '~0.0' will be displayed
    /// if the value is smaller than NiceStringSettings.roundToZeroBelow '0.0' will be shown.
    /// this is Double.Epsilon by default
    let float  (x:float) = 
        if   Double.IsNaN x then Literals.NaN
        elif x = Double.NegativeInfinity then Literals.NegativeInfinity
        elif x = Double.PositiveInfinity then Literals.PositiveInfinity
        elif x = -1.23432101234321e+308 then Literals.RhinoMathUnsetDouble // for https://developer.rhino3d.com/api/RhinoCommon/html/F_Rhino_RhinoMath_UnsetValue.htm
        elif x = 0.0 then "0.0" // not "0" as in sprintf "%g"
        else
            let  a = abs x
            if   a >= 10000.     then x.ToString("#")|> addThousandSeparators
            elif a <  roundToZeroBelow then Literals.RoundedToZero
            elif a >= 1000.      then x.ToString("#")
            elif a >= 100.       then x.ToString("#.#" , invC)
            elif a >= 10.        then x.ToString("#.##" , invC)
            elif a >= 1.         then x.ToString("#.###" , invC)
            elif a >= 0.1        then x.ToString("0.####" , invC)|> addThousandSeparators
            elif a >= 0.01       then x.ToString("0.#####" , invC)|> addThousandSeparators
            elif a >= 0.001      then x.ToString("0.######" , invC)|> addThousandSeparators
            elif a >= 0.0001     then x.ToString("0.#######" , invC)|> addThousandSeparators
            elif a >= 0.00001    then x.ToString("0.########" , invC)|> addThousandSeparators
            elif a >= 0.000001   then x.ToString("0.#########" , invC)|> addThousandSeparators
            elif a >= 0.0000001  then x.ToString("0.##########" , invC)|> addThousandSeparators
            elif a >= 0.000000000000001 then x.ToString("0.###############" , invC)|> addThousandSeparators // 15 decimal paces for doubles
            elif x >= 0.0 then "~0.0"
            else "~-0.0"


    /// Formating with automatic precision
    /// e.g.: 0 digits behind comma if above 1000
    let decimal  (x:Decimal) = 
        if x = 0M then "0.0" // not "0" as in sprintf "%g"
        else
            let  a = abs x
            if   a >= 10000M then x.ToString("#")|> addThousandSeparators
            elif a <  decimal(roundToZeroBelow) then Literals.RoundedToZero
            elif a >= 1000M   then x.ToString("#")
            elif a >= 100M    then x.ToString("#.#" , invC)
            elif a >= 10M     then x.ToString("#.##" , invC)
            elif a >= 1M      then x.ToString("#.###" , invC)
            elif a >= 0.1M    then x.ToString("0.####" , invC)
            elif a >= 0.01M   then x.ToString("0.#####" , invC)
            elif a >= 0.001M  then x.ToString("0.######" , invC)
            elif a >= 0.0001M then x.ToString("0.#######" , invC)
            else                   x.ToString("0.########" , invC)

    /// Formating with automatic precision
    /// e.g.: 0 digits behind comma if above 1000
    let single (x:float32) = 
        if   Single.IsNaN x then Literals.NaN
        elif x = Single.NegativeInfinity then Literals.PositiveInfinity
        elif x = Single.PositiveInfinity then Literals.NegativeInfinity
        elif x = -1.234321e+38f then Literals.RhinoMathUnsetSingle // for https://developer.rhino3d.com/api/RhinoCommon/html/F_Rhino_RhinoMath_UnsetSingle.htm
        elif x = 0.0f then "0.0" // not "0" as in sprintf "%g"
        else
            let  a = abs x
            if   a >= 10000.f then x.ToString("#")|> addThousandSeparators
            elif a <  float32(roundToZeroBelow) then Literals.RoundedToZero
            elif a >= 1000.f  then x.ToString("#")
            elif a >= 100.f   then x.ToString("#.#" , invC)
            elif a >= 10.f    then x.ToString("#.##" , invC)
            elif a >= 1.f     then x.ToString("0.###" , invC)
            elif a >= 0.1f    then x.ToString("0.####" , invC)
            elif a >= 0.01f   then x.ToString("0.#####" , invC)
            elif a >= 0.001f  then x.ToString("0.######" , invC)
            else                   x.ToString("0.#######" , invC) // 7 decimal paces for singles


    /// If the input string is longer than maxChars + 20 then
    /// it returns the input string trimmed to maxChars, a count of skiped characters and the last 6 characters (all enclosed in double quotes ")
    /// e.g. "abcde[..20 more Chars..]xyz"
    /// Else, if the input string is less than maxChars + 20, it is still returned in full (enclosed in double quotes ").
    /// also see String.truncatedFormated
    let truncateString (stringToTrim:string) = 
        if isNull stringToTrim then "-null string-" // add too, just in case this gets called externally
        elif stringToTrim.Length <= NiceStringSettings.maxCharsInString + 20 then sprintf "\"%s\""stringToTrim
        else
            let len   = stringToTrim.Length
            let st    = stringToTrim.Substring(0, maxCharsInString)
            let last20 = stringToTrim.Substring(len-21)
            sprintf "\"%s[<< ...... %d more chars ...... >>]%s\"" st (len - maxCharsInString - 20) last20


    /// return the string befor a splitter
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
        let cleaned = 
            if fullPath.StartsWith "System." then
                fullPath.Substring(7)
            else
                fullPath
                    .Replace("Microsoft.FSharp.Collections.mkSeq","seq")
                    .Replace("Microsoft.FSharp.Collections.","")
                    .Replace("Microsoft.FSharp.Core.","")
        let param = 
            match typ.GetGenericArguments() with
            | null   -> ""
            | [| |]  -> ""
            | ts     -> "<" + (ts |> Array.map typeName |> String.concat "," ) + ">" // recursive call
        cleaned+param

module internal NiceStringImplementation  = 
    open System.Collections.Generic
    open Microsoft.FSharp.Reflection
    open NiceStringSettings

    // TODO test alternative implementations:
    // https://github.com/eiriktsarpalis/TypeShape/blob/main/samples/TypeShape.Samples/printer.fs
    // https://eiriktsarpalis.wordpress.com/2016/08/05/typeshape-practical-generic-programming-in-f/
    // and use :
    // http://www.fssnip.net/cV/title/A-Generic-PrettyPrinter-for-Record-types

    type SeqCount = Counted of int | More of int

    /// retruns if the returned list is trimmed to maxCount,  input has more elements then maxCount
    let rec getItemsInSeq (nsl:NicePrintSettings) depth (xs:Collections.IEnumerable) : ResizeArray<Lines>=  // non generic IEnumerable
        let mutable reachedEnd = false
        let mutable k = 0
        let rs = ResizeArray<Lines>()
        let enum = xs.GetEnumerator()
        let mutable  chars = 15 // stat higher to be sure it will be rendered in one line
        while enum.MoveNext() &&  (k < nsl.maxVertItems || chars < nsl.maxHorChars) do
            let lns = getLines nsl depth (box enum.Current)
            match lns with
            |Element s -> chars <- chars + s.Length
            |EarlyEnd  -> chars <- chars + 4
            |Head _    -> chars <- 9999 // to stop this line if k is alreadey more than nsl.maxVertItems via ( || chars < nsl.maxHorChars). dont do nested seq in one line !
            rs.Add(lns)
            k <- k+1
        if enum.MoveNext() then // don@iteret full seqence if only the first few items are printed
            rs.Add( EarlyEnd)
        rs


    and getCollection (nsl:NicePrintSettings) depth (x:obj) (xs:Collections.ICollection) :Lines = // non generic ICollection
        // count is always available
        let typ = xs.GetType()
        let name =  NiceFormat.typeName typ
        let desc = 
            if xs.Count = 0 then sprintf "empty %s "  name
            else                 sprintf "%s with %d items"  name xs.Count
        if depth = nsl.maxDepth then Element desc else Head (desc, getItemsInSeq nsl (depth+1) xs )


    and getSeq (nsl:NicePrintSettings) depth (x:obj) (xs:Collections.IEnumerable) :Lines = // non generic IEnumerable
        // count may not be available , sequences are not only iterated till NiceStringSettings.maxItemsPerSeq
        let typ = xs.GetType()
        let name =   NiceFormat.typeName   typ
        let seqTyO = typ.GetInterfaces()|> Array.tryFind( fun x -> x.IsGenericType && x.GetGenericTypeDefinition() = typedefof<IEnumerable<_>> )//  generic IEnumerable
        let ls = getItemsInSeq nsl (depth+1) xs
        let desc = 
            if ls.Count = 0 then                 sprintf "empty %s " name
            elif ls.[ls.Count-1] = EarlyEnd then sprintf "%s with more than %d items" name  (ls.Count-1)
            else                                 sprintf "%s with %d items" name  ls.Count
        if depth = nsl.maxDepth then Element desc else Head (desc, ls )


    ///  x is boxed already
    and getLines  (nsl:NicePrintSettings) (depth:int) (x:obj) : Lines = 
        match externalFormater x with // first check if externalFormater provides a string , this is used e.g. for types from RhinoCommon.dll
        | Some ln -> ln
        | None ->
            match x with // boxed already
            | null -> "'null' (or Option.None)"               |> Element
            | :? int        as i   -> i |> NiceFormat.int     |> Element
            | :? float      as v   -> v |> NiceFormat.float   |> Element
            | :? single     as v   -> v |> NiceFormat.single  |> Element
            | :? decimal    as d   -> d |> NiceFormat.decimal |> Element
            | :? Char       as c   -> c.ToString()            |> Element // "'" + c.ToString() + "'" // or add qotes?
            | :? string     as s   -> if depth=0 && nsl.maxVertItems = Int32.MaxValue then s |> Element // dont truncate string if toplevel and printFull
                                      else  NiceFormat.truncateString s  |> Element
            | :? Guid       as g   -> sprintf "Guid[%O]" g    |> Element
            | :? Collections.ICollection as xs -> getCollection nsl depth x xs
            | :? Collections.IEnumerable as xs -> getSeq nsl depth x xs
            | _ ->
                let typ = x.GetType()
                if depth = 0 then
                    if FSharpType.IsTuple(typ) then     // just to trim brackerts off
                        let tyStr = NiceFormat.typeName typ
                        let formatA = (sprintf "%A" x) .[1.. ^1] // trim enclosing in brackets ?
                        Element (sprintf "%s: %s" tyStr formatA)
                        //let fields = FSharpValue.GetTupleFields(x)
                        //let desc =  sprintf "%s of %d items" tyStr fields.Length
                        //if depth = maxDepth then Element desc else Head (desc, getItemsInSeq (depth+1) fields)

                    elif FSharpType.IsUnion(typ) then // just to include declaring type
                        let union, fields =  FSharpValue.GetUnionFields(x, typ)
                        let declTy = union.DeclaringType
                        let mainType = NiceFormat.typeName  declTy
                        Element (sprintf "%s.%A" mainType x )
                        //let desc =  sprintf "a %s of case %s" mainType union.Name
                        //if depth = maxDepth then Element desc else Head (desc, getItemsInSeq (depth+1) fields) // TODO test what are the fields

                    else
                        sprintf "%A" x |> Element
                else
                    if FSharpType.IsTuple(typ) then
                        Element (sprintf "%A" x) .[1.. ^1] // trim enclosing in brackets ?
                    else
                        sprintf "%A" x |> Element


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

    let appendIfFitInOneLine (sb:Text.StringBuilder, nsl:NicePrintSettings, xs:ResizeArray<Lines>) = 
        let onlyElements = xs |> Seq.forall (function |Element _ | EarlyEnd -> true |Head _ -> false) // no sub lists
        if onlyElements then
            // we only count the raw length  without head or separators
            let len = Seq.fold (fun k e -> match e with | Element s -> k + s.Length | EarlyEnd -> k + 4 |Head _ -> eprintfn "**no Heads here ever3**";k ) 0 xs
            if len < nsl.maxHorChars  then // less than 120 chars joint
                sb.Append(": [ ")  |> ignoreObj
                for i=0 to xs.Count - 2 do
                    match xs.[i] with
                    |Element s -> sb.Append(s).Append("; ")  |> ignoreObj
                    |EarlyEnd -> eprintfn "**EarlyEnd second last?**"
                    |Head _   -> eprintfn "**no Heads here1**"
                match xs.[xs.Count - 1] with
                |Element s -> sb.Append(s).AppendLine(" ]")  |> ignoreObj
                |EarlyEnd -> sb.AppendLine("... ]")  |> ignoreObj
                |Head _   -> eprintfn "**no Heads here2**"
                true
            else
                false
        else
            false

    let formatLines  (nsl:NicePrintSettings) (lines:Lines) : string = 
        let inde = 4 // indent size
        let colonAfterNumber = ": "
        let sb = Text.StringBuilder()
        let rec loop (pos:int) depth (lns:Lines) = 
            if depth <= nsl.maxDepth then
                if depth > 0 then // add indent with number
                    sb.Append(pos.ToString().PadLeft(inde*depth,' ')).Append(colonAfterNumber) |> ignoreObj

                match lns with
                |Element s    -> sb.AppendLine(s)       |> ignoreObj
                |EarlyEnd     -> sb.AppendLine("...")   |> ignoreObj
                |Head (h,xs)  ->
                    // header:
                    sb.Append(h) |> ignoreObj
                    // items:
                    if xs.Count=0 then
                        sb.AppendLine()  |> ignoreObj // dont add colon, no items follow
                    elif appendIfFitInOneLine(sb,nsl, xs) then
                        ()
                    else // list items vertically
                        sb.AppendLine(":")  |> ignoreObj  // only add colon if items follow
                        for i,x in Seq.indexed xs do
                            loop i (depth+1) x
        loop 0 0 lines

        //finally trimm of line return at end
        let len = sb.Length
        if sb.Chars(len-2) = '\r' then sb.Remove(len-2 , 2) |> ignoreObj
        let len = sb.Length
        if sb.Chars(len-1) = '\n' then sb.Remove(len-1 , 1) |> ignoreObj //in case ther is no '\r'
        sb.ToString()


module NiceString  = 
    open NiceStringImplementation
    open NiceStringSettings

    //TODO sync the docstring here with print and printfull in module Print and the shadowing one in Rhino.Scripting

    /// Nice formating for numbers including thousand Separator and (nested) sequences, first six items are printed out.
    /// Settings are exposed in FsEx.NiceString.NiceStringSettings:
    /// • thousandSeparator       = '     ; set this to change the printing of floats and integers larger than 10'000
    /// • maxNestingDepth         = 3     ; set this to change how deep the content of nested seq is printed (printFull ignores this)
    /// • maxItemsPerSeq          = 6     ; set this to change how how many items per seq are printed in vertical list(printFull ignores this)
    /// • maxCharsInString        = 2000  ; set this to change how many characters of a string might be printed at once.
    let toNiceString (x:'T) :string = 
        let nsl = {
            maxDepth     = NiceStringSettings.maxNestingDepth
            maxVertItems = NiceStringSettings.maxItemsPerSeq
            maxHorChars  = NiceStringSettings.maxCharsPerLine }
        x |> box |> getLines nsl 0 |> formatLines nsl


    /// Nice formating for numbers including thousand Separator and (nested) sequences, first 50 items are printed out.
    /// Settings are exposed in FsEx.NiceString.NiceStringSettings:
    /// • thousandSeparator       = '     ; set this to change the printing of floats and integers larger than 10'000
    /// • maxNestingDepth         = 3     ; set this to change how deep the content of nested seq is printed (printFull ignores this)
    /// • maxCharsInString        = 2000  ; set this to change how many characters of a string might be printed at once.
    let toNiceStringLong (x:'T) :string = 
        let nsl = {
            maxDepth     = NiceStringSettings.maxNestingDepth
            maxVertItems = 50
            maxHorChars  = NiceStringSettings.maxCharsPerLine }
        x |> box |> getLines nsl 0 |> formatLines nsl


    /// Nice formating for numbers including thousand Separator, all items of sequences, including nested items, are printed out.
    /// Settings are exposed in FsEx.NiceString.NiceStringSettings:
    /// • thousandSeparator       = '      ; set this to change the printing of floats and integers larger than 10'000
    /// • maxCharsInString        = 2000   ; set this to change how many characters of a string might be printed at once.
    let toNiceStringFull (x:'T) :string = 
        let nsl = {
            maxDepth     = 300
            maxVertItems = Int32.MaxValue
            maxHorChars  = NiceStringSettings.maxCharsPerLine }
        x |> box |> getLines nsl 0 |> formatLines nsl



