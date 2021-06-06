namespace FsEx

open System
open System.Globalization

open FsEx.SaveIgnore //so that  |> ignore  can only be used on value types


/// exposes the settings used in toNiceString pretty printing function
module NiceStringSettings = 
    
    /// |Head of string * seq<Lines>
    /// |Element of string
    /// |EarlyEnd // "this is just the string "..." or similar;  for not fully enumerated sequences 
    type Lines = 
        |Head of string * ResizeArray<Lines>
        |Element of string
        |EarlyEnd // "this is just the string "..." or similar 

    /// set this to change the printing of floats larger than 10'000
    let mutable thousandSeparator = '\'' // = just one quote '

    /// set this to change how deep the content of nested seq is printed (printFull ignores this)
    /// default = 3
    let mutable maxDepth = 3

    /// set this to change how how many items per seq are printed (printFull ignores this)
    /// default = 5
    let mutable maxItemsPerSeq = 5

    /// set this to change how many characters of a string might be printed at onece.
    /// default = 2000
    let mutable maxCharsInString = 2000


    /// if the absolut value of a float is below this, display ±0.0
    /// default = Double.Epsilon = no rounding down
    let mutable roundToZeroBelow = Double.Epsilon
 

    /// Allows to inject an optional formater that gets called befor main formater
    /// This formater shall return None if the main formater should be used
    /// use externalFormater for types defined in other assemblies 
    let mutable externalFormater : obj -> option<Lines> = 
        fun _ -> None   

    

module NiceFormat  = 
    
    module Literals = 
        
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
        let AlmostZero = "~0.0"

        [<Literal>]
        let AlmostZeroNeg = "-~0.0"

        [<Literal>]
        let RoundedToZero = "±0.0"
    
    open NiceStringSettings

    //let internal deDE = Globalization.CultureInfo("de-DE")
    let internal invC = Globalization.CultureInfo.InvariantCulture

    /// Assumes a string that represent a float or int with '.' as decimal serapator and no other input formating
    let addThousandSeparators (s:string) =
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
    let tryParseNiceFloat (s:string)=
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
        // because abs(Int32.MinValue) would fail !!
        if x = Int32.MinValue || abs (x) >= 1000 then x.ToString() |> addThousandSeparators 
        else                                          x.ToString()  

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
        if stringToTrim.Length <= NiceStringSettings.maxCharsInString + 20 then sprintf "\"%s\""stringToTrim
        else 
            let len   = stringToTrim.Length
            let st    = stringToTrim.Substring(0, maxCharsInString) 
            let last20 = stringToTrim.Substring(len-21) 
            sprintf "\"%s[<< ...... %d more chars ...... >>]%s\"" st (len - maxCharsInString - 20) last20

    
    /// return the string befor a splitter
    /// if splitter is missing return full string 
    let inline before (splitter:Char) (s:string) = 
        let start = s.IndexOf(splitter) 
        if start = -1 then s
        else s.Substring(0, start )
    
    /// Includes GenericArguments types if available
    /// remove tick at end e.g. List`1  to  Collections.Generic.List
    let rec typeName (typ:Type) =
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
    open NiceStringSettings 
    open Microsoft.FSharp.Reflection
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.DerivedPatterns
    open Microsoft.FSharp.Quotations.Patterns   
    
 

    type SeqCount = Counted of int | More of int
 
    /// retruns  if the returned list is trimmed to maxCount,  input has more elements then maxCount
    let rec getItemsInSeq depth (xs:Collections.IEnumerable) : ResizeArray<Lines>=  // non generic IEnumerable
        let mutable reachedEnd = false
        let mutable k = 0
        let rs = ResizeArray<Lines>() 
        let enum = xs.GetEnumerator()
        while enum.MoveNext() && k < maxItemsPerSeq do 
            rs.Add(getLines depth (box enum.Current))
            k <- k+1
        if k = maxItemsPerSeq && enum.MoveNext() then // don@iteret full seqence if only the first few items are printed
            rs.Add( EarlyEnd)
        rs                                                 
  

    and getCollection depth (x:obj) (xs:Collections.ICollection) :Lines = // non generic ICollection
        // count is always available 
        let typ = xs.GetType()
        let name =  NiceFormat.typeName   typ
        let desc = 
            if xs.Count = 0 then sprintf "empty %s "  name 
            else                 sprintf "%s with %d items"  name xs.Count
        if depth = maxDepth then Element desc else Head (desc, getItemsInSeq (depth+1) xs )


    and getSeq depth (x:obj) (xs:Collections.IEnumerable) :Lines = // non generic IEnumerable
        // count may not be available , sequences are not only iterated till NiceStringSettings.maxItemsPerSeq
        let typ = xs.GetType()
        let name =   NiceFormat.typeName   typ   
        let seqTyO = typ.GetInterfaces()|> Array.tryFind( fun x -> x.IsGenericType && x.GetGenericTypeDefinition() = typedefof<IEnumerable<_>> )//  generic IEnumerable        
        let ls = getItemsInSeq (depth+1) xs
        let desc = 
            if ls.Count = 0 then                 sprintf "empty %s " name 
            elif ls.[ls.Count-1] = EarlyEnd then sprintf "%s with more than %d items" name  (ls.Count-1) 
            else                                 sprintf "%s with %d items" name  ls.Count
        if depth = maxDepth then Element desc else Head (desc, ls )
        
        

    ///  x is boxed already
    and getLines depth (x:obj) : Lines =        
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
            | :? string     as s   -> if depth=0 && maxItemsPerSeq = Int32.MaxValue then s |> Element // dont truncate string if toplevel and printFull
                                      else  NiceFormat.truncateString s  |> Element   
            | :? Guid       as g   -> sprintf "Guid[%O]" g    |> Element 
            | :? Collections.ICollection as xs -> getCollection depth x xs
            | :? Collections.IEnumerable as xs -> getSeq depth x xs            
            | _ ->  
                let t = x.GetType()
                if FSharpType.IsTuple(t) then // TODO test
                    let tyStr = NiceFormat.typeName  t
                    let fields = FSharpValue.GetTupleFields(x)
                    let desc =  sprintf "%s of %d items" tyStr fields.Length
                    if depth = maxDepth then Element desc else Head (desc, getItemsInSeq (depth+1) fields)
                
                elif FSharpType.IsUnion(t) then // TODO test
                    let union, fields =  FSharpValue.GetUnionFields(x, t)
                    let declTy = union.DeclaringType
                    let mainType = NiceFormat.typeName  declTy
                    let desc =  sprintf "a %s of case %s" mainType union.Name 
                    if depth = maxDepth then Element desc else Head (desc, getItemsInSeq (depth+1) fields) // TODO test what are the fields                
                else
                    sprintf "%A" x |> Element
                
                // TODO use 
                // http://www.fssnip.net/cV/title/A-Generic-PrettyPrinter-for-Record-types 

    let formatLines (lines:Lines) = 
        match lines with
        |Element s    -> s //without adding a new line at end
        |EarlyEnd     -> eprintf "EarlyEnd schould not be at root of Lines tree" ; "..."
        |Head (h0,xs0)  -> 
            let sb = Text.StringBuilder(h0).AppendLine()             
            let rec loop depth (lns:Lines) = 
                if depth <= maxDepth then 
                    match lns with
                    |Element s    -> sb.Append(String(' ', 4 * depth)).AppendLine(s)        |> ignoreObj 
                    |EarlyEnd     -> sb.Append(String(' ', 4 * depth)).AppendLine("(...)")  |> ignoreObj  
                    |Head (h,xs)  ->                                  
                                     // header:
                                     sb.Append(String(' ', 4 * depth)).Append(h)     |> ignoreObj  
                                     (if xs.Count=0 then sb.AppendLine() else sb.AppendLine(":"))  |> ignoreObj  // only add colon if items follow
                                     // items
                                     for x in xs do 
                                        loop (depth+1) x
            
            for x0 in xs0 do 
                loop (1) x0
            
            //finally trimm of line return at end
            let len = sb.Length
            if sb.Chars(len-2) = '\r' then 
                sb.Remove(len-2 , 2) |> ignoreObj  
            sb.ToString()

    (*
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

module NiceString  =
    open NiceStringImplementation
    open NiceStringSettings


    //TODO sync the docstring here with print and printfull in module Print and the shadowing one in Rhino.scripting 


    /// Nice formating for numbers including thousand Separator and (nested) sequences, first five items are printed out.
    /// Settings are exposed in FsEx.NiceString.NiceStringSettings:
    /// • thousandSeparator          = '\'' (this is just one quote: ')  ; set this to change the printing of floats and integers larger than 10'000
    /// • toNiceStringMaxDepth       = 3                                 ; set this to change how deep the content of nested seq is printed (printFull ignores this)
    /// • toNiceStringMaxItemsPerSeq = 5                                 ; set this to change how how many items per seq are printed (printFull ignores this)
    /// • maxCharsInString           = 5000                              ; set this to change how many characters of a string might be printed at once.    
    let toNiceString (x:'T) = 
        x |> box |> getLines 0 |> formatLines


    /// Nice formating for numbers including thousand Separator, all items of sequences, including nested items, are printed out.
    /// Settings are exposed in FsEx.NiceString.NiceStringSettings:
    /// • thousandSeparator          = '\'' (this is just one quote: ')  ; set this to change the printing of floats and integers larger than 10'000
    /// • maxCharsInString           = 5000                              ; set this to change how many characters of a string might be printed at once.
    let toNiceStringFull (x:'T) = 
        //set depth and max item count to 99999999 afterwards then reset it again :
        let pervDepth    = maxDepth
        let prevMaxItems = maxItemsPerSeq
        maxDepth        <- Int32.MaxValue
        maxItemsPerSeq  <- Int32.MaxValue
        let res = x |> box |> getLines 0 |> formatLines
        maxDepth        <- pervDepth  // TODO if format lines fails values dont get reset !
        maxItemsPerSeq  <- prevMaxItems
        res

       
