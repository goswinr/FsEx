namespace FsEx

open System
open FsEx.SaveIgnore //so that  |> ignore  can only be used on value types
open System


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
    /// default = 5000
    let mutable maxCharsInString = 5000

    /// if the absolut value of a float is below this, display just Zero
    /// default = Double.Epsilon = no rounding down
    let mutable roundToZeroBelow = Double.Epsilon
 

    /// Allows to inject an optional formater that gets called befor main formater
    /// This formater shall return None if the main formater should be used
    /// use externalFormater for types defined in other assemblies 
    let mutable externalFormater : obj -> option<Lines> = 
        fun _ -> None   
    

module NiceFormat  = 
    open NiceStringSettings

    //let internal deDE = Globalization.CultureInfo("de-DE")
    let internal invC = Globalization.CultureInfo.InvariantCulture


    let addThousandSeparators (s:string) =
        let last = s.Length - 1         
        let sb = Text.StringBuilder()
        let inline add (c:char) = sb.Append(c) |> ignoreObj
        for i = 0 to last do
            if i = 0 || i = last then 
                add s.[i]
            elif i = 1 && s.[0] = '-' then 
                add s.[i]
            else
                if (last - i + 1) % 3 = 0 then 
                    add thousandSeparator
                    add s.[i]
                else                
                    add s.[i]
        sb.ToString() 
    
    let int (x:int) = 
        if abs(x) > 1000 then x.ToString() |> addThousandSeparators
        else                  x.ToString() 

    /// Formating with automatic precision 
    /// e.g.: 0 digits behind comma if above 1000 
    let float  (x:float) =
        if   Double.IsNaN x then "NaN"
        elif x = Double.NegativeInfinity then "Negative Infinity"
        elif x = Double.PositiveInfinity then "Positive Infinity"
        elif x = -1.23432101234321e+308 then "-1.23432e+308 ( = RhinoMath.UnsetValue)" // for https://developer.rhino3d.com/api/RhinoCommon/html/F_Rhino_RhinoMath_UnsetValue.htm
        elif x = 0.0 then "0.0" // not "0" as in sprintf "%g"
        else
            let  a = abs x
            if   a < roundToZeroBelow then "0.0"
            elif a > 10000. then x.ToString("#")|> addThousandSeparators 
            elif a > 1000.  then x.ToString("#")
            elif a > 100.   then x.ToString("#.#" , invC)
            elif a > 10.    then x.ToString("#.##" , invC)
            elif a > 1.     then x.ToString("#.###" , invC)
            elif a > 0.1    then x.ToString("0.####" , invC)
            elif a > 0.01   then x.ToString("0.#####" , invC)
            elif a > 0.001  then x.ToString("0.######" , invC)
            elif a > 0.0001 then x.ToString("0.#######" , invC)
            else                  x.ToString("0.###############" , invC)// 15 decimal paces for doubles
    
    /// Formating with automatic precision 
    /// e.g.: 0 digits behind comma if above 1000 
    let decimal  (x:Decimal) =
        if x = 0M then "0.0" // not "0" as in sprintf "%g"
        else
            let  a = abs x
            if   a < decimal(roundToZeroBelow) then "0.0"
            elif a > 10000M then x.ToString("#")|> addThousandSeparators 
            elif a > 1000M  then x.ToString("#")
            elif a > 100M   then x.ToString("#.#" , invC)
            elif a > 10M    then x.ToString("#.##" , invC)
            elif a > 1M     then x.ToString("#.###" , invC)
            elif a > 0.1M    then x.ToString("0.####" , invC)
            elif a > 0.01M   then x.ToString("0.#####" , invC)
            elif a > 0.001M  then x.ToString("0.######" , invC)
            elif a > 0.0001M then x.ToString("0.#######" , invC)
            else                  x.ToString("0.########" , invC)  

    /// Formating with automatic precision 
    /// e.g.: 0 digits behind comma if above 1000
    let single (x:float32) =
        if   Single.IsNaN x then "NaN"
        elif x = Single.NegativeInfinity then "Negative Infinity"
        elif x = Single.PositiveInfinity then "Positive Infinity"            
        elif x = -1.234321e+38f then "-1.2343e+38 ( = RhinoMath.UnsetSingle)" // for https://developer.rhino3d.com/api/RhinoCommon/html/F_Rhino_RhinoMath_UnsetSingle.htm
        elif x = 0.0f then "0.0" // not "0" as in sprintf "%g"
        else
            let  a = abs x
            if   a < float32(roundToZeroBelow) then "0.0"
            elif a > 10000.f then x.ToString("#")|> addThousandSeparators 
            elif a > 1000.f  then x.ToString("#")
            elif a > 100.f   then x.ToString("#.#" , invC)
            elif a > 10.f    then x.ToString("#.##" , invC)
            elif a > 1.f     then x.ToString("0.###" , invC)
            elif a > 0.1f    then x.ToString("0.####" , invC)
            elif a > 0.01f   then x.ToString("0.#####" , invC)
            elif a > 0.001f  then x.ToString("0.######" , invC)
            else                  x.ToString("0.#######" , invC) // 7 decimal paces for singles
    

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
            let last6 = stringToTrim.Substring(len-7) 
            sprintf "\"%s[..%d more Chars..]%s\"" st (len - maxCharsInString - 6) last6

    
    /// return the string befor a splitter
    /// if splitter is missing return full string 
    let inline before (splitter:Char) (s:string) = 
        let start = s.IndexOf(splitter) 
        if start = -1 then s
        else s.Substring(0, start )
    

    /// remove tick at end e.g. List`1  to  Collections.Generic.List
    let typeName (nameSpace:string) name = 
        let name = name |> before '`' |> before '@' 
        let fullPath = if String.IsNullOrEmpty nameSpace then name else nameSpace  + "." + name 
        if fullPath.StartsWith "System." then 
            fullPath.Substring(7)
        else 
            fullPath
                .Replace("Microsoft.FSharp.Collections.mkSeq","seq")
                .Replace("Microsoft.FSharp.Collections.","")
                .Replace("Microsoft.FSharp.Core.","")
    


module internal NiceStringImplementation  =
    open System.Collections.Generic 
    open NiceStringSettings 
    open Microsoft.FSharp.Reflection
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.DerivedPatterns
    open Microsoft.FSharp.Quotations.Patterns   
    
    // TODO use 
    // http://www.fssnip.net/cV/title/A-Generic-PrettyPrinter-for-Record-types 

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
        let name =   NiceFormat.typeName   typ.Namespace   typ.Name   
        let collTyO = typ.GetInterfaces()|> Array.tryFind( fun x -> x.IsGenericType && x.GetGenericTypeDefinition() = typedefof<ICollection<_>> ) // generic ICollection
        let desc = // the header line of a collection 
            match collTyO with
            |Some collTy ->
                match collTy.GetGenericArguments() |> Array.tryHead with 
                |Some elTyp -> 
                    let elName = NiceFormat.typeName elTyp.Namespace elTyp.Name
                    sprintf "%s with %d items of %s" name xs.Count elName
                |None -> sprintf "%s with %d items"  name xs.Count
            |None ->     sprintf "%s with %d items"  name xs.Count
        if depth = maxDepth then Element desc else Head (desc, getItemsInSeq (depth+1) xs )

    and getSeq depth (x:obj) (xs:Collections.IEnumerable) :Lines = // non generic IEnumerable
        // count may not be available , sequences are not only iterated till NiceStringSettings.maxItemsPerSeq
        let typ = xs.GetType()
        let name =   NiceFormat.typeName   typ.Namespace   typ.Name   
        let seqTyO = typ.GetInterfaces()|> Array.tryFind( fun x -> x.IsGenericType && x.GetGenericTypeDefinition() = typedefof<IEnumerable<_>> )//  generic IEnumerable        
        let ls = getItemsInSeq (depth+1) xs
        let count = if ls.Count = 0 then Counted 0 elif ls.[ls.Count-1] = EarlyEnd then More (ls.Count-1) else  Counted ls.Count
        let desc = // the header line of a collection 
            match seqTyO with
            |Some seqTy ->                 
                match seqTy.GetGenericArguments() |> Array.tryHead  with 
                |Some elTyp -> 
                    let elName = NiceFormat.typeName elTyp.Namespace elTyp.Name
                    match count with 
                    |More    i -> sprintf "%s with more than %d items of %s" name i elName
                    |Counted i -> sprintf "%s with %d items of %s" name i elName
                |None -> 
                    match count with 
                    |More    i -> sprintf "%s with more than %d items" name i 
                    |Counted i -> sprintf "%s with %d items" name i                
            |None ->  
                match count with 
                |More    i -> sprintf "%s with more than %d items" name i 
                |Counted i -> sprintf "%s with %d items" name i
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
            | :? string     as s   -> NiceFormat.truncateString  s  |> Element   
            | :? Guid       as g   -> sprintf "Guid[%O]" g    |> Element 
            | :? Collections.ICollection as xs -> getCollection depth x xs
            | :? Collections.IEnumerable as xs -> getSeq depth x xs            
            | _ ->  
                let t = x.GetType()
                if FSharpType.IsTuple(t) then // TODO test
                    let fields = FSharpValue.GetTupleFields(x)
                    let desc =  sprintf "Tuple of %d items" fields.Length
                    if depth = maxDepth then Element desc else Head (desc, getItemsInSeq (depth+1) fields)
                
                elif FSharpType.IsUnion(t) then // TODO test
                    let union, fields =  FSharpValue.GetUnionFields(x, t)
                    let declTy = union.DeclaringType
                    let mainType = NiceFormat.typeName  declTy.Namespace declTy.Name
                    let desc =  sprintf "a %s of case %s" mainType union.Name
                    if depth = maxDepth then Element desc else Head (desc, getItemsInSeq (depth+1) fields) // TODO test what are the fields
                
                else
                    sprintf "%A" x |> Element
    

    let formatLines (lines:Lines) = 
        let sb = Text.StringBuilder()        
        let rec loop depth (lns:Lines) = 
            if depth <= maxDepth then 
                match lns with
                |Element s    -> sb.Append(String(' ', 4 * depth)).AppendLine(s)      |> ignoreObj 
                |EarlyEnd     -> sb.Append(String(' ', 4 * depth)).AppendLine("(...)")  |> ignoreObj  
                |Head (h,xs)  ->                                  
                                 sb.Append(String(' ', 4 * depth)).Append(h)     |> ignoreObj  
                                 (if xs.Count=0 then sb.AppendLine() else sb.AppendLine(":"))  |> ignoreObj  // only add colon if items follow
                                 for x in xs do loop (depth+1) x
        loop 0 lines
        sb.ToString()


    (*

    open Microsoft.FSharp.Reflection
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.DerivedPatterns
    open Microsoft.FSharp.Quotations.Patterns    

    open System.Reflection
    open Microsoft.FSharp.Reflection
    
    let explore x =
        let t = x.GetType()
        if FSharpType.IsTuple(t) then
            let fields =
                FSharpValue.GetTupleFields(x)
                |> Array.map string
                |> fun strings -> System.String.Join(", ", strings)
            
            printfn "Tuple: (%s)" fields
        elif FSharpType.IsUnion(t) then
            let union, fields =  FSharpValue.GetUnionFields(x, t)
            
            printfn "Union: %s(%A)" union.Name fields
        else
            printfn "Got another type"



    //reflection fails for extension members?
    let (|StructToNiceString|_|) (x : obj) = // for structs that hav a ToNiceString property 
        let typ = x.GetType() 
        if typ.IsValueType then            
            let prop = typ.GetProperty("ToNiceString")
            if isNull prop then None
            else
                let txt = prop.GetValue(x)
                try
                    Some (txt :?> string)
                with | _ -> None
        else
            None


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
        maxDepth        <- pervDepth
        maxItemsPerSeq  <- prevMaxItems
        res

       
