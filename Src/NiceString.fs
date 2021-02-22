namespace FsEx

open System.Collections.Generic
open System
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.Patterns
open FsEx.SaveIgnore //so that  |> ignore  can only be used on value types
open System.Linq

/// exposes the settings used in toNiceString pretty printing function
module NiceStringSettings = 

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

    
    /// |Head of string * seq<Lines>
    /// |Element of string
    /// |EarlyEnd // "this is just the string "..." or similar  
    type Lines = 
        |Head of string * seq<Lines>
        |Element of string
        |EarlyEnd // "this is just the string "..." or similar  

    /// Allows to inject an optional formater that gets called befor main formater
    /// This formater shall return None if the main formater should be used
    /// use externalFormater for types defined in other assemblies 
    let mutable externalFormater : obj -> option<Lines> = 
        fun _ -> None   
    
[<RequireQualifiedAccess>]
module internal Format  = 

    let addThousandSeparators (s:string) =
        let last = s.Length - 1         
        let sb= Text.StringBuilder()
        let inline add (c:char) = sb.Append(c) |> ignoreObj
        for i = 0 to last do
            if i = 0 || i = last then 
                add s.[i]
            elif i = 1 && s.[0] = '-' then 
                add s.[i]
            else
                if (last - i + 1) % 3 = 0 then 
                    add NiceStringSettings.thousandSeparator
                    add s.[i]
                else                
                    add s.[i]
        sb.ToString() 

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
            if   a > 10000. then x.ToString("#") |> addThousandSeparators 
            elif a > 1000.  then x.ToString("#")
            elif a > 100.   then x.ToString("#.#")
            elif a > 10.    then x.ToString("#.##")
            elif a > 1.     then x.ToString("#.###")
            elif a > 0.1    then x.ToString("#.####")
            elif a > 0.01   then x.ToString("#.#####")
            elif a > 0.001  then x.ToString("#.######")
            elif a > 0.0001 then x.ToString("#.#######")
            else                  x.ToString("#.###############") 
        

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
            if   a > 10000.f then x.ToString("#") |> addThousandSeparators 
            elif a > 1000.f  then x.ToString("#")
            elif a > 100.f   then x.ToString("#.#")
            elif a > 10.f    then x.ToString("#.##")
            elif a > 1.f     then x.ToString("#.###")
            elif a > 0.1f    then x.ToString("#.####")
            elif a > 0.01f   then x.ToString("#.#####")
            elif a > 0.001f  then x.ToString("#.######")
            elif a > 0.0001f then x.ToString("#.#######")
            else                  x.ToString("#.###############")  
    

    /// If the input string is longer than maxChars + 20 then 
    /// it returns the input string trimmed to maxChars, a count of skiped characters and the last 6 characters (all enclosed in double quotes ")
    /// e.g. "abcde[..20 more Chars..]xyz"
    /// Else, if the input string is less than maxChars + 20, it is still returned in full (enclosed in double quotes ").
    let truncate (maxChars:int) (stringToTrim:string) =
        if stringToTrim.Length <= maxChars + 20 then sprintf "\"%s\""stringToTrim
        else 
            let len   = stringToTrim.Length
            let st    = stringToTrim.Substring(0,maxChars) 
            let last6 = stringToTrim.Substring(len-7) 
            sprintf "\"%s[..%d more Chars..]%s\"" st (len - maxChars - 6) last6


    
    /// return the string befor a splitter
    /// if splitter is missing return full string 
    let inline before (splitter:string) (s:string) = 
        let start = s.IndexOf(splitter,StringComparison.Ordinal) 
        if start = -1 then s
        else s.Substring(0, start )
    

    /// remove tick at end e.g. List`1  to  Collections.Generic.List
    let typeName nameSpace name = 
        let name = name |> before "`" 
        let fullPath = nameSpace  + "." + name 
        if fullPath.StartsWith "System." then 
            fullPath.Substring(7)
        else 
            fullPath
    


module internal NiceStringImplementation  =
    
    open NiceStringSettings    
    
    // TODO use 
    // http://www.fssnip.net/cV/title/A-Generic-PrettyPrinter-for-Record-types    
       


    type Count = 
        | Counted  of int 
        | MoreThan of int

    let  (|IsSeq|_|) (xs : obj) : option<Count*seq<obj>*string*string> =
        let typ = xs.GetType() 
        let interfaces= typ.GetInterfaces()
        let seqType = interfaces  |> Seq.tryFind( fun x -> x.IsGenericType && x.GetGenericTypeDefinition() = typedefof<IEnumerable<_>> )
        match seqType with
        |Some iet ->
            let iCollType = interfaces  |> Seq.tryFind( fun x -> x.IsGenericType && x.GetGenericTypeDefinition() = typedefof<ICollection<_>> )
            let count =
                match iCollType with
                |None -> -1 // no count available on sequences that do not have ICollection interface
                |Some _ -> 
                    try (xs :?> ICollection<_>).Count // TODO fix
                    with _ -> -2
        
            let args = iet.GetGenericArguments()
            if args.Length = 1 then             
                let arg = args.[0]            
                let name =   Format.typeName typ.Namespace typ.Name           
                let elName = Format.typeName arg.Namespace arg.Name 
                try // xs :?> IEnumerable<_> may stil fail  on Map // TODO fix
                    if args.[0].IsValueType then //create new trimmed Colllection with values boxed
                    
                        let rs = ResizeArray<obj>()
                        let ics = xs :?> IEnumerable<_>
                        let enum = ics.GetEnumerator()
                        let mutable k = 0
                        while enum.MoveNext() && k < maxItemsPerSeq do 
                            rs.Add (box enum.Current)
                            k <- k+1
                        if k = maxItemsPerSeq && enum.MoveNext() && count < 0 then                         
                            Some (MoreThan k, rs:>IEnumerable<_>, name, elName)   // the retuened seq is trimmed!  to a count of maxItemsPerSeq
                        else 
                            Some (Counted count, rs:>IEnumerable<_>, name, elName)
                    else                
                        let ics = xs :?> IEnumerable<_> 
                        if count > 0 then 
                            Some (Counted count, ics, name, elName)
                        else
                            let enum = ics.GetEnumerator()
                            let mutable k = 0
                            while enum.MoveNext() && k < maxItemsPerSeq do 
                                k <- k+1
                            if k = maxItemsPerSeq && enum.MoveNext() && count < 0 then                         
                                Some (MoreThan k, ics, name, elName)  
                            else 
                                Some (Counted count, ics, name, elName)
                with _ -> None
            else
                None
        |None -> None

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
    

    let getCollHead count (x:obj) = ""
        
    type IsTrimmed = Trimmed | UnTrimmed

    /// retruns  if the returned list is trimmed to maxCount,  input has more elements then maxCount
    let truncateSeq maxCount (xs:seq<obj>) =  
        let mutable reachedEnd = false
        let mutable k = 0
        let rs = ResizeArray() 
        let enum = xs.GetEnumerator()
        while enum.MoveNext() && k < maxCount do 
            rs.Add( enum.Current)
            k <- k+1
        if k = maxCount && enum.MoveNext() then  Trimmed,   rs   // the returned Rarr is trimmed!  to a count of maxCount 
        else                                     UnTrimmed, rs   // the returned Rarr is untrimmed 
    

    let rec getCollection (x:obj) (xs:Collections.ICollection) :Lines = 
        let typ = xs.GetType()
        let name =   Format.typeName   typ.Namespace   typ.Name   
        let collTyO = typ.GetInterfaces()  |> Seq.tryFind( fun x -> x.IsGenericType && x.GetGenericTypeDefinition() = typedefof<ICollection<_>> )
        let desc = 
            match collTyO with
            |Some collTy -> 
                let elTyps = collTy.GetGenericArguments()
                match Array.tryHead elTyps with 
                |Some elTyp -> 
                    let elName = Format.typeName elTyp.Namespace elTyp.Name
                    sprintf "%s with %d items of %s:" name xs.Count elName
                |None -> sprintf "%s with %d items:"  name xs.Count
            |None ->     sprintf "%s with %d items:"  name xs.Count
        match truncateSeq maxItemsPerSeq with 
        
        Head (desc, [| for x in xs |> Seq.truncate maxItemsPerSeq do
                         toNiceStringRec x |])




    //  x is boxed already
    and rec toNiceStringRec (x:obj) : Lines =        
        match externalFormater x with // first check if externalFormater provides a string , this is used e.g. for types from RhinoCommon.dll
        | Some ln -> ln
        | None ->
            match x with // boxed already
            | null -> "'null' (or Option.None)"                           |> Element
            | :? float      as v   -> v |> Format.float                   |> Element
            | :? single     as v   -> v |> Format.single                  |> Element       
            | :? Char       as c   -> c.ToString()                        |> Element // "'" + c.ToString() + "'" // or add qotes?
            | :? string     as s   -> Format.truncate maxCharsInString s  |> Element    // is  in quotes, s.ToString() also adds a " at start and end
            | :? Guid       as g   -> sprintf "Guid[%O]" g                |> Element
            
            | :? Collections.ICollection as xs -> 
                    let head = getCollHead xs.Count x 
                    let items = [| for |]





            (*
            //| StructToNiceString s -> add s //reflection fails for extension members?
            | UC <@ Some @> [v]    -> adn "Option.Some: ";  toNiceStringRec (v, externalFormater, indent+1)
            //| UC <@ None @> [] -> add "Option.None or 'null'" caught above
            | IsSeq (leng, xs, name, elName) ->  
                    match leng with
                    |Counted count -> sprintf "%s with %d items of %s" name count elName |> add // new line added below at    adn ":" 
                    |MoreThan _ ->   sprintf "%s of %s" name elName  |> add 
            
                    if indent < toNiceStringMaxDepth  then 
                        adn ":"
                        for i, x in xs  |> Seq.truncate toNiceStringMaxItemsPerSeq |> Seq.indexed do  
                            if i > 0 then adn ""
                            toNiceStringRec (x, externalFormater, indent+1)                            
                
                        match leng with
                        |Count count -> 
                            if count > toNiceStringMaxItemsPerSeq then
                                adn ""
                                toNiceStringRec ("...", externalFormater, indent+1)                                
                        |MoreThan _ ->  
                            adn ""
                            toNiceStringRec ("...", externalFormater, indent+1)                            
                    else 
                        adn ""                
            //| _ ->  x.ToString() |> add
            *)
            | _ ->  sprintf "%A" x |> Element


            
            
                
    /// Nice formating for floats  and sequences of any kind, first four items are printed out.
    /// use externalFormater for types defined in other assemblies alowed
    /// set NiceString.toNiceStringMaxItemsPerSeq to other value if more or less shall be shown (default is 4)
    /// set NiceString.toNiceStringMaxDepth to change how deep nested lists are printed (default is 2)
    let  toNiceStringWithFormater (x:'T, externalFormater: obj-> option<string>) = 
        sb.Clear() |> ignoreObj
        toNiceStringRec(box x, externalFormater , 0 ) //0 indent for start
        sb.ToString()
            
    /// Nice formating for floats , some Rhino Objects and sequences of any kind, first four items are printed out.
    /// set NiceString.toNiceStringMaxItemsPerSeq to other value if more or less shall be shown (default is 4)
    /// set NiceString.toNiceStringMaxDepth to change how deep nested lists are printed (default is 2)
    let toNiceString (x:'T) = toNiceStringWithFormater(x, (fun _ -> None) ) 
               
            
    /// Nice formating for floats , some Rhino Objects and sequences of any kind, all items including nested items are printed out.
    /// use externalFormater for types defined in other assemblies alowed
    let toNiceStringFullWithFormater (x:'T, externalFormater: obj-> option<string>) = 
        let maxDepthP = toNiceStringMaxDepth  
        let maxItemsPerSeqP = toNiceStringMaxItemsPerSeq 
        toNiceStringMaxDepth <- Int32.MaxValue
        toNiceStringMaxItemsPerSeq  <- Int32.MaxValue
            
        sb.Clear() |> ignoreObj
        toNiceStringRec(box x, externalFormater, 0)
            
        toNiceStringMaxDepth <- maxDepthP 
        toNiceStringMaxItemsPerSeq  <- maxItemsPerSeqP 
        let s = sb.ToString()
        let st = s.Trim()
        if st.Contains (Environment.NewLine) then s else st // trim new line on one line strings
            
    /// Nice formating for floats , some Rhino Objects and sequences of any kind, all items including nested items are printed out.
    let toNiceStringFull (x:'T) = toNiceStringFullWithFormater(x, (fun _ -> None))



module NiceString  =


    /// Nice formating for floats , some Rhino Objects and sequences of any kind, first four items are printed out.
    /// Settings are exposed in NiceString.NiceStringSettings
    /// •thousandSeparator          = '\'' (this is just one quote: ')  ; set this to change the printing of floats larger than 10'000
    /// •toNiceStringMaxDepth       = 3                                 ; set this to change how deep the content of nested seq is printed (printFull ignores this)
    /// •toNiceStringMaxItemsPerSeq = 5                                 ; set this to change how how many items per seq are printed (printFull ignores this)
    /// • maxCharsInString          = 5000                              ;set this to change how many characters of a string might be printed at onece.    
    let toNiceString (x:'T) = 
        NiceStringSettings.mainFormater (box x)


    /// Nice formating for floats and numbers in including thousand Separator ,  all items including nested items are printed out.
    let toNiceStringFull (x:'T) = 
        NiceStringSettings.mainFormaterFull (box x)

              