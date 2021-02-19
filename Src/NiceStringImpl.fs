namespace FsEx

open System.Collections.Generic
open System
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.Patterns
open FsEx.SaveIgnore //so that  |> ignore  can only be used on value types


// TODO use 
// http://www.fssnip.net/cV/title/A-Generic-PrettyPrinter-for-Record-types



module internal NiceStringImplementation  =
    
    open NiceString

    //--------------------------------
    // -- generic pretty printer-----
    //-------------------------------


    /// return the string befor a splitter
    /// if splitter is missing return full string 
    let private before (splitter:string) (s:string) = 
        let start = s.IndexOf(splitter,StringComparison.Ordinal) 
        if start = -1 then s
        else s.Substring(0, start )

    let private formatTypeName nameSpace name = 
        let name = name |> before "`" // Collections.Generic.List`1  to  Collections.Generic.List
        let fullPath = nameSpace  + "." + name 
        if fullPath.StartsWith "System." then 
            fullPath.Substring(7)
        else 
            fullPath

    type private Count = Count of int | MoreThan of int

    let private (|IsSeq|_|) (xs : obj) : option<Count*seq<obj>*string*string> =
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
                let name =   formatTypeName typ.Namespace typ.Name           
                let elName = formatTypeName arg.Namespace arg.Name 
                try // xs :?> IEnumerable<_> may stil fail  on Map // TODO fix
                    if args.[0].IsValueType then //create new trimmed Colllection with values boxed
                    
                        let rs = ResizeArray<obj>()
                        let ics = xs :?> IEnumerable<_>
                        let enum = ics.GetEnumerator()
                        let mutable k = 0
                        while enum.MoveNext() && k < toNiceStringMaxItemsPerSeq do 
                            rs.Add (box enum.Current)
                            k <- k+1
                        if k = toNiceStringMaxItemsPerSeq && enum.MoveNext() && count < 0 then                         
                            Some (MoreThan k, rs:>IEnumerable<_>, name, elName)   // the retuened seq is trimmed!  to a count of maxItemsPerSeq
                        else 
                            Some (Count count, rs:>IEnumerable<_>, name, elName)
                    else                
                        let ics = xs :?> IEnumerable<_> 
                        if count > 0 then 
                            Some (Count count, ics, name, elName)
                        else
                            let enum = ics.GetEnumerator()
                            let mutable k = 0
                            while enum.MoveNext() && k < toNiceStringMaxItemsPerSeq do 
                                k <- k+1
                            if k = toNiceStringMaxItemsPerSeq && enum.MoveNext() && count < 0 then                         
                                Some (MoreThan k, ics, name, elName)  
                            else 
                                Some (Count count, ics, name, elName)
                with _ -> None
            else
                None
        |None -> None

    //reflection fails for extension members?
    let private (|StructToNiceString|_|) (x : obj) = // for structs that hav a ToNiceString property 
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


    let private (|UC|_|) e o =
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
    
    let fixVeryLongStr (s:string) =
        if s.Length > maxCharsInString then 
            if s.Length < maxCharsInString + 500 then s // if its just 500 more add them, its ok 
            else s.Substring(0,maxCharsInString) + sprintf "%s[...and %d more characters]" Environment.NewLine (s.Length - maxCharsInString)
        else 
            s

    /// The internal stringbuilder for recursive function
    let private sb = Text.StringBuilder()


    let rec private toNiceStringRec (x:obj, externalFormater: obj-> option<string> , indent:int) : unit =
    
        let add  (s:string) =  sb.Append(String(' ', 4 *  indent )).Append(s)     |> ignoreObj
        let adn  (s:string) =  sb.AppendLine(s) |> ignoreObj
    
        match externalFormater x with // first check if externalFormater provides a string , this is used e.g. for types from RhinoCommon.dll
        | Some s -> add s
        | None ->
            match x with // boxed already
            | null -> "'null' (or Option.None)" |> add
            | :? float      as v   -> v |> floatToString    |> add
            | :? single     as v   -> v |> singleToString   |> add        
            | :? Char       as c   -> c.ToString()          |> add // "'" + c.ToString() + "'" // or add qotes?
            | :? string     as s   -> fixVeryLongStr s      |> add // to not have it in quotes, s.ToString() adds a " at start and end
            | :? Guid       as g   -> sprintf "Guid[%O]" g  |> add
            //| StructToNiceString s -> add s //reflection fails for extension members?
            | UC <@ Some @> [v]    -> adn "Option.Some: ";  toNiceStringRec (v, externalFormater, indent+1)
            //| UC <@ None @> [] -> add "Option.None or 'null'" caught above
            | IsSeq (leng, xs, name, elName) ->  
                    match leng with
                    |Count count -> sprintf "%s with %d items of %s" name count elName |> add // new line added below at    adn ":" 
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
            | _ ->  sprintf "%A" x |> add


            
            
                
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
            