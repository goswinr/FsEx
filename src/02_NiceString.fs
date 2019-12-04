namespace FsEx

open System.Collections.Generic
open System
open System.Runtime.CompilerServices
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.Patterns
open System.Runtime.CompilerServices

module NiceString =
    
    let mutable thousandSeparator = "'"

    let internal formatThousands (s:string) =
        let last = s.Length - 1         
        let sb= Text.StringBuilder()
        let inline add (c:char) = sb.Append(c) |> ignore
        for i = 0 to last do
            if i = 0 || i = last then 
                add s.[i]
            elif i = 1 && s.[0] = '-' then 
                add s.[i]
            else
                if (last - i + 1) % 3 = 0 then 
                    sb.Append(thousandSeparator) |> ignore
                    add s.[i]
                else                
                    add s.[i]
        sb.ToString() 
    
    ///Formating with automatic precision 
    ///e.g.: 0 digits behind comma if above 1000 
    let floatToString  (x:float) =
        if   Double.IsNaN x then "NaN"
        elif Double.IsInfinity x then "Infinity"
        elif x = -1.23432101234321e+308 then "-1.234321e+308 (=RhinoMath.UnsetValue)" // for https://developer.rhino3d.com/api/RhinoCommon/html/F_Rhino_RhinoMath_UnsetValue.htm
        elif x = 0.0 then "0.0" // not "0" as in sprintf "%g"
        else
            let  a = abs x
            if   a > 10000. then sprintf "%.0f" x |> formatThousands
            elif a > 1000.  then sprintf "%.0f" x
            elif a > 100.   then sprintf "%.1f" x 
            elif a > 10.    then sprintf "%.2f" x 
            elif a > 1.     then sprintf "%.3f" x 
            else                 sprintf "%g"   x  

    ///Formating with automatic precision 
    ///e.g.: 0 digits behind comma if above 1000
    let singleToString  (x:float32) =
        if   Single.IsNaN x then "NaN"
        elif Single.IsInfinity x then "Infinity"
        elif x = -1.234321e+38f then "-1.2343e+38 (=RhinoMath.UnsetSingle)" // for https://developer.rhino3d.com/api/RhinoCommon/html/F_Rhino_RhinoMath_UnsetSingle.htm
        elif x = 0.0f then "0.0" // not "0" as in sprintf "%g"
        else
            let  a = abs x
            if   a > 10000.f then sprintf "%.0f" x |> formatThousands
            elif a > 1000.f  then sprintf "%.0f" x
            elif a > 100.f   then sprintf "%.1f" x 
            elif a > 10.f    then sprintf "%.2f" x 
            elif a > 1.f     then sprintf "%.3f" x 
            else                  sprintf "%g"   x  
    
    //--------------------------------
    // -- generic pretty printer-----
    //-------------------------------

    let mutable toNiceStringMaxDepth = 2
    let mutable toNiceStringMaxItemsPerSeq = 4

    let private before (splitter:string) (s:string) = 
        let start = s.IndexOf(splitter) 
        if start = -1 then s
        else s.Substring(0, start )
    
    let private formatTypeName nameSpace name = 
        let name = name |> before "`" // Collections.Generic.List`1  -> Collections.Generic.List
        let fullPath = nameSpace  + "." + name 
        if fullPath.StartsWith "System." then 
            fullPath.Substring(7)
        else 
            fullPath
    
    type private Count = Count of int | MoreThan of int

    let private (|IsSeq|_|) (xs : obj) =
        let typ = xs.GetType() 
        let interfaces= typ.GetInterfaces()
        let seqType = interfaces  |> Seq.tryFind( fun x -> x.IsGenericType && x.GetGenericTypeDefinition() = typedefof<IEnumerable<_>> )
        match seqType with
        |Some iet ->
            let iCollType = interfaces  |> Seq.tryFind( fun x -> x.IsGenericType && x.GetGenericTypeDefinition() = typedefof<ICollection<_>> )
            let count =
                match iCollType with
                |None -> -1 // no count available on sequences that do not have ICollection interface
                |Some _ -> (xs :?> Collections.ICollection).Count
            
            let args = iet.GetGenericArguments()
            if args.Length = 1 then             
                let arg = args.[0]            
                let name =   formatTypeName typ.Namespace typ.Name           
                let elName = formatTypeName arg.Namespace arg.Name 
                if args.[0].IsValueType then //create new trimmed Colllection with values boxed
                    let rs = ResizeArray<obj>()
                    let ics = xs :?> Collections.IEnumerable
                    let enum = ics.GetEnumerator()
                    let mutable k = 0
                    while enum.MoveNext() && k < toNiceStringMaxItemsPerSeq do 
                        rs.Add (box enum.Current)
                        k <- k+1
                    if k= toNiceStringMaxItemsPerSeq && enum.MoveNext() then                         
                        Some (MoreThan k, rs:>IEnumerable<_>, name, elName)   // the retuened seq is trimmed!  to a count of maxItemsPerSeq
                    else 
                        Some (Count k, rs:>IEnumerable<_>, name, elName)
                else                
                    let ics = xs :?> IEnumerable<_> 
                    if count > 0 then 
                        Some (Count count, ics, name, elName)
                    else
                        let enum = ics.GetEnumerator()
                        let mutable k = 0
                        while enum.MoveNext() && k < toNiceStringMaxItemsPerSeq do 
                            k <- k+1
                        if k= toNiceStringMaxItemsPerSeq && enum.MoveNext() then                         
                            Some (MoreThan k, ics, name, elName)  
                        else 
                            Some (Count k, ics, name, elName)
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
        match e with ///https://stackoverflow.com/questions/3151099/is-there-a-way-in-f-to-type-test-against-a-generic-type-without-specifying-the
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
    
    /// The internal stringbuilder for recursive function
    let private sb = Text.StringBuilder()


    let rec private toNiceStringRec (x:obj, externalFormater: obj-> option<string> , indent:int) : unit =
        
        let add  (s:string) =  sb.Append(String(' ', 4 *  indent )).Append(s)     |> ignore
        let adn  (s:string) =  sb.AppendLine(s) |> ignore
        
        match externalFormater x with
        | Some s -> add s
        | None ->
            match x with // boxed already
            | null -> "'null' (or Option.None)" |> add
            | :? float      as v   -> v |> floatToString    |> add
            | :? single     as v   -> v |> singleToString   |> add        
            | :? Char       as c   -> c.ToString()          |> add // "'" + c.ToString() + "'" // or add qotes?
            | :? string     as s   -> s                     |> add // to not have it in quotes, s.ToString() adds a " at start and end
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
            | _ ->  x.ToString() |> add
    
    /// Nice formating for floats  and sequences of any kind, first four items are printed out.
    /// use externalFormater for types defined in other assemblies alowed
    /// set NiceString.toNiceStringMaxItemsPerSeq to other value if more or less shall be shown (default is 4)
    /// set NiceString.toNiceStringMaxDepth to change how deep nested lists are printed (default is 2)
    let toNiceStringWithFormater (x:'T, externalFormater: obj-> option<string>) = 
        sb.Clear() |> ignore
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

        sb.Clear() |> ignore
        toNiceStringRec(box x, externalFormater, 0)

        toNiceStringMaxDepth <- maxDepthP 
        toNiceStringMaxItemsPerSeq  <- maxItemsPerSeqP 
        let s = sb.ToString()
        let st = s.Trim()
        if st.Contains (Environment.NewLine) then s else st // trim new line on one line strings

    /// Nice formating for floats , some Rhino Objects and sequences of any kind, all items including nested items are printed out.
    let toNiceStringFull (x:'T) = toNiceStringFullWithFormater(x, (fun _ -> None))

[<AutoOpen>]
module TypeExtensionsObject =   
    
    /// prints toNiceStringFull
    let print x = printfn "%s" (NiceString.toNiceStringFull x)

   
    [<EXT>]
    type Object with 
        [<EXT>]  
        ///A property like the ToString() method, 
        ///But with richer formationg for collections
        member obj.ToNiceString = NiceString.toNiceString obj

