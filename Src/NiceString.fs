namespace FsEx

open System.Collections.Generic
open System
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.Patterns
open FsEx.SaveIgnore


// TODO use 
// http://www.fssnip.net/cV/title/A-Generic-PrettyPrinter-for-Record-types


[<AutoOpen>] // to have print functions at end of module auto opened
module Print =

    [<RequireQualifiedAccess>]
    module NiceString  =
    
        /// set this to change the printing of floats larger than 10'000
        let mutable thousandSeparator = '\'' // = just one quote '

        let internal formatThousands (s:string) =
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
                        add thousandSeparator
                        add s.[i]
                    else                
                        add s.[i]
            sb.ToString() 
    
        /// Formating with automatic precision 
        /// e.g.: 0 digits behind comma if above 1000 
        let floatToString  (x:float) =
            if   Double.IsNaN x then "NaN"
            elif x = Double.NegativeInfinity then "Negative Infinity"
            elif x = Double.PositiveInfinity then "Positive Infinity"
            elif x = -1.23432101234321e+308 then "-1.23432e+308 ( = RhinoMath.UnsetValue)" // for https://developer.rhino3d.com/api/RhinoCommon/html/F_Rhino_RhinoMath_UnsetValue.htm
            elif x = 0.0 then "0.0" // not "0" as in sprintf "%g"
            else
                let  a = abs x
                if   a > 10000. then sprintf "%.0f" x |> formatThousands
                elif a > 1000.  then sprintf "%.0f" x
                elif a > 100.   then sprintf "%.1f" x 
                elif a > 10.    then sprintf "%.2f" x 
                elif a > 1.     then sprintf "%.3f" x 
                else                 sprintf "%g"   x  

        /// Formating with automatic precision 
        /// e.g.: 0 digits behind comma if above 1000
        let singleToString  (x:float32) =
            if   Single.IsNaN x then "NaN"
            elif x = Single.NegativeInfinity then "Negative Infinity"
            elif x = Single.PositiveInfinity then "Positive Infinity"            
            elif x = -1.234321e+38f then "-1.2343e+38 ( = RhinoMath.UnsetSingle)" // for https://developer.rhino3d.com/api/RhinoCommon/html/F_Rhino_RhinoMath_UnsetSingle.htm
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

        /// set this to change how deep the content of nested seq is printed (printFull ignores this)
        /// default = 3
        let mutable toNiceStringMaxDepth = 3
    
        /// set this to change how how many items per seq are printed (printFull ignores this)
        /// default = 4
        let mutable toNiceStringMaxItemsPerSeq = 5
        
        /// set this to change how many characters of a string might be printed at onece.
        /// default = 5000
        let mutable maxCharsInString = 5000

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
        let toNiceStringWithFormater (x:'T, externalFormater: obj-> option<string>) = 
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

    //-----------------------
    // functions that are AutoOpened
    //-----------------------
    
    /// prints FsEx.NiceString.toNiceString
    let print x = printfn "%s" (NiceString.toNiceString x)
    
    /// prints two values separated by a space using FsEx.NiceString.toNiceString
    let print2 x y = printfn "%s %s" (NiceString.toNiceString x) (NiceString.toNiceString y)
    
    /// prints three values separated by a space using FsEx.NiceString.toNiceString
    let print3 x y z = printfn "%s %s %s" (NiceString.toNiceString x) (NiceString.toNiceString y) (NiceString.toNiceString z) 
    
    /// prints four values separated by a space using FsEx.NiceString.toNiceString
    let print4 w x y z = printfn "%s %s %s %s" (NiceString.toNiceString w) (NiceString.toNiceString x) (NiceString.toNiceString y) (NiceString.toNiceString z) 

    /// prints FsEx.NiceString.toNiceStringFull
    let printFull x = printfn "%s" (NiceString.toNiceStringFull x)


    /// Exposes functionality of the Seff Editor, when FsEx is loaded there
    type internal Seff private () = // no public constructor  
       
        //static let mutable syncContext: Threading.SynchronizationContext  = null //set via reflection below from Seff.Rhino       
           
        //static let mutable seffWindow : System.Windows.Window = null //set via reflection below from Seff  
           
        static let mutable printColor : int-> int -> int -> string -> unit = //set via reflection below from Seff
            fun r g b s -> printf "%s" s    
           
        static let mutable printnColor : int-> int -> int -> string -> unit = //set via reflection below from Seff
            fun r g b s -> printfn "%s" s

        static let mutable clear : unit -> unit = fun () -> () //set via reflection below from Seff
         
        //static let allAss = AppDomain.CurrentDomain.GetAssemblies()
        
        //static let assemblySeff, assemblyRhinoCommon = 
        //    let ass = AppDomain.CurrentDomain.GetAssemblies()
        //    ass |> Seq.tryFind (fun a -> a.GetName().Name = "Seff") ,
        //    ass |> Seq.tryFind (fun a -> a.GetName().Name = "RhinoCommon") 

        static let mutable doInit = true
        
        static let init()=
            doInit <- false
            let allAss = AppDomain.CurrentDomain.GetAssemblies()
            
            let assemblySeff       =  allAss |> Seq.tryFind (fun a -> a.GetName().Name = "Seff")
            //let assemblyRhinoCommon = allAss |> Seq.tryFind (fun a -> a.GetName().Name = "RhinoCommon") 
            
            match assemblySeff with 
            | Some seffAssembly -> 
                try   
                    let printModule = seffAssembly.GetType("Seff.Model.ISeffLogModule")                    
                    printColor   <- printModule.GetProperty("printColor" ).GetValue(seffAssembly)  :?>  int-> int -> int -> string -> unit
                    printnColor  <- printModule.GetProperty("printnColor").GetValue(seffAssembly)  :?>  int-> int -> int -> string -> unit
                    clear        <- printModule.GetProperty("clear").GetValue(seffAssembly)        :?>  unit -> unit
                with ex ->
                    eprintfn "Failed to get Seff.Model.ISeffLog.printnColor via Reflection, If you are not using the Seff Editor Plugin this is normal. The function 'printfnColor' will behave just as 'printfn'\r\nMessage: %A" ex     
                
            |None -> ()
                //eprintfn "Only found:"
                //AppDomain.CurrentDomain.GetAssemblies()
                //|> Seq.map (fun a -> a.GetName().Name ) 
                //|> Seq.sortBy string
                //|> Seq.iter (eprintfn "%s" )
            
    
        static member PrintColor r g b s = 
            if doInit then init()
            printColor r g b s
        
        static member PrintnColor r g b s = 
            if doInit then init()
            printnColor r g b s

        static member Clear () = 
            if doInit then init()
            clear()

    
    /// print with rgb colors if running in Seff Editor. Else just normal printf 
    /// does NOT add a new line
    /// red -> green -> blue -> string -> unit
    let printfColor red green blue msg =  Printf.kprintf (fun s -> Seff.PrintColor red green blue s)  msg

    /// print with rgb colors if running in Seff Editor. Else just normal printfn
    /// adds a new line at end
    /// red -> green -> blue -> string -> unit
    let printfnColor red green blue msg = Printf.kprintf (fun s -> Seff.PrintnColor red green blue s)  msg

    /// Clears the Seff Log View, 
    /// if it can be found via reflection in loaded assemblies,
    /// else does nothing.
    /// Can be called from any thread.
    let clearSeffLog() = Seff.Clear()
