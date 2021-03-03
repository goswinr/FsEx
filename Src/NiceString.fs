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
    let truncate (stringToTrim:string) =
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
    let typeName nameSpace name = 
        let name = name |> before '`' |> before '@' 
        let fullPath = nameSpace  + "." + name 
        if fullPath.StartsWith "System." then 
            fullPath.Substring(7)
        else 
            fullPath.Replace("Microsoft.FSharp.Collections.mkSeq","seq")
    


module internal NiceStringImplementation  =
    open System.Collections.Generic 
    open NiceStringSettings    
    
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
                    sprintf "%s with %d items of %s:" name xs.Count elName
                |None -> sprintf "%s with %d items:"  name xs.Count
            |None ->     sprintf "%s with %d items:"  name xs.Count
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
                    |More    i -> sprintf "%s with more than %d items of %s:" name i elName
                    |Counted i -> sprintf "%s with %d items of %s:" name i elName
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
            | :? string     as s   -> NiceFormat.truncate  s  |> Element    // is  in quotes, s.ToString() also adds a " at start and end
            | :? Guid       as g   -> sprintf "Guid[%O]" g    |> Element 
            | :? Collections.ICollection as xs -> getCollection depth x xs
            | :? Collections.IEnumerable as xs -> getSeq depth x xs
            | _ ->  sprintf "%A" x |> Element
    

    let formatLines (lines:Lines) = 
        let sb = Text.StringBuilder()        
        let rec loop depth (lns:Lines) = 
            if depth <= maxDepth then 
                match lns with
                |Element s    -> sb.Append(String(' ', 4 * depth)).AppendLine(s)      |> ignoreObj 
                |EarlyEnd     -> sb.Append(String(' ', 4 * depth)).AppendLine("...")  |> ignoreObj  
                |Head (h,xs)  -> 
                                 sb.Append(String(' ', 4 * depth)).AppendLine(h)      |> ignoreObj  
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


    /// Nice formating for floats , some Rhino Objects and sequences of any kind, first four items are printed out.
    /// Settings are exposed in NiceString.NiceStringSettings
    /// • thousandSeparator          = '\'' (this is just one quote: ')  ; set this to change the printing of floats and integers larger than 10'000
    /// • toNiceStringMaxDepth       = 3                                 ; set this to change how deep the content of nested seq is printed (printFull ignores this)
    /// • toNiceStringMaxItemsPerSeq = 5                                 ; set this to change how how many items per seq are printed (printFull ignores this)
    /// • maxCharsInString          = 5000                              ;set this to change how many characters of a string might be printed at onece.    
    let toNiceString (x:'T) = 
        x |> box |> getLines 0 |> formatLines


    /// Nice formating for floats and numbers in including thousand Separator ,  all items including nested items are printed out.
    /// Settings are exposed in NiceString.NiceStringSettings
    /// • thousandSeparator          = '\'' (this is just one quote: ')  ; set this to change the printing of floats and integers larger than 10'000
    /// • maxCharsInString          = 5000                              ;set this to change how many characters of a string might be printed at onece.
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

       


module StringMakerCombinators = 
    //adapted from "C:\work\Goswin\30_F#\01_Books\FSharpForTechnicalComputing\F#.NET Journal\000_All html text only\genericprettyprinting.html"

    (*
    Revisiting combinators
    In fact, the built-in StringBuilder class is not the most efficient solution. A more efficient solution is to write directly into a char array.
    However, this introduces the problem of choosing an array large enough or resizing the array when it is too small. Our tests indicate that 
    precomputing the size of the array for each message is too slow, accounting for almost half of the total running time of the pretty printer. 
    An efficient alternative is to start pretty printing into a static array and resize it when the out of bounds exception is thrown.

    We begin by writing combinators that pretty print into a given char array. The following set1Char combinator writes a single character 
    into the array cs at the current index i: *)

    let inline set1Char (cs: char []) i c =
        cs.[i] <- c
        i+1    

    (* Note that we are making judicious use of inline in the interests of performance. This is beneficial not only because it avoids the 
    overhead of function calls, as in other languages, but also because it causes generic functions to be type specialized when they are 
    inlined to locations that have statically-known types.
    The following set2Chars function emits two characters:    *)

    let inline set2Chars (cs: char []) i c0 c1 =
        cs.[i] <- c0
        cs.[i+1] <- c1
        i+2

    (*The following setSep function emits "; " and will be used to setSeparate elements in lists and arrays:    *)
    let inline setSep cs i =
        set2Chars cs i ';' ' '

    let inline setStr cs i (s:string) =
        let mutable ii = i
        for is=0 to s.Length-1 do
            ii <- set1Char cs ii s.[is]
        ii

    
    // The following setInt function emits the decimal representation of an int with a preceding - sign if it is negative:  *)
    let inline setInt cs i num =
        let rec setIntR ii n =
          if n<10 then set1Char cs ii (char(48 + n)) 
          else         set1Char cs (setIntR ii (n/10)) (char (48 + n%10))
        if num<0 then setIntR (set1Char cs i '-') -num else setIntR i num
    (* Note our style of mutating the array elements in place but accumulating the immutable int index i. Although it is tempting to try to use a mutable 
    index it is cumbersome and less efficient because the program ends up passing a pointer to the stack allocated int instead of the int itself even 
    though both are the same size. *)

    let inline pow10 d = 
        match d with
        | 1uy -> 1e1 | 2uy -> 1e2 | 3uy -> 1e3
        | 4uy -> 1e4 | 5uy -> 1e5 | 6uy -> 1e6 
        | 7uy -> 1e7 | 8uy -> 1e8 | 0uy -> 0.0 
        | d -> float(pown d 10)     

    let inline setFloat sep digits cs i num  =
          
        let rec setIntR ii n =
            if n<10 then set1Char cs ii (char(48 + n)) 
            else         set1Char cs (setIntR ii (n/10)) (char (48 + n%10)) 
        
        let rec setFloatR ii n dig =            
            if dig = digits then
                set1Char cs (setIntR ii n) sep // but not : setIntR (set1Char cs ii sep) n            
            else            
                set1Char cs (setFloatR ii (n/10) (dig+1uy) ) (char (48 + n%10)) 
        
        if   Double.IsNaN num              then setStr cs i "NaN"
        elif Double.IsPositiveInfinity num then setStr cs i "+Infinity"
        elif Double.IsNegativeInfinity num then setStr cs i "-Infinity"    
        elif num > 0.0 then 
            if digits = 0uy then setIntR   i (int(num+0.5)) // midpoint always up rounding
            else                 setFloatR i (int(num*(pow10 digits)+0.5)) 0uy
        else
            if digits = 0uy then setIntR   (set1Char cs i '-') (int(-num+0.5))
            else                 setFloatR (set1Char cs i '-') (int(-num*(pow10 digits)+0.5)) 0uy
    
    
    //Now for the generic combinators. The following setList combinator emits a list in F# syntax using the given function fun1Char to emit each element:    *)

    let inline setList fun1Char cs i xs =
        let rec setListR i = function
          | [] -> i
          | [x] -> fun1Char cs i x
          | x::xs -> setListR (setSep cs (fun1Char cs i x)) xs
        set1Char cs (setListR (set1Char cs i '[') xs) ']'
    
    (* 
        Note how elegantly the accumulation of the index i allows us to compose the calls to list, setSep and fun1Char inside the pattern match 
        at the core of the list function. This is both elegant and predictably efficient thanks to the carefully-crafted characteristics 
        of the F# language.

        Similarly, the following setArray function pretty prints an array:    *)

    let inline setArray fun1Char cs i (xs: _ []) =
        if xs.Length=0 then set2Chars cs (set2Chars cs i '[' '|') '|' ']' else
        let mutable i = fun1Char cs (set2Chars cs i '[' '|') xs.[0]
        for ii=1 to xs.Length-1 do
          i <- fun1Char cs (setSep cs i) xs.[ii]
        set2Chars cs i '|' ']'
    
    (* 
        Now the problem turns to allocating an array large enough to fit the message. The simplest solution is to simply preallocate an 
        array believed to be large enough but we want to handle the problem of overflow for our users. One possible solution is to 
        precompute the length of array that will be needed but this turns out to be almost as time consuming as the pretty printing 
        itself. Another solution would be to increase the length of the array in the event of an overflow (appearing as an out-of-bounds 
        exception). However, starting with a small array and doubling its length until it is long enough for each message is also too 
        slow because exceptions are very slow on .NET:    
        do
            for i=1 to 1000 do
              try
                failwith "Foo"
              with _ -> ()

        This shows that raising and catching a single exception takes 53Âµs, which is substantially slower than pretty printing our message. 
        Therefore, we cannot afford to have even a single exception thrown for each message. However, we can reuse the same preallocated 
        array for setSeparate messages in order to amortize this cost across many calls to the pretty printer.

        The following print function preallocates an array cs each time it is invoked with a combinator fun1Char and then loops trying to pretty 
        print into the preallocated array and doubling its length if there is a failure: *)

    let print fun1Char =
        let cs = ref [|' '|]
        fun x ->
          let rec loop() =
            try
              String(!cs, 0, fun1Char !cs 0 x)
            with _ ->
              cs := Array.append !cs !cs
              loop()
          loop()
    
    (* val print : (char [] -> int -> 'a -> int) -> ('a -> System.String)

        This print function can be used to pretty print our example value as follows:

        print (setArray (setList setInt)) xs
         val it : System.String = "[|[1]; [2]; [3]|]"

        This is by far the fastest solution so far and it is completely generic:

        do
            let printIntListArray = print (setArray (setList setInt))
            for i=1 to 1000000 do
              printIntListArray xs
              |> ignore

        These results show that a single call now takes just 0.78Âµs which is 7,600× faster than the built-in sprintf "%A".
        Summary

        This article has examined a variety of techniques for generic pretty printing including the built-in sprintf "%A", combinators and reflection. 
        Using sprintf "%A" is easy but extremely inefficient at only 170 calls per second. Once a reflection-based solution has been defined it is 
        almost as easy to use and performance is substantially better, allowing up to 40,000 calls per second. Combinators accumulating into a 
        StringBuffer were slightly more cumbersome to use and ran at up to 210,000 calls per second. The fastest generic solution used inline 
        combinators to fill a char array and ran at up to 1,300,000 calls per second.

        Future F#.NET Journal articles will revisit the subjects of combinators, reflection and the related subject of serialization. *)


    /// StartOp, strOp, intOp, floatOp, EndOp:
    /// let ( !+ ),( ++ ),( ++* ),( ++. ),( +! )  = getFunctions '.' "; " 2uy
    let inline getFunctions decimalSep csvSep digits =
        let cs = ref [|' '|]
        let resize () = 
            cs := Array.append !cs !cs
            //printfn "* Char Arr resized !"
        //let sb = Text.StringBuilder()    use this for strings ??     
        
        let st =    fun s -> // Start op: !> or use '0+&'
                        let rec loop() =
                            try   setStr !cs 0 s
                            with | :? IndexOutOfRangeException -> resize () ; loop() | e -> raise e
                        loop() 
         
        let en =   fun i () -> // Stop op <!()
                        let rec loop() =
                            try  String(!cs, 0, i)
                            with | :? IndexOutOfRangeException -> resize () ; loop() | e -> raise e
                        loop()         

        if csvSep = "" then // do not call for separator if ther is none
            st ,
            (fun i s-> // add string op +&
                let rec loop() =
                    try   setStr !cs i s
                    with | :? IndexOutOfRangeException -> resize () ; loop() | e -> raise e
                loop() ), 
            
            (fun i v-> // add int op +&+
                let rec loop() =
                    try   setInt !cs i v
                    with | :? IndexOutOfRangeException -> resize () ; loop() | e -> raise e
                loop() ),
                        
            (fun i v-> // add double op +&*
                let rec loop() =
                    try   setFloat decimalSep digits !cs i v
                    with | :? IndexOutOfRangeException -> resize () ; loop() | e -> raise e
                loop() ),
            en
        else
            st,
            (fun i s-> // add string op +&
                let rec loop() =
                    try   setStr !cs (setStr !cs i csvSep) s
                    with | :? IndexOutOfRangeException -> resize () ; loop() | e -> raise e
                loop() ), 
            
            (fun i v-> // add int op +&+
                let rec loop() =
                    try   setInt !cs (setStr !cs i csvSep) v
                    with | :? IndexOutOfRangeException -> resize () ; loop() | e -> raise e
                loop() ),
                        
            (fun i v-> // add double op +&*
                let rec loop() =
                    try   setFloat decimalSep digits !cs (setStr !cs i csvSep) v
                    with | :? IndexOutOfRangeException -> resize () ; loop() | e -> raise e
                loop() ),
            en

    
    //let ( !+ ),( ++ ),( ++* ),( ++. ),( +! ) = getFunctions '.' "; " 6uy  // #nowarn "86" // for overwriting '&' operator


    //let x = !+ "abcde" ++* -9 ++ "xx"   ++. -92.03443210 +!()

    //let x = !+ "abcde"  ++  "--x" ++. -12.341235 ++* 528 +!()
              
