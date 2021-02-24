namespace FsEx
open System
open FsEx.SaveIgnore //so that  |> ignore  can only be used on value types
open System.Text

module ComputationalExpressionsBuilders =    
    let mutable csvSep = ';'
    
    let inline private addchr   (b: StringBuilder) (c:char)   = b.Append      c                                          |> ignoreObj
    let inline private add      (b: StringBuilder) (s:string) = b.Append      s                                          |> ignoreObj
    let inline private addLn    (b: StringBuilder) (s:string) = b.AppendLine  s                                          |> ignoreObj
    let inline private addCsv   (b: StringBuilder) (s:string) = b.Append(s).Append(csvSep)                               |> ignoreObj
    let inline private addCsvLn (b: StringBuilder) (s:string) = b.Append(s).Append(csvSep).Append(Environment.NewLine)   |> ignoreObj


    /// The maybe monad. 
    type MaybeBuilder() =
        // from https://github.com/fsprojects/FSharpx.Extras/blob/master/src/FSharpx.Extras/ComputationExpressions/Option.fs
        // This monad is my own and uses an 'T option. Others generally make their own Maybe<'T> type from Option<'T>.
        // The builder approach is from Matthew Podwysocki's excellent Creating Extended Builders series 
        // http://codebetter.com/blogs/matthew.podwysocki/archive/2010/01/18/much-ado-about-monads-creating-extended-builders.aspx.        
        
        member this.Return(x) = Some x

        member this.ReturnFrom(m: 'T option) = m

        member this.Bind(m, f) = Option.bind f m

        member this.Zero() = None

        member this.Combine(m, f) = Option.bind f m

        member this.Delay(f: unit -> _) = f

        member this.Run(f) = f()

        member this.TryWith(m, h) =
            try this.ReturnFrom(m)
            with e -> h e

        member this.TryFinally(m, compensation) =
            try this.ReturnFrom(m)
            finally compensation()

        member this.Using(res:#IDisposable, body) =
            this.TryFinally(body res, fun () -> match res with null -> () | disp -> disp.Dispose())

        member this.While(guard, f) =
            if not (guard()) then Some () else
            do f() |> ignoreObj
            this.While(guard, f)

        member this.For(sequence:seq<_>, body) =
            this.Using(sequence.GetEnumerator(), fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current)))
    

    type StringBufferBuilder () = 
        // adapted from https://github.com/fsharp/fslang-suggestions/issues/775

        member inline _.Yield (c: char) =      fun (b: StringBuilder) ->  addchr b c 
        member inline _.Yield (txt: string) =  fun (b: StringBuilder) ->  add b txt 
        member inline _.Yield (f: float) =     fun (b: StringBuilder) ->  add b (f.AsString) // thousand separators not desired ? (NiceString.Floats.floatToString f)
        member inline _.Yield (i: int) =       fun (b: StringBuilder)  -> add b (i.ToString())  
        member inline _.Yield (g: Guid) =      fun (b: StringBuilder)  -> add b (g.ToString())  
        //member inline _.Yield (x: 'T) =        fun (b: StringBuilder)  -> b.Append (x.ToString())  |> ignore

        member inline _.YieldFrom (txt: string) =  fun (b: StringBuilder) -> addLn b txt 
        member inline _.YieldFrom (c: char) =      fun (b: StringBuilder) -> addLn b (c.ToString())   
        member inline _.YieldFrom (f: float) =     fun (b: StringBuilder) -> addLn b (f.AsString) // thousand separators not desired ? (NiceString.Floats.floatToString f)
        member inline _.YieldFrom (i: int) =       fun (b: StringBuilder) -> addLn b (i.ToString())  
        member inline _.YieldFrom (g: Guid) =      fun (b: StringBuilder) -> addLn b (g.ToString())  

        //member inline _.YieldFrom (f: StringBuffer) = f // use for new line instead
        
        member inline _.Yield (strings: seq<string>) =
            fun (b: StringBuilder) -> 
                for s in strings do 
                    addLn b s      
        
        member inline _.Combine (f, g) = fun (b: StringBuilder) -> f b; g b
        
        member inline _.Delay f = fun (b: StringBuilder) -> (f()) b
        
        member inline _.Zero () = ignoreObj
        
        member inline _.For (xs: 'T seq, f: 'T -> StringBuilder -> unit) =
            fun (b: StringBuilder) ->
                use e = xs.GetEnumerator ()
                while e.MoveNext() do
                    (f e.Current) b
        
        member inline _.While (p: unit -> bool, f: StringBuilder -> unit) =
            fun (b: StringBuilder) -> 
                while p () do 
                    f b
            
        member inline _.Run (f: StringBuilder -> unit) =
            let b = StringBuilder()
            do f b
            b.ToString()
    

    type CsvBuilder () = 
           
           member inline _.Yield (txt: string) =  fun (b: StringBuilder) ->  addCsv b txt 
           member inline _.Yield (c: char) =      fun (b: StringBuilder) ->  addCsv b (c.ToString())  
           member inline _.Yield (f: float) =     fun (b: StringBuilder) ->  addCsv b (f.AsString)
           member inline _.Yield (i: int) =       fun (b: StringBuilder)  -> addCsv b (i.ToString())  
           member inline _.Yield (g: Guid) =      fun (b: StringBuilder)  -> addCsv b (g.ToString())  
           //member inline _.Yield (x: 'T) =        fun (b: StringBuilder)  -> b.Append (x.ToString())  |> ignore

           member inline _.YieldFrom (txt: string) =  fun (b: StringBuilder) -> addCsvLn b txt 
           member inline _.YieldFrom (c: char) =      fun (b: StringBuilder) -> addCsvLn b (c.ToString())   
           member inline _.YieldFrom (f: float) =     fun (b: StringBuilder) -> addCsvLn b (f.AsString)
           member inline _.YieldFrom (i: int) =       fun (b: StringBuilder) -> addCsvLn b (i.ToString())  
           member inline _.YieldFrom (g: Guid) =      fun (b: StringBuilder) -> addCsvLn b (g.ToString()) 
                     
           member inline _.Combine (f, g) = fun (b: StringBuilder) -> f b; g b
           
           member inline _.Delay f = fun (b: StringBuilder) -> (f()) b
           
           member inline _.Zero () = ignoreObj
           
           member inline _.For (xs: 'T seq, f: 'T -> StringBuilder -> unit) =
               fun (b: StringBuilder) ->
                   use e = xs.GetEnumerator ()
                   while e.MoveNext() do
                       (f e.Current) b
           
           member inline _.While (p: unit -> bool, f: StringBuilder -> unit) =
               fun (b: StringBuilder) -> 
                   while p () do 
                       f b
               
           member inline _.Run (f: StringBuilder -> unit) =
               let b = StringBuilder()
               do f b
               b.ToString()
       

    type RarrBuilder<'T> () =
        member inline _.Yield (x: 'T) =  
            fun (r: Rarr<'T>) -> r.Add(x)      
        
        member inline _.YieldFrom (xs: #seq<'T>) =
            fun (r: Rarr<'T>) -> r.AddRange(xs) 
        
        member inline _.Combine (f, g) = 
            fun (r: Rarr<'T>) -> f r; g r
        
        member inline _.Delay f = 
            fun (r: Rarr<'T>) -> (f()) r
        
        member inline _.Zero () =  ignoreObj
        
        member inline _.For (xs: 'U seq, f: 'U -> Rarr<'T> -> unit) =
            fun (r: Rarr<'T>) ->
                use e = xs.GetEnumerator()
                while e.MoveNext() do
                    (f e.Current) r
        
        member inline _.While (p: unit -> bool, f: Rarr<'T> -> unit) =
            fun (r: Rarr<'T>) -> while p () do  f r
            
        member inline _.Run (f: Rarr<'T> -> unit) =
            let r = Rarr<'T>()
            do f r
            r  
  
  
[<AutoOpen>]
module ComputationalExpressions  =
    open ComputationalExpressionsBuilders 
    
    /// A maybe monad for option types. 
    let maybe = MaybeBuilder()

    /// Computational Expression:  
    /// use 'yield' to append text
    /// and 'yield!' (with an exclamation mark)  to append text followed by a new line character.
    /// accepts ints and floats too. (including nice Formating via NiceString.floatToString )
    let stringBuffer = StringBufferBuilder () 
     
    /// Computational Expression for making csv files:  
    /// use 'yield' to append text and a  subsequent semicolon
    /// and 'yield!' (with an exclamation mark)  to append text followed by a new line character.
    /// accepts ints and floats too. (including nice Formating via NiceString.floatToString )
    let csv = CsvBuilder ()

    /// Computational Expression:  use 'yield' to add alements to a Rarr (= Collections.Generic.List).
    let rarr<'T> = new RarrBuilder<'T> ()
 

    

   