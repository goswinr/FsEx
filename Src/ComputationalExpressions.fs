namespace FsEx

open System
open System.Text
//open FsEx.SaveIgnore //so that  |> ignore  can only be used on value types

module ComputationalExpressionsBuilders = 
    let mutable csvSepEn = ','
    let mutable csvSepDe = ';'

    let inline private addchr     (b: StringBuilder) (c:char)   = b.Append      c                                            |> ignore<StringBuilder>
    let inline private add        (b: StringBuilder) (s:string) = b.Append      s                                            |> ignore<StringBuilder>
    let inline private addLn      (b: StringBuilder) (s:string) = b.AppendLine  s                                            |> ignore<StringBuilder>
    let inline private addCsvEn   (b: StringBuilder) (s:string) = b.Append(s).Append(csvSepEn)                               |> ignore<StringBuilder>
    let inline private addCsvEnLn (b: StringBuilder) (s:string) = b.Append(s).Append(csvSepEn).Append(Environment.NewLine)   |> ignore<StringBuilder>
    let inline private addCsvDe   (b: StringBuilder) (s:string) = b.Append(s).Append(csvSepDe)                               |> ignore<StringBuilder>
    let inline private addCsvDeLn (b: StringBuilder) (s:string) = b.Append(s).Append(csvSepDe).Append(Environment.NewLine)   |> ignore<StringBuilder>

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
            do f() |> ignore<StringBuilder>
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

        member inline _.Yield (strings: seq<string>) = 
            fun (b: StringBuilder) ->
                for s in strings do
                    addLn b s

        member inline _.Combine (f, g) = fun (b: StringBuilder) -> f b; g b

        member inline _.Delay f = fun (b: StringBuilder) -> (f()) b

        member inline _.Zero () = ignore

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


    type CsvBuilderEN () = 

           member inline _.Yield (txt: string) =  fun (b: StringBuilder) ->  addCsvEn b txt
           member inline _.Yield (c: char) =      fun (b: StringBuilder) ->  addCsvEn b (c.ToString())
           member inline _.Yield (f: float) =     fun (b: StringBuilder) ->  addCsvEn b (f.AsString)
           member inline _.Yield (i: int) =       fun (b: StringBuilder)  -> addCsvEn b (i.ToString())
           member inline _.Yield (g: Guid) =      fun (b: StringBuilder)  -> addCsvEn b (g.ToString())
           //member inline _.Yield (x: 'T) =        fun (b: StringBuilder)  -> b.Append (x.ToString())  |> ignore

           member inline _.YieldFrom (txt: string) =  fun (b: StringBuilder) -> addCsvEnLn b txt
           member inline _.YieldFrom (c: char) =      fun (b: StringBuilder) -> addCsvEnLn b (c.ToString())
           member inline _.YieldFrom (f: float) =     fun (b: StringBuilder) -> addCsvEnLn b (f.AsString)
           member inline _.YieldFrom (i: int) =       fun (b: StringBuilder) -> addCsvEnLn b (i.ToString())
           member inline _.YieldFrom (g: Guid) =      fun (b: StringBuilder) -> addCsvEnLn b (g.ToString())
           member inline _.YieldFrom () =             fun (b: StringBuilder) -> b.AppendLine()

           member inline _.Combine (f, g) = fun (b: StringBuilder) -> f b; g b

           member inline _.Delay f = fun (b: StringBuilder) -> (f()) b

           member inline _.Zero () = ignore

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

    type CsvBuilderDE () = 

           member inline _.Yield (txt: string) =  fun (b: StringBuilder) ->  addCsvDe b txt
           member inline _.Yield (c: char) =      fun (b: StringBuilder) ->  addCsvDe b (c.ToString())
           member inline _.Yield (f: float) =     fun (b: StringBuilder) ->  addCsvDe b (f.AsStringDE)
           member inline _.Yield (f: single) =    fun (b: StringBuilder) ->  addCsvDe b (f.AsStringDE)
           member inline _.Yield (f: decimal) =   fun (b: StringBuilder) ->  addCsvDe b (f.AsStringDE)
           member inline _.Yield (i: int) =       fun (b: StringBuilder)  -> addCsvDe b (i.ToString())
           member inline _.Yield (g: Guid) =      fun (b: StringBuilder)  -> addCsvDe b (g.ToString())
           //member inline _.Yield (x: 'T) =        fun (b: StringBuilder)  -> b.Append (x.ToString())  |> ignore

           member inline _.YieldFrom (txt: string) =  fun (b: StringBuilder) -> addCsvDeLn b txt
           member inline _.YieldFrom (c: char) =      fun (b: StringBuilder) -> addCsvDeLn b (c.ToString())
           member inline _.YieldFrom (f: float) =     fun (b: StringBuilder) -> addCsvDeLn b (f.AsStringDE)
           member inline _.YieldFrom (f: single) =    fun (b: StringBuilder) -> addCsvDeLn b (f.AsStringDE)
           member inline _.YieldFrom (f: decimal) =   fun (b: StringBuilder) -> addCsvDeLn b (f.AsStringDE)
           member inline _.YieldFrom (i: int) =       fun (b: StringBuilder) -> addCsvDeLn b (i.ToString())
           member inline _.YieldFrom (g: Guid) =      fun (b: StringBuilder) -> addCsvDeLn b (g.ToString())
           member inline _.YieldFrom () =             fun (b: StringBuilder) -> b.AppendLine()

           member inline _.Combine (f, g) = fun (b: StringBuilder) -> f b; g b

           member inline _.Delay f = fun (b: StringBuilder) -> (f()) b

           member inline _.Zero () = ignore

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

        member inline _.Zero () =  ignore

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
        (*
        TODO
        https://fsharpforfunandprofit.com/posts/computation-expressions-builder-part6/
        member this.While(guard, body) =
            if not (guard())
            then this.Zero()
            else this.Bind( body(), fun () ->
                this.While(guard, body))

        member this.TryWith(body, handler) =
            try this.ReturnFrom(body())
            with e -> handler e

        member this.TryFinally(body, compensation) =
            try this.ReturnFrom(body())
            finally compensation()

        member this.Using(disposable:#System.IDisposable, body) =
            let body' = fun () -> body disposable
            this.TryFinally(body', fun () ->
                match disposable with
                    | null -> ()
                    | disp -> disp.Dispose())
        *)


[<AutoOpen>]
module AutoOpenComputationalExpressions  = 
    open ComputationalExpressionsBuilders

    /// A maybe monad for option types.
    let maybe = MaybeBuilder()

    /// Computational Expression:
    /// use 'yield' to append text
    /// and 'yield!' (with an exclamation mark)  to append text followed by a new line character.
    /// accepts ints and floats too. (including nice Formating via NiceString.floatToString )
    let stringBuffer = StringBufferBuilder ()

    /// Computational Expression for making csv files in English culture:
    /// use 'yield' to append text and a  subsequent comma
    /// and 'yield!' (with an exclamation mark)  to append text followed by a new line character.
    /// accepts ints and floats too. (floats are printed in full length using f.AsString)
    let csvEN = CsvBuilderEN ()


    /// Computational Expression for making csv files German culture:
    /// use 'yield' to append text and a  subsequent semicolon
    /// and 'yield!' (with an exclamation mark)  to append text followed by a new line character.
    /// accepts ints and floats too. (floats are printed in full length using f.AsStringDE)
    let csvDE = CsvBuilderDE ()


    /// Computational Expression:  use 'yield' to add elements to a Rarr (= Collections.Generic.List).
    let rarr<'T> = new RarrBuilder<'T> ()





