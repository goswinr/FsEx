namespace FsEx
open System
open FsEx.SaveIgnore

[<AutoOpen>]
module MaybeBuilder =    

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
            this.Using(sequence.GetEnumerator(),
                                 fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current)))
    
    /// A maybe monad. 
    let maybe = MaybeBuilder()

[<AutoOpen>]
module StringBufferBuilder =    
    // adapted from https://github.com/fsharp/fslang-suggestions/issues/775

    open System.Text

    type StringBufferBuilder () = 
        
        member inline _.Yield (txt: string) =  fun (b: StringBuilder) -> b.Append  txt |> ignoreObj
        member inline _.Yield (c: char) =      fun (b: StringBuilder) -> b.Append  c   |> ignoreObj
        member inline _.Yield (f: float) =     fun (b: StringBuilder) -> f |> NiceString.floatToString |> b.Append  |> ignoreObj
        member inline _.Yield (i: int) =       fun (b: StringBuilder)  -> b.Append (i.ToString())  |> ignoreObj
        //member inline _.Yield (x: 'T) =        fun (b: StringBuilder)  -> b.Append (x.ToString())  |> ignore

        member inline _.YieldFrom (txt: string) =  fun (b: StringBuilder) -> b.AppendLine txt |> ignoreObj // 
        member inline _.YieldFrom (c: char) =      fun (b: StringBuilder) -> b.AppendLine  (c.ToString())   |> ignoreObj
        member inline _.YieldFrom (f: float) =     fun (b: StringBuilder) -> f |> NiceString.floatToString |> b.AppendLine  |> ignoreObj
        member inline _.YieldFrom (i: int) =       fun (b: StringBuilder)  -> b.AppendLine (i.ToString())  |> ignoreObj
        //member inline _.YieldFrom (f: StringBuffer) = f // use for new line instead
        
        member inline _.Yield (strings: seq<string>) =
            fun (b: StringBuilder) -> 
                for s in strings do 
                    Printf.bprintf b "%s%s" s Environment.NewLine         
        
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
    
    /// Computational Expression:  
    /// use 'yield' to append text
    /// and 'yield!' (with an exclamation mark)  to append text followed by a new line character.
    /// accepts ints and floats too. (including nice Formating)
    let stringBuffer = StringBufferBuilder ()



[<AutoOpen>]
module ResizeArrayBuilder =

    type ResizeArrayBuilder<'T> () =
        member inline _.Yield (x: 'T) =  
            fun (r: ResizeArray<'T>) -> r.Add(x)      
        
        member inline _.YieldFrom (xs: #seq<'T>) =
            fun (r: ResizeArray<'T>) -> r.AddRange(xs) 
        
        member inline _.Combine (f, g) = 
            fun (r: ResizeArray<'T>) -> f r; g r
        
        member inline _.Delay f = 
            fun (r: ResizeArray<'T>) -> (f()) r
        
        member inline _.Zero () =  ignoreObj
        
        member inline _.For (xs: 'U seq, f: 'U -> ResizeArray<'T> -> unit) =
            fun (r: ResizeArray<'T>) ->
                use e = xs.GetEnumerator()
                while e.MoveNext() do
                    (f e.Current) r
        
        member inline _.While (p: unit -> bool, f: ResizeArray<'T> -> unit) =
            fun (r: ResizeArray<'T>) -> while p () do  f r
            
        member inline _.Run (f: ResizeArray<'T> -> unit) =
            let r = ResizeArray<'T>()
            do f r
            r  
    
    /// Computational Expression:  use 'yield' to add alements to a ResizeArray (= Collections.Generic.List).
    let resizeArray<'T> = new ResizeArrayBuilder<'T> ()


   

[<AutoOpen>]
module RarrBuilder =

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
    
    /// Computational Expression:  use 'yield' to add alements to a Rarr (= Collections.Generic.List).
    let rarr<'T> = new RarrBuilder<'T> ()
    