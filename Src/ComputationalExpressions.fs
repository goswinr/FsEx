namespace FsEx

open System
open System.Text
//open FsEx.SaveIgnore //so that  |> ignore  can only be used on value types

module ComputationalExpressionsBuilders = 

    let inline private addChr   (b: StringBuilder) (c:char)   = b.Append      c   |> ignore<StringBuilder>
    let inline private addLnChr (b: StringBuilder) (c:char)   = b.Append(c).AppendLine()   |> ignore<StringBuilder>
    let inline private add      (b: StringBuilder) (s:string) = b.Append      s   |> ignore<StringBuilder>
    let inline private addLn    (b: StringBuilder) (s:string) = b.AppendLine  s   |> ignore<StringBuilder>
    
    /// The maybe monad.
    type MaybeBuilder() = 
        // from https://github.com/fsprojects/FSharpx.Extras/blob/master/src/FSharpx.Extras/ComputationExpressions/Option.fs
        // This monad is my own and uses an 'T option. Others generally make their own Maybe<'T> type from Option<'T>.
        // The builder approach is from Matthew Podwysocki's excellent Creating Extended Builders series
        // http://codebetter.com/blogs/matthew.podwysocki/archive/2010/01/18/much-ado-about-monads-creating-extended-builders.aspx.

        member inline this.Return(x) = Some x

        member inline this.ReturnFrom(m: option<'T>) = m

        member inline this.Bind(m, f) = Option.bind f m

        member inline this.Zero() = None

        member inline this.Combine(m, f) = Option.bind f m

        member inline this.Delay(f: unit -> _) = f

        member inline this.Run(f) = f()

        member inline this.TryWith(m, h) = 
            try this.ReturnFrom(m)
            with e -> h e

        member inline  this.TryFinally(m, compensation) = 
            try this.ReturnFrom(m)
            finally compensation()

        member inline this.Using(res:#IDisposable, body) = 
            this.TryFinally(body res, fun () -> match res with null -> () | disp -> disp.Dispose())

        member this.While(guard, f) = 
            if not (guard()) then Some () else
            do f() |> ignore<StringBuilder>
            this.While(guard, f)

        member inline  this.For(sequence:seq<_>, body) = 
            this.Using(sequence.GetEnumerator(), fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current)))


    type StringBufferBuilder () = 
        // adapted from https://github.com/fsharp/fslang-suggestions/issues/775

        member inline _.Yield (c: char) =      fun (b: StringBuilder) ->  addChr b c
        member inline _.Yield (txt: string) =  fun (b: StringBuilder) ->  add b txt        
        member inline _.Yield (i: int) =       fun (b: StringBuilder)  -> add b (i.ToString())
        member inline _.Yield (g: Guid) =      fun (b: StringBuilder)  -> add b (g.ToString())        

        member inline _.YieldFrom (txt: string) =  fun (b: StringBuilder) -> addLn b txt
        member inline _.YieldFrom (c: char) =      fun (b: StringBuilder) -> addLnChr b c
        member inline _.YieldFrom (i: int) =       fun (b: StringBuilder) -> addLn b (i.ToString())
        member inline _.YieldFrom (g: Guid) =      fun (b: StringBuilder) -> addLn b (g.ToString())

        member inline _.Yield (strings: seq<string>) = 
            fun (b: StringBuilder) -> for s in strings do  addLn b s

        member inline _.Combine (f, g) = fun (b: StringBuilder) -> f b; g b

        member inline _.Delay f = fun (b: StringBuilder) -> (f()) b

        member inline _.Zero () = ignore

        member inline _.For (xs: 'T seq, f: 'T -> StringBuilder -> unit) = 
            fun (b: StringBuilder) ->
                use e = xs.GetEnumerator ()
                while e.MoveNext() do  (f e.Current) b

        member inline _.While (p: unit -> bool, f: StringBuilder -> unit) = 
            fun (b: StringBuilder) -> 
                while p () do f b

        member inline _.Run (f: StringBuilder -> unit) = 
            let b = StringBuilder()
            do f b
            b.ToString()

        member inline  _.TryWith(body: StringBuilder -> unit, handler: exn ->  StringBuilder -> unit) =
            fun (b: StringBuilder) -> 
                try body b with e -> handler e b

        member inline  _.TryFinally(body: StringBuilder -> unit, compensation:  StringBuilder -> unit) =
            fun (b: StringBuilder) ->  
                try body b finally compensation  b

        member inline this.Using(disposable: #IDisposable, body: #IDisposable -> StringBuilder -> unit) =            
            this.TryFinally(  body disposable ,  fun (b: StringBuilder)  ->  if not <| Object.ReferenceEquals(disposable,null) then disposable.Dispose() ) // might be disposed already                        
     
 
[<AutoOpen>]
module AutoOpenComputationalExpressions  = 
    open ComputationalExpressionsBuilders

    /// A maybe monad for option types.
    let maybe = MaybeBuilder()

    /// Computational Expression String Builder:
    /// use 'yield' to append text, or seq of strings separated by a new line
    /// and 'yield!' (with an exclamation mark)  to append text followed by a new line character.
    /// accepts ints, guids and chars too.
    [<Obsolete("Use the short name of this builder via str{} ")>]
    let stringBuffer = StringBufferBuilder ()

    /// Computational Expression String Builder:
    /// use 'yield' to append text, or seq of strings separated by a new line
    /// and 'yield!' (with an exclamation mark)  to append text followed by a new line character.
    /// accepts ints, guids and chars  too. 
    let str = StringBufferBuilder ()





