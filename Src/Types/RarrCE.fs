namespace FsEx

open System

module ComputationalExpressionsRarr =

    //TODO: optimize with 
    // [<InlineIfLambda>] as in https://gist.github.com/Tarmil/afcf5f50e45e90200eb7b01615b0ffc0
    // or https://github.com/fsharp/fslang-design/blob/main/FSharp-6.0/FS-1099-list-collector.md

    type RarrBuilder<'T> () = 
        member inline _.Yield (x: 'T) = 
            fun (r: Rarr<'T>) -> 
                r.Add(x)

        member inline _.YieldFrom (xs: #seq<'T>) = 
            fun (r: Rarr<'T>) -> 
                r.AddRange(xs)

        member inline _.Combine (f, g) = 
            fun (r: Rarr<'T>) -> 
                f r; 
                g r

        member inline _.Delay f = 
            fun (r: Rarr<'T>) -> (f()) r

        member inline _.Zero () =  
            ignore

        member inline _.For (xs: 'U seq, body: 'U -> Rarr<'T> -> unit) = 
            fun (r: Rarr<'T>) ->
                use e = xs.GetEnumerator()
                while e.MoveNext() do    (body e.Current) r

        member inline _.While (predicate: unit -> bool, body: Rarr<'T> -> unit) = 
            fun (r: Rarr<'T>) -> 
                while predicate () do  body r

        member inline _.Run (body: Rarr<'T> -> unit) = 
            let r = Rarr<'T>()
            do body r
            r
        
        member inline  _.TryWith(body: Rarr<'T> -> unit, handler: exn ->  Rarr<'T> -> unit) =
            fun (r: Rarr<'T>) -> 
                try body r  with e -> handler e r

        member inline  _.TryFinally(body: Rarr<'T> -> unit, compensation:  Rarr<'T> -> unit) =
            fun (r: Rarr<'T>) -> 
                try     body r finally compensation  r

        member inline this.Using(disposable: #IDisposable, body: #IDisposable -> Rarr<'T> -> unit) =            
            this.TryFinally( body disposable ,  fun (r: Rarr<'T>)  ->  if not <| Object.ReferenceEquals(disposable,null) then disposable.Dispose() ) // might be disposed already                        

[<AutoOpen>]
module AutoOpenComputationalExpressionsRarr  =     

    /// Computational Expression:  use 'yield' to add elements to a Rarr (a wraper over Collections.Generic.List).
    let rarr<'T> = new ComputationalExpressionsRarr.RarrBuilder<'T> ()               