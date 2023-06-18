open System

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
        do f() |> ignore
        this.While(guard, f)

    member inline  this.For(sequence:seq<_>, body) = 
        this.Using(sequence.GetEnumerator(), fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current)))

let maybe = MaybeBuilder()

let a () =  
    printfn "a"
    Some 1

let b x =  
    printfn "b"
    Some (1+x) 
    
let c x =  
    printfn "c"
    Some (1+x) 
    
let d x =  
    (1+x)  |> ignore 
    None 
    
let e x =  
    printfn "e"
    (1+x)  |> ignore 
    None 
    
let f x =  
    printfn "f"
    Some (1+x) 
    
maybe{ 
    let mutable k = 10
    while k > 0 do  
        k <- k - 1 
       
    
    let! aa = a()
    let! bb = b(aa)
    let! cc = c(bb)
    let! dd = d(cc)
    let! _ = a()
    let! _ = a()
    let! _ = a()
    let! ee = e(dd)
    return! f(ee) 
    } |> printfn "Result: %A"