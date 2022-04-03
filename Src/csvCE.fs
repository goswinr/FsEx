namespace FsEx

open System
open System.Text
//open FsEx.SaveIgnore //so that  |> ignore  can only be used on value types

module ComputationalExpressionsBuilderCsv = 
    let mutable csvSepEn = ','
    let mutable csvSepDe = ';'
   
    let inline private addCsvEn   (b: StringBuilder) (s:string) = b.Append(s).Append(csvSepEn)                               |> ignore<StringBuilder>
    let inline private addCsvEnLn (b: StringBuilder) (s:string) = b.Append(s).Append(csvSepEn).Append(Environment.NewLine)   |> ignore<StringBuilder>
    let inline private addCsvDe   (b: StringBuilder) (s:string) = b.Append(s).Append(csvSepDe)                               |> ignore<StringBuilder>
    let inline private addCsvDeLn (b: StringBuilder) (s:string) = b.Append(s).Append(csvSepDe).Append(Environment.NewLine)   |> ignore<StringBuilder>


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

        member inline  _.TryWith(body: StringBuilder -> unit, handler: exn ->  StringBuilder -> unit) =
            fun (b: StringBuilder) -> 
                try body b with e -> handler e b

        member inline  _.TryFinally(body: StringBuilder -> unit, compensation:  StringBuilder -> unit) =
            fun (b: StringBuilder) ->  
                try body b finally compensation  b

        member inline this.Using(disposable: #IDisposable, body: #IDisposable -> StringBuilder -> unit) =            
            this.TryFinally(  body disposable ,  fun (b: StringBuilder)  ->  if not <| Object.ReferenceEquals(disposable,null) then disposable.Dispose() ) // might be disposed already                        
       


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
        
         member inline  _.TryWith(body: StringBuilder -> unit, handler: exn ->  StringBuilder -> unit) =
             fun (b: StringBuilder) -> 
                 try body b with e -> handler e b
        
         member inline  _.TryFinally(body: StringBuilder -> unit, compensation:  StringBuilder -> unit) =
             fun (b: StringBuilder) ->  
                 try body b finally compensation  b
        
         member inline this.Using(disposable: #IDisposable, body: #IDisposable -> StringBuilder -> unit) =            
             this.TryFinally(  body disposable ,  fun (b: StringBuilder)  ->  if not <| Object.ReferenceEquals(disposable,null) then disposable.Dispose() ) // might be disposed already                        
        
    
 
[<AutoOpen>]
module AutoOpenComputationalExpressionCSV  = 
    open ComputationalExpressionsBuilderCsv

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




