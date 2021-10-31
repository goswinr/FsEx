namespace FsEx

open System

/// A module to run command line processes via Diagnostics.Process()
module Process = 

    /// Run a command synchronously.
    /// The error and standart text output is redirected to the delegates 'output' and 'error'.
    let run (output:string->unit) (error:string->unit) processName arguments : unit =         
        //https://stackoverflow.com/questions/1145969/processinfo-and-redirectstandardoutput
        let p = new Diagnostics.Process()
        p.StartInfo.FileName <- processName
        p.StartInfo.Arguments <- arguments
        // for console also see https://stackoverflow.com/a/1427817/969070
        p.StartInfo.StandardOutputEncoding <- Text.Encoding.GetEncoding(Globalization.CultureInfo.CurrentCulture.TextInfo.OEMCodePage) //https://stackoverflow.com/a/48436394/969070
        p.StartInfo.StandardErrorEncoding  <- Text.Encoding.GetEncoding(Globalization.CultureInfo.CurrentCulture.TextInfo.OEMCodePage) //https://stackoverflow.com/a/48436394/969070
        p.EnableRaisingEvents <- true
        p.StartInfo.UseShellExecute <- false
        p.StartInfo.CreateNoWindow <- true //true if the process should be started without creating a new window to contain it
        p.StartInfo.RedirectStandardError <-true
        p.StartInfo.RedirectStandardOutput <-true
        p.OutputDataReceived.Add ( fun d -> if notNull d.Data then output(d.Data) )
        p.ErrorDataReceived.Add  ( fun d -> if notNull d.Data then error (d.Data) )
        //p.Exited.Add( fun _ ->() )    //covered by p.WaitForExit()    
        p.Start() |> ignore
        p.BeginOutputReadLine()
        p.BeginErrorReadLine()
        p.WaitForExit()    



    
    /// Run a command synchronously.
    /// ProcessName, the arguments, output text and errors will be printed to Console.Out.
    /// Each process's output is sourrounded by a border drawn with ASCII art characters.
    /// If this is called inside Seff Editor the words in redHighlights and greenHighlights will be highlighted if found in the output text.    
    let runWithHighlighting (redHighlights:string list) (greenHighlights:string list) processName arguments : unit = 
        let gray = 170        
        Console.WriteLine "┌────────────────────────────────────────────────────────────────────────────────────────────────────────"  
        Console.Write "│"
        Printf.color 50 50 100 "%s " processName
        Printfn.color 50 50 180 "%s" arguments   
        run            
            (fun txt ->
                let mutable printPending = true
                for r in redHighlights do
                    if printPending && not (String.IsNullOrWhiteSpace r) && txt.Contains(r) then //TODO: add Regex based highlighting
                        Console.Write "│"
                        printWithHighlightColor 240 0 0 gray gray gray r txt
                        printPending <- false
                for g in greenHighlights do
                    if  printPending && not (String.IsNullOrWhiteSpace g) && txt.Contains(g) then
                        Console.Write "│"
                        printWithHighlightColor 0 200 0 gray gray gray g txt
                        printPending <- false
                if printPending then
                    Console.Write "│"
                    Printfn.color gray gray gray "%s" txt  )
            (fun e -> Console.Write "│" ;  Printfn.color 240 0 240 "%s" e )
            processName
            arguments
        Console.WriteLine "└────────────────────────────────────────────────────────────────────────────────────────────────────────"


    (*
    /// to run them all in sequence and not at the same time
    let private pendingOps = Collections.Concurrent.ConcurrentQueue<Async<unit>>()

    /// Run a command atomically. Subsequent calls to run will be enqueued async and only run when the previous one is done.
    /// The delegates : Starting(name and args), output, errors and completion will be called as their name suggests.
    let runAtomicallyWithDelegates (starting:string*string->unit) (output:string->unit) (error:string->unit) (oneCompleted:unit->unit) (allCompleted:unit->unit) processName arguments : unit = 
        let op = 
            async{
                //https://stackoverflow.com/questions/1145969/processinfo-and-redirectstandardoutput
                let p = new Diagnostics.Process()
                p.StartInfo.FileName <- processName
                p.StartInfo.Arguments <- arguments
                // for console also see https://stackoverflow.com/a/1427817/969070
                p.StartInfo.StandardOutputEncoding <- Text.Encoding.GetEncoding(Globalization.CultureInfo.CurrentCulture.TextInfo.OEMCodePage) //https://stackoverflow.com/a/48436394/969070
                p.StartInfo.StandardErrorEncoding  <- Text.Encoding.GetEncoding(Globalization.CultureInfo.CurrentCulture.TextInfo.OEMCodePage) //https://stackoverflow.com/a/48436394/969070
                p.EnableRaisingEvents <- true
                p.StartInfo.UseShellExecute <- false
                p.StartInfo.CreateNoWindow <- true //true if the process should be started without creating a new window to contain it
                p.StartInfo.RedirectStandardError <-true
                p.StartInfo.RedirectStandardOutput <-true
                p.OutputDataReceived.Add ( fun d -> if notNull d.Data then output(d.Data) )
                p.ErrorDataReceived.Add (  fun d -> if notNull d.Data then error(d.Data) )
                p.Exited.Add( fun _ ->
                    oneCompleted()
                    match pendingOps.TryDequeue() with // only remove operatiopn from queue once its done
                    | false, _  -> ()// should never be false, because the current op is still inside the queue and can be dequeued
                    | true , _ ->  // this operation that just finished was removed fom queue now
                        match pendingOps.TryPeek() with // now  peek if there is a next one that was added in the meantime , (but dont remove it yet)
                        | false, _  -> allCompleted() // end of recursion
                        | true , nextOp -> // loop to next operation
                            nextOp |> Async.Start
                    )
                starting(processName,arguments)
                p.Start() |> ignore
                p.BeginOutputReadLine()
                p.BeginErrorReadLine()
                p.WaitForExit()
                }

        pendingOps.Enqueue(op) // to block any other coming afterwards
        match pendingOps.Count with
        | 1 ->  op |> Async.Start //only start if this operation that was just added is the only one in the queue
        | _ -> ()  //otherwise do nothing since it is enqueued already an will be started at end of currently running operation


    /// Run a command atomically. Subsequent calls to run will be enqueued async and only run when this one is done.
    /// ProcessName, arguments, output and errors will be printed to Console.Out as well as "--------------" after each process.
    /// If this is called inside Seff Editor the words in redHighlights and greenHighlights will be highlighted if found in the output text.
    /// The allCompleted callback wil be called at the end if there are no more other processes pending.
    let runAtomicallyWithHighlighting (redHighlights:string list) (greenHighlights:string list) (allCompleted:unit->unit) processName arguments : unit = 
        let gray = 170        
        runAtomicallyWithDelegates
            (fun (pName,args) ->
                Printfn.color 180 180 255 "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"  
                Printf.color 50 50 100 "%s " pName
                Printfn.color 50 50 180 "%s" args    )
            (fun txt ->
                let mutable printPending = true
                for r in redHighlights do
                    if printPending && not (String.IsNullOrWhiteSpace r) && txt.Contains(r) then
                        printWithHighlightColor 240 0 0 gray gray gray r txt
                        printPending <- false
                for g in greenHighlights do
                    if  printPending && not (String.IsNullOrWhiteSpace g) && txt.Contains(g) then
                        printWithHighlightColor 0 200 0 gray gray gray g txt
                        printPending <- false
                if printPending then
                    Printfn.color gray gray gray "%s" txt  )
            (fun e -> Printfn.color 240 0 240 "%s" e )
            (fun ()-> Printfn.color 180 180 255 "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv")
            allCompleted
            processName
            arguments

    /// Run a command atomically. Subsequent calls to run will be enqueued async and only run when this one is done.
    /// ProcessName, arguments, output and errors will be printed to Console.Out as well as "--------------" after each process.
    let runAtomically processName arguments : unit = 
        runAtomicallyWithDelegates
            (fun (pName,args) -> printf "%s " pName ; printfn "%s" args    )
            (printfn "%s")
            (printfn "%s")
            (fun () -> printfn "----------------------------------------------------------------------" )
            (fun () -> ())
            processName
            arguments



    *)