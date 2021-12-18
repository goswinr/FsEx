namespace FsEx

open System

/// A module to run command line processes via Diagnostics.Process()
module Process = 

    /// Run a command synchronously.
    /// The error and standard text output is redirected to the delegates 'output' and 'error'.
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
    /// Each process's output is surrounded by a border drawn with ASCII art characters.
    /// If this is called inside Seff Editor the words in redHighlights and greenHighlights will be highlighted if found in the output text.    
    let runWithHighlighting (redHighlights:string list) (greenHighlights:string list) processName arguments : unit = 
        let gray = 170        
        Console.WriteLine "┌─────────────────────────────────────────────────────────────────"  
        Console.Write     "│"
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
        Console.WriteLine "└─────────────────────────────────────────────────────────────────"

