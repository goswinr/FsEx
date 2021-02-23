﻿namespace FsEx


open System
open FsEx.SaveIgnore //so that  |> ignore  can only be used on value types
open System.Linq.Expressions


[<AutoOpen>] // to have print functions at end of module auto opened
module Print =
    
    open NiceString

    /// Exposes functionality liek print and clear of the Seff Editor, when FsEx is loaded there
    type internal Seff private () = // no public constructor  
                  
        static let mutable printColor : int-> int -> int -> string -> unit = //set via reflection below from Seff
            fun r g b s -> Console.WriteLine s    
           
        static let mutable printnColor : int-> int -> int -> string -> unit = //set via reflection below from Seff
            fun r g b s -> Console.WriteLine s

        static let mutable clear : unit -> unit =  //set via reflection below from Seff         
            fun () -> ()
        
        static let mutable doInit = true
        
        static let init()=
            doInit <- false
            let allAss = AppDomain.CurrentDomain.GetAssemblies()
            
            let assemblySeff       =  allAss |> Seq.tryFind (fun a -> a.GetName().Name = "Seff")            
            
            match assemblySeff with 
            | Some seffAssembly -> 
                try   
                    let printModule = seffAssembly.GetType("Seff.Model.ISeffLogModule")                    
                    printColor   <- printModule.GetProperty("printColor" ).GetValue(seffAssembly)  :?>  int-> int -> int -> string -> unit
                    printnColor  <- printModule.GetProperty("printnColor").GetValue(seffAssembly)  :?>  int-> int -> int -> string -> unit
                    clear        <- printModule.GetProperty("clear").GetValue(seffAssembly)        :?>  unit -> unit
                with ex ->
                    eprintfn "Failed to get Seff.Model.ISeffLog.printnColor via Reflection, If you are not using the Seff Editor Plugin this is normal. The function 'printfnColor' will behave just as 'printfn'\r\nMessage: %A" ex     
                
            |None -> ()
                //eprintfn "Only found:"
                //AppDomain.CurrentDomain.GetAssemblies()
                //|> Seq.map (fun a -> a.GetName().Name ) 
                //|> Seq.sortBy string
                //|> Seq.iter (eprintfn "%s" )
            
    
        static member PrintColor r g b s = 
            if doInit then init() // to delay reflection calls to lates possible moment
            printColor r g b s
        
        static member PrintnColor r g b s = 
            if doInit then init() // to delay reflection calls to lates possible moment
            printnColor r g b s

        static member Clear () = 
            if doInit then init() // to delay reflection calls to lates possible moment
            clear()
    
    /// Includes nice formating for floats , on seqencesthe  first five items are printed out.
    /// Settings are exposed in NiceString.NiceStringSettings
    /// •thousandSeparator          = '\'' (this is just one quote: ')  ; set this to change the printing of floats larger than 10'000
    /// •toNiceStringMaxDepth       = 3                                 ; set this to change how deep the content of nested seq is printed (printFull ignores this)
    /// •toNiceStringMaxItemsPerSeq = 5                                 ; set this to change how how many items per seq are printed (printFull ignores this)
    /// • maxCharsInString          = 5000                              ; set this to change how many characters of a string might be printed at onece.    
    let print x = Console.WriteLine (toNiceString x)// about 2-3 times faster than printf "%s"
    
    /// prints two values separated by a space using FsEx.toNiceString
    let print2 x y = printfn "%s %s" (toNiceString x) (toNiceString y)
    
    /// prints three values separated by a space using FsEx.toNiceString
    let print3 x y z = printfn "%s %s %s" (toNiceString x) (toNiceString y) (toNiceString z) 
    
    /// prints four values separated by a space using FsEx.toNiceString
    let print4 w x y z = printfn "%s %s %s %s" (toNiceString w) (toNiceString x) (toNiceString y) (toNiceString z) 

    /// prints FsEx.toNiceStringFull
    let printFull x = Console.WriteLine (toNiceStringFull x)
    
    /// print with rgb colors if running in Seff Editor. Else just normal printf 
    /// does NOT add a new line
    /// red -> green -> blue -> string -> unit
    let printfColor red green blue msg =  Printf.kprintf (fun s -> Seff.PrintColor red green blue s)  msg

    /// print with rgb colors if running in Seff Editor. Else just normal printfn
    /// adds a new line at end
    /// red -> green -> blue -> string -> unit
    let printfnColor red green blue msg = Printf.kprintf (fun s -> Seff.PrintnColor red green blue s)  msg


    /// highligths the given word in the line to print 
    let printWithHighlight (word:string) (fullLine:string)=        
        let rec loop (fromIdx:int) =
            match fullLine.IndexOf(word, fromIdx) with 
            | -1 -> Seff.PrintnColor 180 180 180 (fullLine.Substring(fromIdx))// adds line return 
            | i  -> 
                let beforeLen = i - fromIdx
                if beforeLen > 0 then Seff.PrintColor 180 180 180 (fullLine.Substring(fromIdx,beforeLen))
                
                if i + word.Length = fullLine.Length then                    
                    Seff.PrintnColor 0   0   0 (fullLine.Substring(i,word.Length)) // adds line return 
                else                                            
                    Seff.PrintColor  0   0   0 (fullLine.Substring(i,word.Length)) // no line return
                    loop (i + word.Length)
        loop 0
        


    /// Clears the Seff Log View, 
    /// if it can be found via reflection in loaded assemblies,
    /// else does nothing.
    /// Can be called from any thread.
    let clearSeffLog() = Seff.Clear()


