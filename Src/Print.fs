namespace FsEx

open System
open FsEx.SaveIgnore //so that  |> ignore  can only be used on value types


[<AutoOpen>] // to have print functions at end of module auto opened
module AutoOpenPrint =
    
    open NiceString

    /// Exposes functionality like print and clear of the Seff Editor, when FsEx is loaded there
    type internal Seff private () = // no public constructor  
                  
        static let mutable printColor : int-> int -> int -> string -> unit = //changed via reflection below from Seff
            fun r g b s -> Console.WriteLine s    
           
        static let mutable printnColor : int-> int -> int -> string -> unit = //changed via reflection below from Seff
            fun r g b s -> Console.WriteLine s

        static let mutable clear : unit -> unit = //changed via reflection below from Seff         
            fun () -> ()
        
        static let mutable doInit = true
        
        static let init()=
            doInit <- false
            let allAss = AppDomain.CurrentDomain.GetAssemblies()
            
            let assemblySeff =  allAss |> Seq.tryFind (fun a -> a.GetName().Name = "Seff")            
            
            match assemblySeff with 
            | Some seffAssembly -> 
                try   
                    let printModule = seffAssembly.GetType("Seff.Model.ISeffLogModule")                    
                    printColor   <- printModule.GetProperty("printColor" ).GetValue(seffAssembly)  :?>  int-> int -> int -> string -> unit
                    printnColor  <- printModule.GetProperty("printnColor").GetValue(seffAssembly)  :?>  int-> int -> int -> string -> unit
                    clear        <- printModule.GetProperty("clear").GetValue(seffAssembly)        :?>  unit -> unit
                with ex ->
                    eprintfn "Failed to get Seff.Model.ISeffLog.printnColor via Reflection, If you are not using the Seff Editor Plugin this is normal. The function 'fnColor' will behave just as 'printfn'\r\nMessage: %A" ex     
                
            |None -> ()
                //eprintfn "Only found:"
                //AppDomain.CurrentDomain.GetAssemblies()
                //|> Seq.map (fun a -> a.GetName().Name ) 
                //|> Seq.sortBy string
                //|> Seq.iter (eprintfn "%s" )
            
    
        static member PrintColor r g b s = 
            if doInit then init() // to delay reflection calls to latest possible moment
            printColor r g b s
        
        static member PrintnColor r g b s = 
            if doInit then init() // to delay reflection calls to latest possible moment
            printnColor r g b s

        static member Clear () = 
            if doInit then init() // to delay reflection calls to latest possible moment
            clear()
    
    /// Nice formating for numbers including thousand Separator and (nested) sequences, first five items are printed out.
    /// Settings are exposed in FsEx.NiceString.NiceStringSettings:
    /// • thousandSeparator          = '\'' (this is just one quote: ')  ; set this to change the printing of floats and integers larger than 10'000
    /// • toNiceStringMaxDepth       = 3                                 ; set this to change how deep the content of nested seq is printed (printFull ignores this)
    /// • toNiceStringMaxItemsPerSeq = 5                                 ; set this to change how how many items per seq are printed (printFull ignores this)
    /// • maxCharsInString           = 5000                              ; set this to change how many characters of a string might be printed at once.  
    let print x = 
        Console.WriteLine (toNiceString x)// Console.WriteLine is about 2-3 times faster than printf "%s"
    
    /// Nice formating for numbers including thousand Separator, all items of sequences, including nested items, are printed out.
    /// Settings are exposed in FsEx.NiceString.NiceStringSettings:
    /// • thousandSeparator          = '\'' (this is just one quote: ')  ; set this to change the printing of floats and integers larger than 10'000
    /// • maxCharsInString           = 5000                              ; set this to change how many characters of a string might be printed at once.
    let printFull x = Console.WriteLine (toNiceStringFull x)
    

    /// Highligths the given word in red in the line to print in gray.
    /// Adds line return at end.
    let printWithHighlight (word:string) (fullLine:string)=        
        if String.IsNullOrWhiteSpace word then 
            Seff.PrintnColor 180 180 180 (fullLine)// adds line return 
        else
            let rec loop (fromIdx:int) =
                match fullLine.IndexOf(word, fromIdx) with 
                | -1 -> Seff.PrintnColor 180 180 180 (fullLine.Substring(fromIdx))// adds line return 
                | i  -> 
                    let beforeLen = i - fromIdx
                    if beforeLen > 0 then Seff.PrintColor 180 180 180 (fullLine.Substring(fromIdx,beforeLen))
                
                    if i + word.Length = fullLine.Length then                    
                        Seff.PrintnColor 240   0   0 (fullLine.Substring(i,word.Length)) // adds line return 
                    else                                            
                        Seff.PrintColor  240   0   0 (fullLine.Substring(i,word.Length)) // no line return
                        loop (i + word.Length)
            loop 0
        


    /// Clears the Seff Log View, 
    /// if it can be found via reflection in loaded assemblies,
    /// else does nothing.
    /// Can be called from any thread.
    let clearSeffLog() = Seff.Clear()

/// Tries to printf with colors if running in Seff Editor. 
/// Else just normal printf  
/// Does NOT add a new line at end.  
module Printf = 

    /// Print with rgb colors if running in Seff Editor. Else just normal printf     
    /// red -> green -> blue -> string -> unit
    let color red green blue msg = Printf.kprintf (fun s -> Seff.PrintColor red green blue s)  msg
        
    /// Like printf but in Red if used in Seff Editor. Does not add a new line at end.              
    let red msg =  Printf.kprintf (fun s -> Seff.PrintColor 220 0 0 s)  msg 

    /// Like printf but in Green if used in Seff Editor. Does not add a new line at end.
    let green msg = Printf.kprintf (fun s -> Seff.PrintColor 0 170 0 s)  msg 

    /// Like printf but in Light Blue if in from Seff Editor. Does not add a new line at end.
    let lightBlue msg = Printf.kprintf (fun s -> Seff.PrintColor 170 210 230 s)  msg        

    /// Like printf but in Blue if used in Seff Editor. Does not add a new line at end.
    let blue msg = Printf.kprintf (fun s -> Seff.PrintColor 0 0 220 s)  msg         

    /// Like printf but in Yellow if used in Seff Editor.  Does not add a new line at end.
    let yellow msg = Printf.kprintf (fun s -> Seff.PrintColor 255 230 0 s)  msg   

    /// Like printf but in Gray if used in Seff Editor. Does not add a new line at end.
    let gray msg = Printf.kprintf (fun s -> Seff.PrintColor 150 150 150 s)  msg  
        
    /// Like printf but in Gray if used in Seff Editor. Does not add a new line at end.
    let lightGray msg = Printf.kprintf (fun s -> Seff.PrintColor 200 200 200 s)  msg  

/// Tries to printfn with colors if running in Seff Editor. 
/// Else just normal printf  
/// Adds a new line at end.
module Printfn =        

    /// Print with rgb colors if running in Seff Editor. Else just normal printf     
    /// red -> green -> blue -> string -> unit
    let color red green blue msg = Printf.kprintf (fun s -> Seff.PrintnColor red green blue s)  msg
        
    /// Like printfn but in Red if used in Seff Editor. Adds a new line at end.             
    let red msg =  Printf.kprintf (fun s -> Seff.PrintnColor 220 0 0 s)  msg 

    /// Like printfn but in Green if used in Seff Editor. Adds a new line at end.
    let green msg = Printf.kprintf (fun s -> Seff.PrintnColor 0 170 0 s)  msg 

    /// Like printfn but in Light Blue if in from Seff Editor. Adds a new line at end.
    let lightBlue msg = Printf.kprintf (fun s -> Seff.PrintnColor 170 210 230 s)  msg        

    /// Like printfn but in Blue if used in Seff Editor. Adds a new line at end.
    let blue msg = Printf.kprintf (fun s -> Seff.PrintnColor 0 0 220 s)  msg      
    
    /// Like printfn but in Yellow if used in Seff Editor. Adds a new line at end.
    let yellow msg = Printf.kprintf (fun s -> Seff.PrintnColor 255 230 0 s)  msg     

    /// Like printfn but in Gray if used in Seff Editor. Adds a new line at end.
    let gray msg = Printf.kprintf (fun s -> Seff.PrintnColor 150 150 150 s)  msg  

    /// Like printfn but in Gray if used in Seff Editor. Adds a new line at end.
    let lightGray msg = Printf.kprintf (fun s -> Seff.PrintnColor 200 200 200 s)  msg  
