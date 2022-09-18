namespace FsEx

open System
open FsEx.SaveIgnore //so that  |> ignore  can only be used on value types

// TODO check with https://github.com/eiriktsarpalis/TypeShape/blob/main/samples/TypeShape.Samples/printer.fs

/// This will provide the functions:
/// print, printFull, printWithHighlight, clearSeffLog
[<AutoOpen>] // to have print functions at end of module auto opened
module AutoOpenPrint = 

    open NiceString

    /// Exposes functionality like print and clear of the Seff Editor, when FsEx is loaded there
    type internal Seff private () = // no public constructor

        static let mutable printColor : int-> int -> int -> string -> unit = //changed via reflection below from Seff
            fun r g b s -> Console.Write s

        static let mutable printNewLineColor : int-> int -> int -> string -> unit = //changed via reflection below from Seff
            fun r g b s -> Console.WriteLine s

        static let mutable clear : unit -> unit = // changed via reflection below from Seff
            fun () -> ()

        static let mutable initIsPending = true

        static let init()= 
            initIsPending <- false
            let allAss = AppDomain.CurrentDomain.GetAssemblies()

            let assemblySeff =  allAss |> Seq.tryFind (fun a -> a.GetName().Name = "Seff")

            match assemblySeff with
            | Some seffAssembly ->
                try
                    let printModule = seffAssembly.GetType("Seff.Model.ISeffLogModule")
                    if notNull printModule then 
                        let pc = printModule.GetProperty("printColor" ).GetValue(seffAssembly)  
                        if notNull pc then 
                            let pct = pc :?>  int-> int -> int -> string -> unit
                            printColor   <- pct
                        let pnc = printModule.GetProperty("printnColor").GetValue(seffAssembly) 
                        if notNull pnc then 
                            let pct = pnc :?> int-> int -> int -> string -> unit
                            printNewLineColor   <- pct
                        let cl = printModule.GetProperty("clear").GetValue(seffAssembly) 
                        if notNull cl then 
                            let clt = cl :?>unit -> unit
                            clear   <- clt                        
                with ex ->
                    eprintfn "Failed to get Seff.Model.ISeffLog.printnColor via Reflection, If you are not using the Seff Editor Plug-in this is normal."
                    eprintfn "The function using color will just print to Console.Out."
                    eprintfn "The ignored Error was: %A" ex

            |None -> ()
                //eprintfn "Only found:"
                //AppDomain.CurrentDomain.GetAssemblies()
                //|> Seq.map (fun a -> a.GetName().Name )
                //|> Seq.sortBy string
                //|> Seq.iter (eprintfn "%s" )


        static member PrintColor r g b s = 
            if initIsPending then init() // to delay reflection calls to latest possible moment
            printColor r g b s

        static member PrintLineColor r g b s = 
            if initIsPending then init() // to delay reflection calls to latest possible moment
            printNewLineColor r g b s

        static member Clear () = 
            if initIsPending then init() // to delay reflection calls to latest possible moment
            clear()


    /// Print to standard out. 
    /// Nice formatting for numbers including thousand Separator, records and (nested) sequences, 
    /// NicePrintSettings:
    /// maxDepth          = 3     , how deep the content of nested seq is printed (printFull ignores this)
    /// maxVertItems      = 6     , how many items per seq are printed in vertical list(printFull ignores this)
    /// maxHorChars       = 120   , how  many chars can be in one line before switching to vertical sequencing of items in collection.
    /// maxCharsInString  = 2000  , how many characters of a string might be printed at once.
    /// Settings are exposed in FsEx.NiceString.NiceStringSettings:
    /// .thousandSeparator       = '     ; set this to change the printing of floats and integers larger than 10'000
    let print x = 
        Console.WriteLine (toNiceString x) // Console.WriteLine is about 2-3 times faster than printf "%s"

    /// Print to standard out.
    /// Nice formatting for numbers including thousand Separator, records and (nested) sequences, 
    /// NicePrintSettings:
    /// maxDepth          = 24        , how deep the content of nested seq is printed (printFull ignores this)
    /// maxVertItems      = 100'000   , how many items per seq are printed in vertical list(printFull ignores this)
    /// maxHorChars       = 240       , how  many chars can be in one line before switching to vertical sequencing of items in collection.
    /// maxCharsInString  = 1'000'000 , how many characters of a string might be printed at once.
    /// Settings that are exposed in FsEx.NiceString.NiceStringSettings:
    /// .thousandSeparator = '     , set this to change the printing of floats and integers larger than 10'000
    /// .roundToZeroBelow  = 1e-24 , if the absolute value of a float is below this, display ±0.0
    let printFull x = 
        Console.WriteLine (toNiceStringFull x) // Console.WriteLine is about 2-3 times faster than printf "%s"


    /// Highlights every occurrence of the given word in the color of first three integers (red, green, blue)
    /// and the rest of the line in next three integers.
    /// Adds line return at end.
    let internal printWithHighlightColor wR wG wB fR fG fB (word:string) (fullLine:string)= 
        if String.IsNullOrWhiteSpace word then
            Seff.PrintLineColor fR fG fB (fullLine)// adds line return
        else
            let rec loop (fromIdx:int) = 
                match fullLine.IndexOf(word, fromIdx, StringComparison.Ordinal) with
                | -1 -> Seff.PrintLineColor fR fG fB (fullLine.Substring(fromIdx))// adds line return
                | i  ->
                    let beforeLen = i - fromIdx
                    if beforeLen > 0 then Seff.PrintColor fR fG fB (fullLine.Substring(fromIdx,beforeLen))

                    if i + word.Length = fullLine.Length then
                        Seff.PrintLineColor wR wG wB (fullLine.Substring(i,word.Length)) // adds line return
                    else
                        Seff.PrintColor wR wG wB (fullLine.Substring(i,word.Length)) // no line return
                        loop (i + word.Length)
            loop 0

    /// Tries to printf with colors if running inside Seff Editor.
    /// Highlights the given word in red in the line to print in gray.
    /// Adds line return at end.
    let printWithHighlight (word:string) (fullLine:string)= 
        printWithHighlightColor 240 0 0 180 180 180 word fullLine


    /// Tries to clears the Seff Log View, if it can be found via reflection in loaded assemblies.
    /// Else does nothing.
    /// Can be called from any thread.
    let clearSeffLog() = Seff.Clear()



/// Tries to printf with colors if running inside Seff Editor.
/// Else just normal printf
/// Does NOT add a new line at end.
module Printf = 

    /// Print with rgb colors if running in Seff Editor. Else just normal printf
    /// red -> green -> blue -> string -> unit
    let color red green blue msg = Printf.kprintf (fun s -> Seff.PrintColor red green blue s)  msg


    /// Like printf but in Light Green if used in Seff Editor. Does not add a new line at end.
    let lightGreen msg = Printf.kprintf (fun s -> Seff.PrintColor 110 255 110 s)  msg

    /// Like printf but in Green if used in Seff Editor. Does not add a new line at end.
    let green msg = Printf.kprintf (fun s -> Seff.PrintColor 0 190 0 s)  msg

    /// Like printf but in Dark Green if used in Seff Editor. Does not add a new line at end.
    let darkGreen msg = Printf.kprintf (fun s -> Seff.PrintColor 0 100 0 s)  msg
    
    /// Like printf but in Light Red if used in Seff Editor. Does not add a new line at end.
    let lightRed msg = Printf.kprintf (fun s -> Seff.PrintColor 255 180 190 s)  msg
    
    /// Like printf but in Red if used in Seff Editor. Does not add a new line at end.
    let red msg =  Printf.kprintf (fun s -> Seff.PrintColor 220 0 0 s)  msg

    /// Like printf but in Dark Red if used in Seff Editor. Does not add a new line at end.
    let darkRed msg = Printf.kprintf (fun s -> Seff.PrintColor 180 0 0 s)  msg    
    
    /// Like printf but in Light Blue if in from Seff Editor. Does not add a new line at end.
    let lightBlue msg = Printf.kprintf (fun s -> Seff.PrintColor 170 210 230 s)  msg

    /// Like printf but in Blue if used in Seff Editor. Does not add a new line at end.
    let blue msg = Printf.kprintf (fun s -> Seff.PrintColor 0 0 220 s)  msg

    /// Like printf but in Yellow if used in Seff Editor.  Does not add a new line at end.
    let yellow msg = Printf.kprintf (fun s -> Seff.PrintColor 235 220 0 s)  msg

    /// Like printf but in Gray if used in Seff Editor. Does not add a new line at end.
    let lightGray msg = Printf.kprintf (fun s -> Seff.PrintColor 210 210 210 s)  msg

    /// Like printf but in Gray if used in Seff Editor. Does not add a new line at end.
    let gray msg = Printf.kprintf (fun s -> Seff.PrintColor 160 160 160 s)  msg

    /// Like printf but in Dark Gray if used in Seff Editor. Does not add a new line at end.
    let darkGray msg = Printf.kprintf (fun s -> Seff.PrintColor 100 100 100 s)  msg

    /// Like printf but in Orchid purple if used in Seff Editor. Does not add a new line at end.
    let orchid msg = Printf.kprintf (fun s -> Seff.PrintColor 218 112 214 s)  msg

    /// Like printf but in Purple if used in Seff Editor. Does not add a new line at end.
    let purple msg = Printf.kprintf (fun s -> Seff.PrintColor 128 0 128 s)  msg

    /// Like printf but in Orange if used in Seff Editor. Does not add a new line at end.
    let orange msg = Printf.kprintf (fun s -> Seff.PrintColor 255 140 0 s)  msg

    /// Like printf but in Cyan if used in Seff Editor. Does not add a new line at end.
    let cyan msg = Printf.kprintf (fun s -> Seff.PrintColor 0 150 150 s)  msg

    /// Like printf but in Random Color if used in Seff Editor. Does not add a new line at end.
    /// Very light, white and yellow colors are excluded
    /// The colors used by subsequent calls to this functions will have very distinct hues.
    /// This is achieved by using a golden-ratio-loop and an internal cache of the last generated color.
    let colorRnd msg = 
        let c = Color.randomForRhino()
        Printf.kprintf (fun s -> Seff.PrintColor c.Red.ToInt c.Green.ToInt c.Blue.ToInt s)  msg


/// Tries to printfn with colors if running in Seff Editor.
/// Else just normal printf
/// Adds a new line at end.
module Printfn = 

    /// Print with rgb colors if running in Seff Editor. Else just normal printf
    /// red -> green -> blue -> string -> unit
    let color red green blue msg = Printf.kprintf (fun s -> Seff.PrintLineColor red green blue s)  msg
    
    /// Like printfn but in Light Green if used in Seff Editor. Adds a new line at end.
    let lightGreen msg = Printf.kprintf (fun s -> Seff.PrintLineColor 110 255 110 s)  msg

    /// Like printfn but in Green if used in Seff Editor. Adds a new line at end.
    let green msg = Printf.kprintf (fun s -> Seff.PrintLineColor 0 190 0 s)  msg

    /// Like printfn but in Dark Green if used in Seff Editor. Adds a new line at end.
    let darkGreen msg = Printf.kprintf (fun s -> Seff.PrintLineColor 0 100 0 s)  msg

    /// Like printfn but in Light Red if used in Seff Editor. Adds a new line at end.
    let lightRed msg = Printf.kprintf (fun s -> Seff.PrintLineColor  255 180 190 s)  msg

    /// Like printfn but in Red if used in Seff Editor. Adds a new line at end.
    let red msg =  Printf.kprintf (fun s -> Seff.PrintLineColor 220 0 0 s)  msg
    
    /// Like printfn but in Dark Red if used in Seff Editor. Adds a new line at end.
    let darkRed msg = Printf.kprintf (fun s -> Seff.PrintLineColor 180 0 0 s)  msg

    /// Like printfn but in Light Blue if in from Seff Editor. Adds a new line at end.
    let lightBlue msg = Printf.kprintf (fun s -> Seff.PrintLineColor 170 210 230 s)  msg

    /// Like printfn but in Blue if used in Seff Editor. Adds a new line at end.
    let blue msg = Printf.kprintf (fun s -> Seff.PrintLineColor 0 0 220 s)  msg

    /// Like printfn but in Yellow if used in Seff Editor. Adds a new line at end.
    let yellow msg = Printf.kprintf (fun s -> Seff.PrintLineColor 235 220 0 s)  msg

    /// Like printfn but in Dark Gray if used in Seff Editor. Adds a new line at end.
    let darkGray msg = Printf.kprintf (fun s -> Seff.PrintLineColor 100 100 100 s)  msg

    /// Like printfn but in Gray if used in Seff Editor. Adds a new line at end.
    let gray msg = Printf.kprintf (fun s -> Seff.PrintLineColor 160 160 160 s)  msg

    /// Like printfn but in Gray if used in Seff Editor. Adds a new line at end.
    let lightGray msg = Printf.kprintf (fun s -> Seff.PrintLineColor 210 210 210 s)  msg

    /// Like printfn but in Orchid (light Purple) if used in Seff Editor. Adds a new line at end.
    let orchid msg = Printf.kprintf (fun s -> Seff.PrintLineColor 218 112 214  s)  msg

    /// Like printfn but in Purple if used in Seff Editor. Adds a new line at end.
    let purple msg = Printf.kprintf (fun s -> Seff.PrintLineColor 128 0 128 s)  msg

    /// Like printfn but in Orange if used in Seff Editor. Adds a new line at end.
    let orange msg = Printf.kprintf (fun s -> Seff.PrintLineColor 255 140 0 s)  msg

    /// Like printfn but in Cyan if used in Seff Editor. Adds a new line at end.
    let cyan msg = Printf.kprintf (fun s -> Seff.PrintLineColor 0 150 150 s)  msg

    /// Like printfn but in random Color if used in Seff Editor. Adds a new line at end.
    /// Very light, white and yellow colors are excluded
    /// The colors used by subsequent calls to this functions will have very distinct hues.
    /// This is achieved by using a golden-ratio-loop and an internal cache of the last generated color.
    let colorRnd msg = 
        let c = Color.randomForRhino()
        Printf.kprintf (fun s -> Seff.PrintLineColor c.Red.ToInt c.Green.ToInt c.Blue.ToInt s)  msg
