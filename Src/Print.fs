﻿namespace FsEx

open System
open FsEx.SaveIgnore //so that  |> ignore  can only be used on value types

// TODO check with https://github.com/eiriktsarpalis/TypeShape/blob/main/samples/TypeShape.Samples/printer.fs

/// This will provide the functions:
/// print, printFull, printWithHighlight, clearFeshLog
[<AutoOpen>] // to have print functions at end of module auto opened
module AutoOpenPrint =

    open NiceString


    /// Exposes functionality like print and clear of the Fesh Editor, when FsEx is loaded there
    type internal Fesh private () = // no public constructor

        static let mutable printColor : int-> int -> int -> string -> unit = //changed via reflection below from Fesh
            fun _ _ _ s -> Console.Write s //fun r g b s -> Console.Write s

        static let mutable printNewLineColor : int-> int -> int -> string -> unit = //changed via reflection below from Fesh
            fun _ _ _  s -> Console.WriteLine s //fun r g b s -> Console.WriteLine s

        static let mutable clear : unit -> unit = // changed via reflection below from Fesh
            fun () -> ()

        static let mutable initIsPending = true

        static let init()=
            initIsPending <- false
            let allAss = AppDomain.CurrentDomain.GetAssemblies()

            let assemblyFesh =  allAss |> Seq.tryFind (fun a -> a.GetName().Name = "Fesh")

            match assemblyFesh with
            | Some feshAssembly ->
                try
                    let printModule = feshAssembly.GetType("Fesh.Model.IFeshLogModule")
                    if notNull printModule then
                        let pc = printModule.GetProperty("printColor" ).GetValue(feshAssembly)
                        if notNull pc then
                            let pct = pc :?>  int-> int -> int -> string -> unit
                            printColor   <- pct
                        let pnc = printModule.GetProperty("printnColor").GetValue(feshAssembly)
                        if notNull pnc then
                            let pct = pnc :?> int-> int -> int -> string -> unit
                            printNewLineColor   <- pct
                        let cl = printModule.GetProperty("clear").GetValue(feshAssembly)
                        if notNull cl then
                            let clt = cl :?>unit -> unit
                            clear   <- clt
                with ex ->
                    eprintfn "The Fesh was found but setting up color printing failed. The Error was: %A" ex

            |None -> ()
                //eprintfn "Failed to get Fesh.Model.IFeshLog.printnColor via Reflection, If you are not using the Fesh F# Editor this message is normal."
                //eprintfn "The print and Printfn functions using color will just print to Console.Out."
                //eprintfn "The ignored Error was: %A" ex

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
    /// .roundToZeroBelow  = 1e-24 , if the absolute value of a float is below this, display ±0.0
    let print x =
        Console.WriteLine (toNiceString x) // Console.WriteLine is about 2-3 times faster than printf "%s"

    /// Print to standard out.
    /// Nice formatting for numbers including thousand Separator, records and (nested) sequences,
    /// NicePrintSettings:
    /// maxDepth          = 3     , how deep the content of nested seq is printed (printFull ignores this)
    /// maxVertItems      = 50    , how many items per seq are printed in vertical list(printFull ignores this)
    /// maxHorChars       = 120   , how  many chars can be in one line before switching to vertical sequencing of items in collection.
    /// maxCharsInString  = 2000  , how many characters of a string might be printed at once.
    /// Settings that are exposed in FsEx.NiceString.NiceStringSettings:
    /// .thousandSeparator = '     , set this to change the printing of floats and integers larger than 10'000
    /// .roundToZeroBelow  = 1e-24 , if the absolute value of a float is below this, display ±0.0
    let printLong x =
        Console.WriteLine (toNiceString x)

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
    /// Adds a line return at end.
    let printWithHighlightColor wR wG wB fR fG fB (word:string) (fullLine:string)=
        if String.IsNullOrWhiteSpace word then
            Fesh.PrintLineColor fR fG fB (fullLine)// adds line return
        else
            let rec loop (fromIdx:int) =
                match fullLine.IndexOf(word, fromIdx, StringComparison.Ordinal) with
                | -1 -> Fesh.PrintLineColor fR fG fB (fullLine.Substring(fromIdx))// adds line return
                | i  ->
                    let beforeLen = i - fromIdx
                    if beforeLen > 0 then Fesh.PrintColor fR fG fB (fullLine.Substring(fromIdx,beforeLen))

                    if i + word.Length = fullLine.Length then
                        Fesh.PrintLineColor wR wG wB (fullLine.Substring(i,word.Length)) // adds line return
                    else
                        Fesh.PrintColor wR wG wB (fullLine.Substring(i,word.Length)) // no line return
                        loop (i + word.Length)
            loop 0

    /// Highlights every occurrence of the given Regex in the color of first three integers (red, green, blue)
    /// and the rest of the line in next three integers.
    /// Adds a line return at end.
    let printWithHighlightColorRegex wR wG wB fR fG fB (re:Text.RegularExpressions.Regex) (fullLine:string)=
        if isNull re then
            Fesh.PrintLineColor fR fG fB (fullLine)// adds line return
        else
            let rec loop (fromIdx:int) =
                let m =  re.Match(fullLine, fromIdx)
                if m.Success then
                    let i = m.Index
                    let length = m.Length
                    let beforeLen =  i - fromIdx
                    if beforeLen > 0 then Fesh.PrintColor fR fG fB (fullLine.Substring(fromIdx,beforeLen))

                    if i + length = fullLine.Length then
                        Fesh.PrintLineColor wR wG wB (fullLine.Substring(i,length)) // adds line return
                    else
                        Fesh.PrintColor wR wG wB (fullLine.Substring(i,length)) // no line return
                        loop (i + length)
                else
                    Fesh.PrintLineColor fR fG fB (fullLine.Substring(fromIdx))// adds line return

            loop 0


    /// Tries to printf with colors if running inside Fesh Editor.
    /// Highlights the given word in red in the line to print in gray.
    /// Adds a line return at end.
    let printWithHighlight (word:string) (fullLine:string)=
        printWithHighlightColor 240 0 0 180 180 180 word fullLine


    /// Tries to clears the Fesh Log View, if it can be found via reflection in loaded assemblies.
    /// Else does nothing.
    /// Can be called from any thread.
    let clearFeshLog() = Fesh.Clear()

    [<Obsolete("Renamed to clearFeshLog")>]
    let clearSeffLog() = Fesh.Clear()



/// Tries to printf with colors if running inside Fesh Editor.
/// Else just normal printf
/// Does NOT add a new line at end.
module Printf =

    /// Print with rgb colors if running in Fesh Editor. Else just normal printf
    /// red -> green -> blue -> string -> unit
    let color red green blue msg = Printf.kprintf (fun s -> Fesh.PrintColor red green blue s)  msg


    /// Like printf but in Light Green if used in Fesh Editor. Does not add a new line at end.
    let lightGreen msg = Printf.kprintf (fun s -> Fesh.PrintColor 110 255 110 s)  msg

    /// Like printf but in Green if used in Fesh Editor. Does not add a new line at end.
    let green msg = Printf.kprintf (fun s -> Fesh.PrintColor 0 190 0 s)  msg

    /// Like printf but in Dark Green if used in Fesh Editor. Does not add a new line at end.
    let darkGreen msg = Printf.kprintf (fun s -> Fesh.PrintColor 0 100 0 s)  msg

    /// Like printf but in Light Red if used in Fesh Editor. Does not add a new line at end.
    let lightRed msg = Printf.kprintf (fun s -> Fesh.PrintColor 255 180 190 s)  msg

    /// Like printf but in Red if used in Fesh Editor. Does not add a new line at end.
    let red msg =  Printf.kprintf (fun s -> Fesh.PrintColor 220 0 0 s)  msg

    /// Like printf but in Dark Red if used in Fesh Editor. Does not add a new line at end.
    let darkRed msg = Printf.kprintf (fun s -> Fesh.PrintColor 180 0 0 s)  msg

    /// Like printf but in Light Blue if in from Fesh Editor. Does not add a new line at end.
    let lightBlue msg = Printf.kprintf (fun s -> Fesh.PrintColor 170 210 230 s)  msg

    /// Like printf but in Blue if used in Fesh Editor. Does not add a new line at end.
    let blue msg = Printf.kprintf (fun s -> Fesh.PrintColor 0 0 220 s)  msg

    /// Like printf but in Yellow if used in Fesh Editor.  Does not add a new line at end.
    let yellow msg = Printf.kprintf (fun s -> Fesh.PrintColor 235 220 0 s)  msg

    /// Like printf but in Gray if used in Fesh Editor. Does not add a new line at end.
    let lightGray msg = Printf.kprintf (fun s -> Fesh.PrintColor 210 210 210 s)  msg

    /// Like printf but in Gray if used in Fesh Editor. Does not add a new line at end.
    let gray msg = Printf.kprintf (fun s -> Fesh.PrintColor 160 160 160 s)  msg

    /// Like printf but in Dark Gray if used in Fesh Editor. Does not add a new line at end.
    let darkGray msg = Printf.kprintf (fun s -> Fesh.PrintColor 100 100 100 s)  msg

    /// Like printf but in Orchid purple if used in Fesh Editor. Does not add a new line at end.
    let orchid msg = Printf.kprintf (fun s -> Fesh.PrintColor 218 112 214 s)  msg

    /// Like printf but in Purple if used in Fesh Editor. Does not add a new line at end.
    let purple msg = Printf.kprintf (fun s -> Fesh.PrintColor 128 0 128 s)  msg

    /// Like printf but in Orange if used in Fesh Editor. Does not add a new line at end.
    let orange msg = Printf.kprintf (fun s -> Fesh.PrintColor 255 140 0 s)  msg

    /// Like printf but in Cyan if used in Fesh Editor. Does not add a new line at end.
    let cyan msg = Printf.kprintf (fun s -> Fesh.PrintColor 0 150 150 s)  msg

    /// Like printf but in Random Color if used in Fesh Editor. Does not add a new line at end.
    /// Very light, white and yellow colors are excluded
    /// The colors used by subsequent calls to this functions will have very distinct hues.
    /// This is achieved by using a golden-ratio-loop and an internal cache of the last generated color.
    let colorRnd msg =
        let c = Color.randomForRhino()
        Printf.kprintf (fun s -> Fesh.PrintColor c.Red.ToInt c.Green.ToInt c.Blue.ToInt s)  msg


/// Tries to printfn with colors if running in Fesh Editor.
/// Else just normal printf
/// Adds a new line at end.
module Printfn =

    /// Print with rgb colors if running in Fesh Editor. Else just normal printf
    /// red -> green -> blue -> string -> unit
    let color red green blue msg = Printf.kprintf (fun s -> Fesh.PrintLineColor red green blue s)  msg

    /// Like printfn but in Light Green if used in Fesh Editor. Adds a new line at end.
    let lightGreen msg = Printf.kprintf (fun s -> Fesh.PrintLineColor 110 255 110 s)  msg

    /// Like printfn but in Green if used in Fesh Editor. Adds a new line at end.
    let green msg = Printf.kprintf (fun s -> Fesh.PrintLineColor 0 190 0 s)  msg

    /// Like printfn but in Dark Green if used in Fesh Editor. Adds a new line at end.
    let darkGreen msg = Printf.kprintf (fun s -> Fesh.PrintLineColor 0 100 0 s)  msg

    /// Like printfn but in Light Red if used in Fesh Editor. Adds a new line at end.
    let lightRed msg = Printf.kprintf (fun s -> Fesh.PrintLineColor  255 180 190 s)  msg

    /// Like printfn but in Red if used in Fesh Editor. Adds a new line at end.
    let red msg =  Printf.kprintf (fun s -> Fesh.PrintLineColor 220 0 0 s)  msg

    /// Like printfn but in Dark Red if used in Fesh Editor. Adds a new line at end.
    let darkRed msg = Printf.kprintf (fun s -> Fesh.PrintLineColor 180 0 0 s)  msg

    /// Like printfn but in Light Blue if in from Fesh Editor. Adds a new line at end.
    let lightBlue msg = Printf.kprintf (fun s -> Fesh.PrintLineColor 170 210 230 s)  msg

    /// Like printfn but in Blue if used in Fesh Editor. Adds a new line at end.
    let blue msg = Printf.kprintf (fun s -> Fesh.PrintLineColor 0 0 220 s)  msg

    /// Like printfn but in Yellow if used in Fesh Editor. Adds a new line at end.
    let yellow msg = Printf.kprintf (fun s -> Fesh.PrintLineColor 235 220 0 s)  msg

    /// Like printfn but in Dark Gray if used in Fesh Editor. Adds a new line at end.
    let darkGray msg = Printf.kprintf (fun s -> Fesh.PrintLineColor 100 100 100 s)  msg

    /// Like printfn but in Gray if used in Fesh Editor. Adds a new line at end.
    let gray msg = Printf.kprintf (fun s -> Fesh.PrintLineColor 160 160 160 s)  msg

    /// Like printfn but in Gray if used in Fesh Editor. Adds a new line at end.
    let lightGray msg = Printf.kprintf (fun s -> Fesh.PrintLineColor 210 210 210 s)  msg

    /// Like printfn but in Orchid (light Purple) if used in Fesh Editor. Adds a new line at end.
    let orchid msg = Printf.kprintf (fun s -> Fesh.PrintLineColor 218 112 214  s)  msg

    /// Like printfn but in Purple if used in Fesh Editor. Adds a new line at end.
    let purple msg = Printf.kprintf (fun s -> Fesh.PrintLineColor 128 0 128 s)  msg

    /// Like printfn but in Orange if used in Fesh Editor. Adds a new line at end.
    let orange msg = Printf.kprintf (fun s -> Fesh.PrintLineColor 255 140 0 s)  msg

    /// Like printfn but in Cyan if used in Fesh Editor. Adds a new line at end.
    let cyan msg = Printf.kprintf (fun s -> Fesh.PrintLineColor 0 150 150 s)  msg

    /// Like printfn but in random Color if used in Fesh Editor. Adds a new line at end.
    /// Very light, white and yellow colors are excluded
    /// The colors used by subsequent calls to this functions will have very distinct hues.
    /// This is achieved by using a golden-ratio-loop and an internal cache of the last generated color.
    let colorRnd msg =
        let c = Color.randomForRhino()
        Printf.kprintf (fun s -> Fesh.PrintLineColor c.Red.ToInt c.Green.ToInt c.Blue.ToInt s)  msg
