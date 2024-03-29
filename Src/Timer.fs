﻿namespace FsEx

open System


/// A performance timer that also measures Garbage Collection Generations.
/// includes nice formatting of ms , sec, and minutes
/// Similar to the #time;; statement built in to FSI
/// The timer starts immediately when created
type Timer() = 

    // taken from FsiTimeReporter at https://github.com/dotnet/fsharp/blob/master/src/fsharp/fsi/fsi.fs#L183

    let numGC = System.GC.MaxGeneration

    let formatGCs prevGC = 
        prevGC
        |> Array.fold (fun (i,txt) _ -> i+1, sprintf "%s  G%d: %d" txt i (System.GC.CollectionCount(i) - prevGC.[i]) ) (0," ; ") //"GC:")
        |> snd

    let formatMilliSeconds ms = 
        if ms < 0.001 then "less than 0.001 ms"
        elif ms < 1.0 then sprintf "%.3f ms" (ms)       //less than 1  millisecond
        elif ms < 10. then sprintf "%.2f ms" (ms)       //less than 10 millisecond
        elif ms < 1e3 then sprintf "%.1f ms" ms         //less than 1  second
        elif ms < 1e4 then sprintf "%.2f sec" (ms/1e3)  //less than 10 second
        elif ms < 6e4 then sprintf "%.1f sec" (ms/1e3)  //less than 1 min
        else      sprintf "%.0f min %.0f sec" (Math.Floor (ms/6e4)) ((ms % 6e4)/1e3)

    let ticWithGC (sw:Diagnostics.Stopwatch) (kGC:int[]) = 
        sw.Reset();  GC.Collect() ;  GC.WaitForPendingFinalizers()
        for i=0 to numGC do kGC.[i] <- GC.CollectionCount(i) // reset GC counter base
        sw.Start()

    let tocWithGC (sw:Diagnostics.Stopwatch) countGC = 
        sw.Stop()
        let el = sw.Elapsed
        let txt = sprintf "%s, %s" (formatMilliSeconds el.TotalMilliseconds) (formatGCs countGC)
        for i=0 to numGC do countGC.[i] <- GC.CollectionCount(i) // reset GC counter base
        sw.Reset()
        sw.Start()
        txt

    let tocNoGC (sw:Diagnostics.Stopwatch) = 
        sw.Stop()
        let el = sw.Elapsed
        let txt = formatMilliSeconds el.TotalMilliseconds
        sw.Reset()
        GC.Collect()
        GC.WaitForPendingFinalizers()
        sw.Start()
        txt

    let kGC = [| for i in 0 .. numGC -> GC.CollectionCount(i) |]


    let stopWatch = new Diagnostics.Stopwatch()

    do
        ticWithGC stopWatch kGC // start stopwatch immediately in constructor

    /// Returns time and Garbage Collector information
    /// since last tic (or toc) as string, then reset and restart timer
    member this.TocEx = tocWithGC stopWatch kGC

    /// Returns only time since last tic (or toc) as string, then reset and restart timer
    member this.Toc = tocNoGC stopWatch

    /// Reset and start Timer
    member this.Tic() =  ticWithGC stopWatch kGC

    /// Stops Timer
    member this.Stop() =  stopWatch.Stop()


    //static member val InstanceStartup = Timer() // An instance of a timer to be used to measure startup performance
