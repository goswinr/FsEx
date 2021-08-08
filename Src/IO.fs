namespace FsEx

open System
open System.IO
open System.Text
open System.Threading


//[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] //need this so doesn't hide IO namespace in C# assemblies 
module IO = 

    /// Raises an FileNotFoundException if the file path does not exist
    let checkIfFileExists s = 
        if not (IO.File.Exists s) then  raise (FileNotFoundException("File missing or path worng: '" + s + "'"))
     
     /// Raises an DirectoryNotFoundException if the directory path does not exist
    let checkIfDirectoryExists s = 
        if not (IO.Directory.Exists s) then  raise (DirectoryNotFoundException("Directory missing or path worng: '" + s + "'"))     
    
    /// Returns all files in folder and subfolders
    /// Ignores all errors and moves on to next folder
    let rec getAllFiles (dir:string) = 
        seq { if Directory.Exists dir then 
                let files = try Directory.GetFiles(dir) with _ -> [||]
                Array.sortInPlace files
                yield! files
                let dirs = try Directory.GetDirectories(dir) with _ -> [||]
                Array.sortInPlace dirs
                for subdir in dirs do 
                    yield! getAllFiles subdir }

    /// Returns all files in folder and subfolders that fit pattern (e.g. "*.pdf" ) 
    /// may fail on IOExceptions
    let rec getAllFilesByPattern (dir:string) (pattern:string) =
        seq {   yield! Directory.EnumerateFiles(dir, pattern)
                for d in Directory.EnumerateDirectories(dir) do
                    yield! getAllFilesByPattern d pattern }

    /// Returns the path to the current Desktop
    /// Environment.GetFolderPath(Environment.SpecialFolder.Desktop)
    let desktop = 
        Environment.GetFolderPath(Environment.SpecialFolder.Desktop)   
    

    /// Determines a text file's encoding by analyzing its byte order mark (BOM).
    /// Returns None  when detection of the text file's endianness fails.( might be ASCII or UTF-8 without BOM)
    /// (Encoding.Unicode = UTF-16LE)
    let getEncoding(filename:string)=
        // https://stackoverflow.com/a/19283954/969070         
        let bom = Array.zeroCreate 4
        use  file = new FileStream(filename, FileMode.Open, FileAccess.Read) // Read the BOM       
        if file.Read(bom, 0, 4) <> 4 then None else         
            // Analyze the BOM
            if   bom.[0] = 0x2buy && bom.[1] = 0x2fuy && bom.[2] = 0x76uy then  Some  Encoding.UTF7
            elif bom.[0] = 0xefuy && bom.[1] = 0xbbuy && bom.[2] = 0xbfuy then  Some  Encoding.UTF8
            elif bom.[0] = 0xffuy && bom.[1] = 0xfeuy && bom.[2] = 0uy && bom.[3] = 0uy then  Some  Encoding.UTF32 //UTF-32LE
            elif bom.[0] = 0xffuy && bom.[1] = 0xfeuy then  Some  Encoding.Unicode; //UTF-16LE
            elif bom.[0] = 0xfeuy && bom.[1] = 0xffuy then  Some  Encoding.BigEndianUnicode; //UTF-16BE
            elif bom.[0] = 0uy    && bom.[1] = 0uy    && bom.[2] = 0xfeuy && bom.[3] = 0xffuy then Some  ( UTF32Encoding(true, true) :> Encoding)   //UTF-32BE
            else 
                // We actually have no idea what the encoding is if we reach this point, might be ASCII or UTF-8 without BOM
                None

    
    /// Reads and Writes with Lock, 
    /// Optionally only once after a delay in which it might be called several times
    /// using Text.Encoding.UTF8
    type SaveReadWriter (path:string)= 
        // same class also exist in  FsEx.Wpf , TODO keep in sync!
       
        let counter = ref 0L // for atomic writing back to file
       
        let lockObj = new Object()
        
        /// calls IO.File.Exists(path)
        member this.FileExists() = IO.File.Exists(path)


        /// Save reading.
        /// Ensures that no writing happens while reading.
        member this.ReadAllText () : string =
            // lock is using Monitor class : https://github.com/dotnet/fsharp/blob/6d91b3759affe3320e48f12becbbbca493574b22/src/fsharp/FSharp.Core/prim-types.fs#L4793
            lock lockObj (fun () -> IO.File.ReadAllText(path, Text.Encoding.UTF8))            
                
    
        /// Save reading.
        /// Ensures that no writing happens while reading.
        member this.ReadAllLines () : string[] =
            // lock is using Monitor class : https://github.com/dotnet/fsharp/blob/6d91b3759affe3320e48f12becbbbca493574b22/src/fsharp/FSharp.Core/prim-types.fs#L4793
            lock lockObj (fun () -> IO.File.ReadAllLines(path, Text.Encoding.UTF8))
        
        
        /// File will be written async and with a Lock.
        /// If it fails an Error is printed to the Error stream via eprintfn.
        /// Ensures that no reading happens while writing.
        member this.WriteAsync (text) =        
            async{
                lock lockObj (fun () -> // lock is using Monitor class : https://github.com/dotnet/fsharp/blob/6d91b3759affe3320e48f12becbbbca493574b22/src/fsharp/FSharp.Core/prim-types.fs#L4793
                    try  IO.File.WriteAllText(path,text, Text.Encoding.UTF8)
                    // try & with is needed because exceptions on threadpool cannot be caught otherwise !!
                    with ex ->  eprintfn "FsEx.IO.SaveWriter.WriteAsync failed with: %A \r\n while writing to %s:\r\n%A" ex path text // use %A to trimm long text        
                    )       
                } |> Async.Start
    
        /// File will be written async and with a Lock.
        /// If it fails an Error is printed to the Error stream via eprintfn
        /// Ensures that no reading happens while writing.
        member this.WriteAllLinesAsync (texts) =        
            async{
                lock lockObj (fun () -> // lock is using Monitor class : https://github.com/dotnet/fsharp/blob/6d91b3759affe3320e48f12becbbbca493574b22/src/fsharp/FSharp.Core/prim-types.fs#L4793
                    try  IO.File.WriteAllLines(path,texts, Text.Encoding.UTF8)
                    // try & with is needed because exceptions on threadpool cannot be caught otherwise !!
                    with ex ->  eprintfn "FsEx.IO.SaveWriter.WriteAllLinesAsync failed with: %A \r\n while writing to %s:\r\n%A" ex path texts // use %A to trimm long text        
                    )       
                } |> Async.Start
   
        /// GetString will be called in sync on calling thread, but file will be written async.
        /// Only if after the delay the counter value is the same as before. 
        /// That means no more recent calls to this function have been made during the delay.
        /// If other calls to this function have been made then only the last call will be written as file.
        /// If it fails an Error is printed to the Error stream via eprintfn.
        /// Also ensures that no reading happens while writing.
        member this.WriteIfLast ( getText: unit->string, delayMillisSeconds:int) =
            async{
                let k = Interlocked.Increment counter
                do! Async.Sleep(delayMillisSeconds) // delay to see if this is the last of many events (otherwise there is a noticable lag in dragging window around, for example, when saving window position)
                if !counter = k then //k > 2L &&   //do not save on startup && only save last event after a delay if there are many save events in a row ( eg from window size change)(ignore first two event from creating window)
                    try 
                        let text = getText() 
                        this.WriteAsync (text) // this should never fail since exeptions are caught inside 
                    with ex -> 
                        // try & with is needed because exceptions on threadpool cannot be caught otherwise !!
                        eprintfn "FsEx.IO.SaveWriter.WriteIfLast: getText() for path (%s) failed with: %A" path ex                 
                } |> Async.StartImmediate            

