namespace FsEx

open System
open System.IO
open System.Threading


//[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] //need this so doesn't hide IO namespace in C# assemblies 
module IO = 
    
    
    module private Kernel32 = 
        open System.Runtime.InteropServices

        //https://stackoverflow.com/questions/6375599/is-this-pinvoke-code-correct-and-reliable
        //https://stackoverflow.com/questions/1689460/f-syntax-for-p-invoke-signature-using-marshalas
        [<DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)>]        
        extern [<MarshalAs(UnmanagedType.Bool)>] bool DeleteFile (string name ); // dont rename! must be called 'DeleteFile'


        //https://christoph.ruegg.name/blog/loading-native-dlls-in-fsharp-interactive.html        
        [<DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)>]
        extern IntPtr LoadLibrary(string lpFileName); // to load native dlls

        //https://stackoverflow.com/a/2445558/969070
        [<DllImport("kernel32.dll", CharSet = CharSet.Unicode)>]
        extern bool FreeLibrary(IntPtr hModule);

    /// To load a native (C++) dll in FSI. Because #r statment does not work for native dlls in fsi
    /// If you were writing this in a long running process where the DLL is used in a well defined section only, 
    /// then you'd better unload the library once no longer needed with the symmetric FreeLibrary routine on kernel32.dll. 
    /// But for quick experiments in F# Interactive it's probably fine.
    let loadLibrary (fullPath:string)= 
        if IO.File.Exists(fullPath) then 
            Kernel32.LoadLibrary(fullPath) |> ignore<IntPtr>
        else 
            FileNotFoundException.Raise "FsEx.IO.loadNativeDll cant find file %s" fullPath     

    
    /// Removes the blocking of dll files from untrusted sources, e.g. the internet
    /// calls pInvoke  kernel32.dll DeleteFile() to remove Zone.Identifier
    /// Raises an exception if file does not exist
    /// Returns true if Zone.Identifier where removed from the file stream. Else false
    let unblockFile(filePath:string ) : bool =
        if IO.File.Exists(filePath) then 
            Kernel32.DeleteFile(filePath + ":Zone.Identifier") 
        else 
            FileNotFoundException.Raise "FsEx.IO.unblock cant find file %s" filePath    

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

    /// returns the path to the current Desktop
    /// Environment.GetFolderPath(Environment.SpecialFolder.Desktop)
    let desktop = 
        Environment.GetFolderPath(Environment.SpecialFolder.Desktop)
    
     
    
    /// Reads and Writes with Lock, 
    /// Optionally only once after a delay in which it might be called several times
    type SaveReadWriter (path:string)= 
        // similar class also exist in FsEx , FsEx.Wpf and Seff
       
        let counter = ref 0L // for atomic writing back to file
       
        let lockObj = new Object()
        
        /// calls IO.File.Exists(path)
        member this.FileExists() = IO.File.Exists(path)

        /// Save reading
        /// Ensures that no writing happens while reading
        member this.ReadAllText () : string =
            // lock is using Monitor class : https://github.com/dotnet/fsharp/blob/6d91b3759affe3320e48f12becbbbca493574b22/src/fsharp/FSharp.Core/prim-types.fs#L4793
            try  
                lock lockObj (fun () -> IO.File.ReadAllText(path))
            with ex ->  
                failwithf "SaveWriter.Read failed while reading:\r\n%s\r\n with: %A" path ex // use %A to trimm long text 
    
        /// Save reading
        /// Ensures that no writing happens while reading
        member this.ReadAllLines () : string[] =
            // lock is using Monitor class : https://github.com/dotnet/fsharp/blob/6d91b3759affe3320e48f12becbbbca493574b22/src/fsharp/FSharp.Core/prim-types.fs#L4793
            try  
                lock lockObj (fun () -> IO.File.ReadAllLines(path))
            with ex ->  
                failwithf "SaveWriter.Read failed while reading:\r\n%s\r\n with: %A" path ex // use %A to trimm long text 

        /// File will be written async and with a Lock.
        /// If it fails an Error is printed to the Error stream via eprintfn
        /// Ensures that no reading happens while writing
        member this.WriteAsync (text) =        
            async{
                lock lockObj (fun () -> // lock is using Monitor class : https://github.com/dotnet/fsharp/blob/6d91b3759affe3320e48f12becbbbca493574b22/src/fsharp/FSharp.Core/prim-types.fs#L4793
                    try  IO.File.WriteAllText(path,text)
                    with ex ->  eprintfn "SaveWriter.WriteAsync failed with: %A \r\n while writing to %s:\r\n%A" ex path text // use %A to trimm long text        
                    )       
                } |> Async.Start
    
        /// File will be written async and with a Lock.
        /// If it fails an Error is printed to the Error stream via eprintfn
        /// Ensures that no reading happens while writing
        member this.WriteAllLinesAsync (texts) =        
            async{
                lock lockObj (fun () -> // lock is using Monitor class : https://github.com/dotnet/fsharp/blob/6d91b3759affe3320e48f12becbbbca493574b22/src/fsharp/FSharp.Core/prim-types.fs#L4793
                    try  IO.File.WriteAllLines(path,texts)
                    with ex ->  eprintfn "SaveWriter.WriteAllLinesAsync failed with: %A \r\n while writing to %s:\r\n%A" ex path texts // use %A to trimm long text        
                    )       
                } |> Async.Start
   
        /// GetString will be called in sync on calling thread, but file will be written async.
        /// Only if after the delay the counter value is the same as before. 
        /// That means no more recent calls to this function have been made during the delay.
        /// If other calls to this function have been made then only the last call will be written as file
        /// If it fails an Error is printed to the Error stream via eprintfn
        /// Also ensures that no reading happens while writing
        member this.WriteIfLast ( getText: unit->string, delayMillisSeconds:int) =
            async{
                let k = Interlocked.Increment counter
                do! Async.Sleep(delayMillisSeconds) // delay to see if this is the last of many events (otherwise there is a noticable lag in dragging window around, for example, when saving window position)
                if !counter = k then //k > 2L &&   //do not save on startup && only save last event after a delay if there are many save events in a row ( eg from window size change)(ignore first two event from creating window)
                    try 
                        let text = getText()               
                        this.WriteAsync (text) // this should never fail since exeptions are caught inside 
                    with ex -> 
                        eprintfn "SaveWriter.WriteIfLast: getText() for path (%s) failed with: %A" path ex                 
                } |> Async.StartImmediate            

