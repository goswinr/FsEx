namespace FsEx

open System
open System.IO

[<RequireQualifiedAccess>]
module Kernel32 = 

    /// For Exceptions that happen while calling native kernel32 functions
    type Kernel32Exception(s)= 
        inherit Exception(s)
        static member inline Raise msg =  Printf.kprintf (fun s -> raise (Kernel32Exception(s))) msg

    module private K32 = 
        open System.Runtime.InteropServices

        // https://stackoverflow.com/questions/6375599/is-this-pinvoke-code-correct-and-reliable
        // https://stackoverflow.com/questions/1689460/f-syntax-for-p-invoke-signature-using-marshalas
        // PCWSTR (immutable string) = LPWSTR (mutable string)
        // Passing a System.String to a LPCWSTR argument is fine!
        // https://stackoverflow.com/questions/21659751/pcwstr-vs-lpwstr


        // https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-deletefilew
        // BOOL DeleteFileW(LPCWSTR lpFileName);
        [<DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)>]
        extern bool DeleteFileW (string lpFileName );

        //extern [<MarshalAs(UnmanagedType.Bool)>] bool DeleteFile (string name );

        // https://christoph.ruegg.name/blog/loading-native-dlls-in-fsharp-interactive.html
        //[<DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)>]
        //extern IntPtr LoadLibrary(string lpFileName); // to load native dlls

        // to load native dlls
        // https://docs.microsoft.com/en-us/windows/win32/api/libloaderapi/nf-libloaderapi-loadlibraryw
        // HMODULE LoadLibraryW ( LPCWSTR lpLibFileName);
        // Passing a System.String to a LPCWSTR argument is fine!
        // https://stackoverflow.com/questions/21659751/pcwstr-vs-lpwstr
        [<DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)>]
        extern IntPtr LoadLibraryW(string lpLibFileName);

        // to unload native dlls
        // https://stackoverflow.com/a/2445558/969070
        // https://docs.microsoft.com/en-us/windows/win32/api/libloaderapi/nf-libloaderapi-freelibrary
        // BOOL FreeLibrary( HMODULE hLibModule );
        [<DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)>]
        extern bool FreeLibrary(IntPtr hLibModule);

        // the return value is an opaque pointer that can be passed to RemoveDllDirectory
        // https://docs.microsoft.com/en-us/windows/win32/api/libloaderapi/nf-libloaderapi-adddlldirectory
        // DLL_DIRECTORY_COOKIE AddDllDirectory( PCWSTR NewDirectory);
        // PCWSTR (immutable string) = LPWSTR (mutable string)
        // https://stackoverflow.com/questions/21659751/pcwstr-vs-lpwstr
        [<DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)>]
        extern IntPtr AddDllDirectory(string NewDirectory);

        // BOOL RemoveDllDirectory(DLL_DIRECTORY_COOKIE Cookie);
        [<DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)>]
        extern bool RemoveDllDirectory(IntPtr Cookie);

        let loadedLibs = Collections.Generic.Dictionary()
        let loadDirs   = Collections.Generic.Dictionary()

    /// To load a native (C++) dll in FSI. Because #r statement does not work for native dlls in fsi
    /// Loads the specified module into the address space of the calling process. The specified module may cause other modules to be loaded.
    /// If you were writing this in a long running process where the DLL is used in a well defined section only,
    /// then you'd better unload the library once no longer needed with the symmetric FreeLibrary routine on kernel32.dll.
    /// But for quick experiments in F# Interactive it's probably fine.
    let loadLibrary (path:string) : unit= 
        if IO.File.Exists(path) then  // see https://github.com/dotnet/fsharp/issues/10136
            let cleanPath = path.Replace("/","\\")
            let ptr = K32.LoadLibraryW(cleanPath)
            K32.loadedLibs.[cleanPath] <- ptr
        else
            FileNotFoundException.Raise "FsEx.Kernel32.loadLibrary cant find file %s" path

    /// To unload a native (C++) dll in FSI. Because #r statement does not work for native dlls in fsi
    let freeLibrary (path:string) : unit = 
        let cleanPath = path.Replace("/","\\")
        if K32.loadedLibs.ContainsKey(cleanPath) then
            let ok = K32.FreeLibrary(K32.loadedLibs.[cleanPath])
            if not ok then Kernel32Exception.Raise "FsEx.Kernel32.freeLibrary handle is no longer valid. Is the Library already unloaded? %s" cleanPath
        else
            Kernel32Exception.Raise "FsEx.Kernel32.freeLibrary cant find pointer for file to unload, is it not loaded? %s" cleanPath

    /// To help load a native (C++) dll in FSI. Because #r statement does not work for native dlls in fsi
    /// Adds a directory to the process DLL search path.
    /// An absolute path to the directory to add to the search path.
    /// For example, to add the directory Dir2 to the process DLL search path, specify \Dir2
    let addDllDirectory (path:string) : unit= 
        if IO.Directory.Exists(path) then
            let cleanPath = path.Replace("/","\\")
            let ptr = K32.AddDllDirectory(cleanPath)
            K32.loadDirs.[cleanPath] <- ptr
        else
            DirectoryNotFoundException.Raise "FsEx.Kernel32.addDllDirectory cant find directory %s" path

    /// Removes a directory that was added to the process DLL search path by using AddDllDirectory.
    let removeDllDirectory (path:string) : unit = 
        let cleanPath = path.Replace("/","\\")
        if K32.loadDirs.ContainsKey(cleanPath) then
            let ok = K32.RemoveDllDirectory(K32.loadDirs.[cleanPath])
            if not ok then Kernel32Exception.Raise "FsEx.Kernel32.removeDllDirectory handle is no longer valid. Is the Folder already unloaded? %s" cleanPath
        else
            Kernel32Exception.Raise "FsEx.Kernel32.removeDllDirectory cant find pointer for folder to unload, is it not loaded? %s" cleanPath

    /// Removes the blocking of dll files from untrusted sources, e.g. the Internet
    /// calls pInvoke  kernel32.dll DeleteFile() to remove Zone.Identifier
    /// Raises an exception if file does not exist
    /// Returns true if Zone.Identifier where removed from the file stream. Else false
    let unblockFile(filePath:string ) : bool = 
        if IO.File.Exists(filePath) then
            let cleanPath = filePath.Replace("/","\\")
            K32.DeleteFileW(cleanPath + ":Zone.Identifier")
        else
            FileNotFoundException.Raise "FsEx.Kernel32.unblock cant find file %s" filePath


