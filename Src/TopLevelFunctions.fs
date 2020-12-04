namespace FsEx

open System
open System.IO
open System.Runtime.CompilerServices
open System.Text
open System.Collections.Generic

type EXT = Runtime.CompilerServices.ExtensionAttribute

[<assembly:EXT>] do() // mark this assembly as extension assembly http://www.latkin.org/blog/2014/04/30/f-extension-methods-in-roslyn/



/// Static Extension methods on Exceptions to cal Excn.Raise "%A" x with F# printf string formating
/// module is set to auto open
[<AutoOpen>]
module  Exceptions =    

    type ArgumentException with
        /// Raise ArgumentException with F# printf string formating
        /// this is also the base class of ArgumentOutOfRangeException and ArgumentNullException
        [<Extension>] static member inline RaiseBase msg =  Printf.kprintf (fun s -> raise (ArgumentException(s))) msg 

    type ArgumentOutOfRangeException with
        /// Raise ArgumentOutOfRangeException with F# printf string formating
        [<Extension>] static member inline Raise msg =  Printf.kprintf (fun s -> raise (ArgumentOutOfRangeException(s))) msg 

    type ArgumentNullException with
        /// Raise ArgumentNullException with F# printf string formating
        [<Extension>] static member inline Raise msg =  Printf.kprintf (fun s -> raise (ArgumentNullException(s))) msg 

    type IndexOutOfRangeException with
        /// Raise IndexOutOfRangeException with F# printf string formating
        [<Extension>] static member inline Raise msg =  Printf.kprintf (fun s -> raise (IndexOutOfRangeException(s))) msg    

    type KeyNotFoundException with
        /// Raise KeyNotFoundException with F# printf string formating
        [<Extension>] static member inline Raise msg =  Printf.kprintf (fun s -> raise (KeyNotFoundException(s))) msg
                        
    type FileNotFoundException with
        /// Raise FileNotFoundException with F# printf string formating
        [<Extension>] static member inline Raise msg =  Printf.kprintf (fun s -> raise (FileNotFoundException(s))) msg

    type DirectoryNotFoundException with
        /// Raise DirectoryNotFoundException with F# printf string formating
        [<Extension>] static member inline Raise msg =  Printf.kprintf (fun s -> raise (DirectoryNotFoundException(s))) msg
   
[<AutoOpen>]
module IO = 
    open System.Runtime.InteropServices
    
    module private Kernel32 = 
        //https://stackoverflow.com/questions/6375599/is-this-pinvoke-code-correct-and-reliable
        //https://stackoverflow.com/questions/1689460/f-syntax-for-p-invoke-signature-using-marshalas
        [<DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)>]        
        extern [<MarshalAs(UnmanagedType.Bool)>] bool DeleteFile (string name ); // dont rename must be called 'DeleteFile'
    
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
    let rec getAllFilesByPattern (dir:string) pattern =
        seq {   yield! Directory.EnumerateFiles(dir, pattern)
                for d in Directory.EnumerateDirectories(dir) do
                    yield! getAllFilesByPattern d pattern }

/// General Utility functions
/// module is set to auto open
[<AutoOpen>]
module  Util = 
    
    /// a quick way to throw an exception.
    /// for use in temporary scripts when you are too lazy to do a proper exception.
    let inline fail() = failwith "Quick fail (inner exception should show more helpful message)" 
    
    /// throws an exception with 'msg' as Error message if 'value' is false.
    /// this function is usefull to follow up on any methods that return booleans indication sucess or failure
    let inline failIfFalse (msg:string) (value :bool) :unit = 
        if not value then failwithf "failIfFalse: %s " msg 
    
    /// throws an exception with 'msg' as Error message if 'value' is null.
    /// this function is usefull to doing many null checks without adding lots if clauses and lots of indenting
    let inline failIfNull (msg:string) (value :'T when 'T: null) : unit = 
        match value with 
        | null -> ArgumentNullException.Raise "<null> in FsEx.Util.failIfNull: %s" msg  
        | _ -> ()

    /// throws an exception with 'msg' as Error message if string is null or empty    
    let inline failIfNullOrEmpty (failMsg:string) (stringToCheck :string) :unit = 
        match stringToCheck with 
        | null -> ArgumentNullException.Raise "Null string in FsEx.Util.failIfNullOrEmpty: %s" failMsg 
        | ""   -> ArgumentException.RaiseBase "Empty string in FsEx.Util.failIfNullOrEmpty: %s" failMsg 
        | _ -> ()        
       
        /// throws an exception with 'msg' as Error message if string is null or empty    
    let inline failIfEmptyGuid (failMsg:string) (guidToCheck :Guid) :unit = 
        if guidToCheck = Guid.Empty then 
            ArgumentException.RaiseBase "Empty Guid in  FsEx.Util.failIfEmptyGuid: %s" failMsg
 
    /// retuns false if the value is null.
    /// the opposit of isNull
    let inline notNull (value :'T when 'T: null) = // Fsharp core does it like this too. don't use Obejct.RefrenceEquals (because of Generics)
        match value with 
        | null -> false  
        | _ -> true    
    
    /// retuns false if the Guid is Empty.
    let inline notEmptyGuid (g :Guid) = 
        g <> Guid.Empty
    
    /// retuns true if the Guid is Empty.
    let inline isEmptyGuid (g :Guid) = 
        g = Guid.Empty

    /// Returns maybeNullValue if it is NOT null, else alternativeValue. 
    let inline ifNull (alternativeValue:'T) (maybeNullValue:'T)  = match maybeNullValue with null -> alternativeValue | _ -> maybeNullValue  

    /// Returns the value on the left unless it is null, then it returns the value on the right.
    let inline (|?) (a:'T) (b:'T)  = // a more fancy version: https://gist.github.com/jbtule/8477768#file-nullcoalesce-fs
        match a with 
        | null -> b  
        | _    -> a // if Object.ReferenceEquals(a, null) then b else a   

    /// use |>! instead
    /// Apply function, like |> , but ignore result. 
    /// Return original input
    /// let inline (|>>) x f =  f x |> ignore ; x 
    [<Obsolete>]
    let inline (|>>) x f =  f x |> ignore ; x 

    /// Apply function, like |> , but ignore result. 
    /// Return original input
    /// let inline (|>!) x f =  f x |> ignore ; x 
    /// be aware of correct indenting see:
    /// https://stackoverflow.com/questions/64784154/indentation-change-after-if-else-expression-not-taken-into-account
    let inline (|>!) x f =  f x |> ignore ; x  //https://twitter.com/GoswinR/status/1316988132932407296

    /// Get first element of Triple (Tuple of three elements)
    let inline t1 (a,_,_) = a

    /// Get second element of Triple (Tuple of three elements)
    let inline t2 (_,b,_) = b

    /// Get third element of Triple (Tuple of three elements)
    let inline t3 (_,_,c) = c    


    /// Converts negative indices to positive ones
    /// correctet results from -len up to len-1
    /// e.g.: -1 is  last item .
    /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
    let inline negIdx i len =
        let ii =  if i < 0 then len+i else i
        if ii<0 || ii >= len then IndexOutOfRangeException.Raise "Util.negIdx: Bad index %d for items count %d." i len
        ii
    
    /// Any int will give a valid index for given collection size.
    /// Converts negative indices to positive ones and loops to start after last index is reached
    /// returns a valid index for a colcction of 'len' items for any integer
    let inline negIdxLooped i len =        
        let t = i % len
        if t >= 0 then t 
        else           t + len 

    /// Any int will give a valid index for given collection size.
    /// division remainder will be used i % len
    /// e.g.: -1 is  last item 
    /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
    let inline saveIdx i len = negIdxLooped i len 
      
    
    /// If condition is true return f(x) else just x
    let inline ifDo condition (f:'T->'T)  (x:'T) = 
        if condition then f x else x
    
      

    /// generic parser that infers desired return type 
    let inline tryParse<'a when 'a: (static member TryParse: string * byref<'a> -> bool)> x =
        // https://twitter.com/mvsmal/status/1317020301046259712
        let mutable res = Unchecked.defaultof<'a> 
        if (^a: (static member TryParse: string * byref<'a> -> bool) (x, &res)) 
        then Some res  
        else None 
    
    /// generic parser that infers desired return type or fails with ArgumentException 
    let inline parse<'a when 'a: (static member TryParse: string * byref<'a> -> bool)> x =
        let mutable res = Unchecked.defaultof<'a> 
        if (^a: (static member TryParse: string * byref<'a> -> bool) (x, &res)) 
        then res  
        else ArgumentException.RaiseBase "Failed to parse %A to a %A" x (res.GetType())



/// Shadows the ignore function to only accept structs
/// This is to prevent accidetially ignoring partially aplied functions that would return struct
module SaveIgnore = 
    
    /// This ignore only work on Value types, 
    /// Objects and functions need to be ignored with ignoreObj
    /// This is to prevent accidetially ignoring partially aplied functions that would return struct
    let inline ignore (x:'T when 'T: struct)=()

    /// Ignores any object (and struct)
    /// For structs use 'ignore'
    let inline ignoreObj (x:obj)=()

/// Functions (and operator !++) to deal with integer ref objects
module IntRef = 

    /// Increment a ref cell and return new incremented integer value
    [<Obsolete>]
    let inline (!++)  i = incr i; !i 

    /// Increment a ref cell by two
    [<Obsolete>]
    let inline incr2 i = i := !i+2

    /// Increment a ref cell by three 
    [<Obsolete>]
    let inline incr3 i = i := !i+3

    /// Increment a ref cell by four
    [<Obsolete>]
    let inline incr4 i = i := !i+4

    /// Increment a ref cell by a given int
    let inline incrBy i (x:int) = i := !i + x

    //let inline incrByR (x:int) i = i := !i + x // useful ?
    
    /// Decrement a ref cell by one
    let inline decr  i = i := !i-1 
    
    /// Decrement a ref cell by two
    [<Obsolete>]
    let inline decr2 i = i := !i-2
    
    /// Decrement a ref cell by three
    [<Obsolete>]
    let inline decr3 i = i := !i-3
    
    /// Decrement a ref cell by four
    [<Obsolete>]
    let inline decr4 i = i := !i-4
    
    /// Decrement a ref cell by a given int    
    let inline decrBy i (x:int) = i := !i - x    
    
    //let inline decrByR (x:int) i = i := !i - x           // useful ?
    
    /// set ref cell to given int if it is bigger than current value
    let inline setMax i (x:int) = if x > !i then i := x

    /// set ref cell to given int if it is smaller than current value
    let inline setMin i (x:int) = if x < !i then i := x

/// Functions to deal with float ref objects
module FloatRef = 
        
    /// Increment a ref cell by a given int
    let inline incrBy i (x:float) = i := !i + x
        
    /// Decrement a ref cell by a given int    
    let inline decrBy i (x:float) = i := !i - x    
        
    //let inline decrByR (x:int) i = i := !i - x           // useful ?
        
    /// set ref cell to given int if it is bigger than current value
    let inline setMax i (x:float) = if x > !i then i := x
    
    /// set ref cell to given int if it is smaller than current value
    let inline setMin i (x:float) = if x < !i then i := x

type FSharpFuncUtil = 
    // from https://blog.paranoidcoding.com/2010/07/27/converting-system-func-lt-t1-tn-gt-to-fsharpfunc-lt-t-tresult-gt.html

    static member ToFSharpFunc<'a> (func : System.Func<'a>) = fun () -> func.Invoke()

    static member ToFSharpFunc<'a,'b> (func : System.Func<'a,'b>) = fun x -> func.Invoke(x)

    static member ToFSharpFunc<'a,'b,'c> (func : System.Func<'a,'b,'c>) = fun x y -> func.Invoke(x,y)

    static member ToFSharpFunc<'a,'b,'c,'d> (func : System.Func<'a,'b,'c,'d>) = fun x y z -> func.Invoke(x,y,z)

    static member ToFSharpFunc<'a> (func : System.Action) = fun () -> func.Invoke()
 
    static member ToFSharpFunc<'a> (func : System.Action<'a>) = fun x -> func.Invoke(x)

    static member Create<'a,'b> (func : System.Func<'a,'b>) = FSharpFuncUtil.ToFSharpFunc func

    static member Create<'a,'b,'c> (func : System.Func<'a,'b,'c>) = FSharpFuncUtil.ToFSharpFunc func

    static member Create<'a,'b,'c,'d> (func : System.Func<'a,'b,'c,'d>) = FSharpFuncUtil.ToFSharpFunc func
