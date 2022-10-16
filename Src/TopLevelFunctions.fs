namespace FsEx

open System
open System.IO
open System.Collections.Generic




/// This module is set to auto open when opening FsEx namespace.
/// Static Extension methods on Exceptions to cal Exception.Raise "%A" x with F# printf string formatting
[<AutoOpen>] 
module AutoOpenExtensionsExceptions = 

    // type FsExStringException is now defined in String Module
    // TODO add argument checking via https://stackoverflow.com/questions/73284201/f-kprintf-missing-warning-about-about-redundant-arguments

    type ArgumentException with
        /// Raise ArgumentException with F# printf string formatting
        /// this is also the base class of ArgumentOutOfRangeException and ArgumentNullException
        static member inline RaiseBase msg =  Printf.kprintf (fun s -> raise (ArgumentException(s))) msg

    type ArgumentOutOfRangeException with
        /// Raise ArgumentOutOfRangeException with F# printf string formatting
        static member inline Raise msg =  Printf.kprintf (fun s -> raise (ArgumentOutOfRangeException(s))) msg

    type ArgumentNullException with
        /// Raise ArgumentNullException with F# printf string formatting
        static member inline Raise msg =  Printf.kprintf (fun s -> raise (ArgumentNullException(s))) msg

    type IndexOutOfRangeException with
        /// Raise IndexOutOfRangeException with F# printf string formatting
        static member inline Raise msg =  Printf.kprintf (fun s -> raise (IndexOutOfRangeException(s))) msg

    type KeyNotFoundException with
        /// Raise KeyNotFoundException with F# printf string formatting
        static member inline Raise msg =  Printf.kprintf (fun s -> raise (KeyNotFoundException(s))) msg

    type FileNotFoundException with
        /// Raise FileNotFoundException with F# printf string formatting
        static member inline Raise msg =  Printf.kprintf (fun s -> raise (FileNotFoundException(s))) msg

    type DirectoryNotFoundException with
        /// Raise DirectoryNotFoundException with F# printf string formatting
        static member inline Raise msg =  Printf.kprintf (fun s -> raise (DirectoryNotFoundException(s))) msg


/// This module is set to auto open when opening FsEx namespace.
/// General Utility functions
[<AutoOpen>] 
module AutoOpenUtil = 

    type Collections.IList with 
        /// Returns the last element of a non generic Collections.IList
        member this.LastObj =  this.[this.Count-1] 

    type Collections.Generic.IList<'T> with 
        /// Returns the last element of a generic Collections.Generic.IList<'T>
        member this.Last =  this.[this.Count-1] 
    
    type Collections.Generic.List<'T> with 
        /// Applies a function to all elements of a generic Collections.Generic.IList<'T>
        /// Returns a new List<'T> also called ResizeArray<'T> in F#
        /// Just does: this.ConvertAll (System.Converter mapping) 
        member this.Map(mapping: 'T -> 'U)  = this.ConvertAll (System.Converter mapping)


    /// A quick way to throw an exception.
    /// for use in temporary scripts when you are too lazy to do a proper exception.
    let inline fail() = 
        raise <| Exception "Quick fail (inner exception should show more helpful message)"
    
    /// Throws an exception with 'msg' as Error message if the projection called with 'value' as argument returns true.
    /// This function is useful to check values in piping
    let inline failIf (projection:'T -> bool) (failMsg:string) (value:'T) : 'T = 
        if projection value then raise <| Exception( "FsEx.failIf: " + failMsg ) else value  
    
    
    /// Throws an exception with 'msg' as Error message if 'value' is false.
    /// This function is useful to follow up on any methods that return booleans indication success or failure
    let inline failIfFalse (failMsg:string) (value :bool) : unit = 
        if not value then raise <| Exception( "FsEx.failIfFalse: " + failMsg )

    /// Throws an exception with 'msg' as Error message if 'value' is null.
    /// This function is useful to doing many null checks without adding lots if clauses and lots of indenting
    let inline failIfNull (failMsg:string) (value :'T when 'T: null) : unit = 
        match value with
        | null -> ArgumentNullException.Raise "<null> in FsEx.Util.failIfNull: %s" failMsg
        | _ -> ()

    /// Throws an exception with 'msg' as Error message if string is null or empty
    let inline failIfNullOrEmpty (failMsg:string) (stringToCheck :string) :unit = 
        match stringToCheck with
        | null -> ArgumentNullException.Raise "Null string in FsEx.Util.failIfNullOrEmpty: %s" failMsg
        | ""   -> ArgumentException.RaiseBase "Empty string in FsEx.Util.failIfNullOrEmpty: %s" failMsg
        | _ -> ()

    /// Throws an exception with 'msg' as Error message if string is null or empty
    let inline failIfEmptyGuid (failMsg:string) (guidToCheck :Guid) :unit = 
        if guidToCheck = Guid.Empty then
            ArgumentException.RaiseBase "Empty Guid in  FsEx.Util.failIfEmptyGuid: %s" failMsg

    /// Returns false if the value is null.
    /// The opposite of isNull
    let inline notNull (value :'T when 'T: null) = // FSharp core does it like this too. don't use Object.ReferenceEquals (because of Generics)
        match value with
        | null -> false
        | _ -> true

    /// Returns false if the Guid is Empty.
    /// g <> Guid.Empty
    let inline notEmptyGuid (g :Guid) = 
        g <> Guid.Empty

    /// Returns true if the Guid is Empty.
    /// g = Guid.Empty
    let inline isEmptyGuid (g :Guid) = 
        g = Guid.Empty

    /// Returns maybeNullValue if it is NOT null, else alternativeValue.
    let inline ifNull (alternativeValue:'T) (maybeNullValue:'T)  = 
        match maybeNullValue with
        |null -> alternativeValue
        | _   -> maybeNullValue

    /// Null coalescing:
    /// Returns the value on the left unless it is null, then it returns the value on the right.
    let inline ( |? ) (a:'T) (b:'T)  = 
        // a more fancy version: https://gist.github.com/jbtule/8477768#file-nullcoalesce-fs
        match a with
        | null -> b
        | _    -> a // if Object.ReferenceEquals(a, null) then b else a


    /// Apply function, like |> , but ignore result.
    /// Return original input.
    /// let inline (|>!) x f =  f x |> ignore ; x
    /// Be aware of correct indenting see:
    /// https://stackoverflow.com/questions/64784154/indentation-change-after-if-else-expression-not-taken-into-account
    let inline ( |>! ) x f = 
        f x |> ignore //https://twitter.com/GoswinR/status/1316988132932407296
        x

    /// Get first element of Triple (Tuple of three elements)
    let inline t1 (a,_,_) = a

    /// Get second element of Triple (Tuple of three elements)
    let inline t2 (_,b,_) = b

    /// Get third element of Triple (Tuple of three elements)
    let inline t3 (_,_,c) = c


    /// Converts negative indices to positive ones.
    /// Correct results from -length up to length-1
    /// e.g.: -1 is  last item .
    /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
    let inline negIdx i len = 
        let ii =  if i < 0 then len+i else i
        if ii<0 || ii >= len then IndexOutOfRangeException.Raise "Util.negIdx: Bad index %d for items count %d." i len
        ii

    /// Any int will give a valid index for given collection size.
    /// Converts negative indices to positive ones and loops to start after last index is reached.
    /// Returns a valid index for a collection of 'length' items for any integer
    let inline negIdxLooped i length = 
        let t = i % length
        if t >= 0 then t
        else           t + length

    /// Any int will give a valid index for given collection size.
    /// Division remainder will be used i % length
    /// e.g.: -1 is last item
    /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
    let inline saveIdx i length = 
        negIdxLooped i length

    /// If condition is true return f(x) else just x
    let inline ifDo condition (f:'T->'T) (x:'T) = 
        if condition then f x else x

    /// Caches the results of a function in a Map.
    /// The argument to the function will be used as key in the Map.
    /// The argument can be unit or null(=None) too.
    let memoize f = 
        let cache = ref Map.empty // using a Dictionary would fail on a null or unit key
        fun x ->
            match (!cache).TryFind(x) with
            | Some res -> res
            | None ->
                let res = f x
                cache := (!cache).Add(x,res)
                res

    /// Generic parser that infers desired return type
    let inline tryParse<'a when 'a: (static member TryParse: string * byref<'a> -> bool)> x = 
        // https://twitter.com/mvsmal/status/1317020301046259712
        let mutable res = Unchecked.defaultof<'a>
        if (^a: (static member TryParse: string * byref<'a> -> bool) (x, &res))
        then Some res
        else None

    /// Generic parser that infers desired return type or fails with ArgumentException
    let inline parse<'a when 'a: (static member TryParse: string * byref<'a> -> bool)> x = 
        let mutable res = Unchecked.defaultof<'a>
        if (^a: (static member TryParse: string * byref<'a> -> bool) (x, &res))
        then res
        else ArgumentException.RaiseBase "Failed to parse %A to a %A" x (res.GetType())



/// Shadows the ignore function to only accept structs
/// This is to prevent accidentally ignoring partially applied functions that would return struct
module SaveIgnore = 

    /// This ignore only work on Value types,
    /// Objects and functions need to be ignored with 'ignoreObj'
    /// This is to prevent accidentally ignoring partially applied functions that would return struct
    let inline ignore (x:'T when 'T: struct) = ()

    /// Ignores any object (and struct)
    /// For structs use 'ignore'
    let inline ignoreObj (x:obj) = ()

/// Functions to deal with integer ref objects
/// Also works with integers augmented with Units of Measure (UoM)
module IntRef = 

    /// Increment a ref cell by one
    /// Shadows built in 'incr' to allow Units of Measure (UoM)
    let inline incr i (x:int<'UoM>) = i := !i + 1<_>

    /// Decrement a ref cell by one
    let inline decr (i:ref<int<'UoM>>) = i := !i - 1<_>

    /// Increment a ref cell by a given int
    let inline incrBy i (x:int<'UoM>) = i := !i + x

    /// Decrement a ref cell by a given int
    let inline decrBy i (x:int<'UoM>) = i := !i - x

    /// Set ref cell to given int if it is bigger than current ref cell  value
    let inline setMax i (x:int<'UoM>) = if x > !i then i := x

    /// Set ref cell to given int if it is smaller than current ref cell  value
    let inline setMin i (x:int<'UoM>) = if x < !i then i := x



/// Functions to deal with float ref objects
/// Also works with floats augmented with Units of Measure (UoM)
module FloatRef = 

    /// Increment a ref cell by a given float
    let inline incrBy i (x:float<'UoM>) = i := !i + x

    /// Decrement a ref cell by a given float
    let inline decrBy i (x:float<'UoM>) = i := !i - x

    /// Set ref cell to given float if it is bigger than current ref cell  value
    let inline setMax i (x:float<'UoM>) = if x > !i then i := x

    /// Set ref cell to given float if it is smaller than current ref cell  value
    let inline setMin i (x:float<'UoM>) = if x < !i then i := x


/// Provides generic math operators for adding, subtracting, multiplying and dividing 
/// numbers that can be converted to a floats.
/// The new operators are: +.  .+   -.  .-   *.  .*   /.  ./   
/// There the period is always on the side of the non float value.
/// A Units of Measure on the non-float number gets ignored and lost however.
module FloatMathOperators = 
    open Microsoft.FSharp.Core.LanguagePrimitives

    /// Multiplies a float with A-number-that-can-be-converted-to-a-float
    let inline ( *. ) (x:float<'M>) (y) : float<'M> = x * (float y)
    
    /// Multiplies a-number-that-can-be-converted-to-a-float with a float
    let inline ( .* ) (x) (y :float<'N>) : float<'M> = (float x) * y

    /// Add a float to A-number-that-can-be-converted-to-a-float
    let inline ( +. ) (x:float<'M>) (y) : float<'M> = x + FloatWithMeasure<'M>(float y)
    
    /// Add A-number-that-can-be-converted-to-a-float to a float
    let inline ( .+ ) (x) (y :float<'M>) : float<'M> = FloatWithMeasure<'M>(float x) + y

    /// Subtract a float from A-number-that-can-be-converted-to-a-float
    let inline ( -. ) (x:float<'M>) (y) : float<'M> = x - FloatWithMeasure<'M>(float y)
    
    /// Subtract A-number-that-can-be-converted-to-a-float to a float
    let inline ( .- ) (x) (y :float<'M>) : float<'M> = FloatWithMeasure<'M>(float x) - y

    /// Divide a float by A-number-that-can-be-converted-to-a-float
    let inline ( /. ) (x:float<'M>) (y) : float<'M>=  x / (float y)
    
    /// Divide A-number-that-can-be-converted-to-a-float by a float
    let inline ( ./ ) (x) (y :float) : float = (float x) / y





