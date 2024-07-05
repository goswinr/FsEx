namespace FsEx

open System

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Array = 
    
    /// Gets the second last item in the Array.
    /// Same as this.[this.Length - 2]
    let inline secondLast  (arr: array<'T>)= 
        if arr.Length < 2 then  IndexOutOfRangeException.Raise "FsEx.Array.secondLast: Failed to get second last item of %s" (NiceString.toNiceStringLong arr)
        arr.[arr.Length - 2]

    /// Gets the third last item in the Array.
    /// Same as this.[this.Length - 3]
    let inline thirdLast  (arr: array<'T>)= 
        if arr.Length < 3 then  IndexOutOfRangeException.Raise "FsEx.Array.thirdLast: Failed to get third last item of %s" (NiceString.toNiceStringLong arr)
        arr.[arr.Length - 3]

    /// Gets the first item in the Array.
    /// Same as this.[0]
    let inline first  (arr: array<'T>)= 
        if arr.Length = 0 then IndexOutOfRangeException.Raise "FsEx.Array.first: Failed to get first item of %s" (NiceString.toNiceStringLong arr)
        arr.[0]

    /// Gets the the only item in the FsEx.Array.
    /// Fails if the Array does not have exactly one element.
    let inline firstAndOnly (arr: array<'T>) =
        if arr.Length = 0 then IndexOutOfRangeException.Raise  "FsEx.Array.firstOnly: Failed to get first item of empty array<%s>" (typeof<'T>).FullName
        if arr.Length > 1 then IndexOutOfRangeException.Raise  "FsEx.Array.firstOnly: array<%s> is expected to have only one item but has %d Array: %s" (typeof<'T>).FullName arr.Length (NiceString.toNiceStringLong arr)
        arr.[0]

    /// Gets the second item in the Array.
    /// Same as this.[1]
    let inline second  (arr: array<'T>)= 
        if arr.Length < 2 then IndexOutOfRangeException.Raise  "FsEx.Array.second: Failed to get second item of %s" (NiceString.toNiceStringLong arr)
        arr.[1]

    /// Gets the third item in the Array.
    /// Same as this.[2]
    let inline third  (arr: array<'T>)= 
        if arr.Length < 3 then IndexOutOfRangeException.Raise "FsEx.Array.third: Failed to get third item of %s" (NiceString.toNiceStringLong arr)
        arr.[2]

    /// Checks if a given array matches the content in Array at a given index
    let matches (searchFor:'T[]) atIdx (searchIn:'T[]) :bool = 
        if atIdx < 0                then IndexOutOfRangeException.Raise    "FsEx.Array.matches: atIdx Index is too small: %d for array of %d items" atIdx searchIn.Length
        if atIdx >= searchIn.Length then IndexOutOfRangeException.Raise    "FsEx.Array.matches: atIdx Index is too big: %d for array of %d items" atIdx searchIn.Length
        let fLast = searchFor.Length - 1
        let iLen = searchIn.Length
        let rec find i f = // index in searchIn ,  index in searchFor
            if  i = iLen then false // not found! not enough items left in searchIn array
            elif searchIn.[i] = searchFor.[f]  then
                if f = fLast then true // found,  exit !
                else find (i + 1) (f + 1)
            else false    // set back search to i+1  before first match
        find atIdx 0


    /// Find first index where searchFor occurs in searchIn array.
    /// Give lower and upper bound index for search space.
    /// Returns -1 if not found
    let findValue (searchFor:'T) fromIdx  tillIdx (searchIn:'T[])  :int = 
        if fromIdx < 0                then IndexOutOfRangeException.Raise    "FsEx.Array.find: fromIdx Index is too small: %d for array of %d items" fromIdx searchIn.Length
        if tillIdx >= searchIn.Length then IndexOutOfRangeException.Raise    "FsEx.Array.find: tillIdx Index is too big:   %d for array of %d items" tillIdx searchIn.Length
        if tillIdx < fromIdx          then ArgumentOutOfRangeException.Raise "FsEx.Array.find: tillIdx Index %d is smaller than fromIdx Index %d for array of %d items" tillIdx fromIdx searchIn.Length
        let rec find i  = 
            if  i > tillIdx  then -1 // not found!
            elif searchIn.[i] = searchFor  then i  // found,  exit !
            else find (i + 1)
        find fromIdx


    /// Find last index where searchFor occurs in searchIn array. Searching from end.
    /// Give lower and upper bound index for search space.
    /// Returns -1 if not found
    let findLastValue (searchFor:'T) fromIdx  tillIdx (searchIn:'T[])   :int = 
        if fromIdx < 0                then IndexOutOfRangeException.Raise    "FsEx.Array.findBack: fromIdx Index is too small: %d for array of %d items" fromIdx searchIn.Length
        if tillIdx >= searchIn.Length then IndexOutOfRangeException.Raise    "FsEx.Array.findBack: tillIdx Index is too big:   %d for array of %d items" tillIdx searchIn.Length
        if tillIdx < fromIdx          then ArgumentOutOfRangeException.Raise "FsEx.Array.findBack: tillIdx Index %d is smaller than fromIdx Index %d for array of %d items" tillIdx fromIdx searchIn.Length
        let rec find i  = 
            if  i < fromIdx  then -1 // not found!
            elif searchIn.[i] = searchFor  then i  // found,  exit !
            else find (i - 1)
        find tillIdx


    /// Find first index where searchFor array occurs in searchIn array.
    /// Give lower and upper bound index for search space.
    /// Returns index of first element or -1 if not found
    let findArray (searchFor:'T[]) fromIdx  tillIdx (searchIn:'T[])   :int = 
        if fromIdx < 0                then IndexOutOfRangeException.Raise    "FsEx.Array.findArray (of %d items): fromIdx Index is too small: %d for array of %d items" searchFor.Length fromIdx searchIn.Length
        if tillIdx >= searchIn.Length then IndexOutOfRangeException.Raise    "FsEx.Array.findArray (of %d items): tillIdx Index is too big:   %d for array of %d items" searchFor.Length tillIdx searchIn.Length
        if tillIdx < fromIdx          then ArgumentOutOfRangeException.Raise "FsEx.Array.findArray (of %d items): tillIdx Index %d is smaller than fromIdx Index %d for array of %d items" searchFor.Length tillIdx fromIdx searchIn.Length
        let fLast = searchFor.Length - 1
        let rec find i f = // index in searchIn ,  index in searchFor
            if  i > tillIdx - fLast + f  then -1 // not found! not enough items left in searchIn array
            elif searchIn.[i] = searchFor.[f]  then
                if f = fLast then i - fLast  // found,  exit !
                else find (i + 1) (f + 1)
            else find (i + 1 - f) 0    // set back search to i+1  before first match
        find fromIdx 0



    /// Find last index where searchFor array occurs in searchIn array. Searching from end.
    /// Give lower and upper bound index for search space.
    /// Returns index of first element  or -1 if not found
    let findLastArray  (searchFor:'T[]) fromIdx  tillIdx (searchIn:'T[])   :int = 
        if fromIdx < 0                then IndexOutOfRangeException.Raise    "FsEx.Array.findLastArray (of %d items): fromIdx Index is too small: %d for array of %d items" searchFor.Length fromIdx searchIn.Length
        if tillIdx >= searchIn.Length then IndexOutOfRangeException.Raise    "FsEx.Array.findLastArray (of %d items): tillIdx Index is too big:   %d for array of %d items" searchFor.Length tillIdx searchIn.Length
        if tillIdx < fromIdx          then ArgumentOutOfRangeException.Raise "FsEx.Array.findLastArray (of %d items): tillIdx Index %d is smaller than fromIdx Index %d for array of %d items" searchFor.Length tillIdx fromIdx searchIn.Length
        let fLast = searchFor.Length - 1
        let rec find i f = // index in searchIn ,  index in searchFor
            if  i - f < fromIdx  then -1 // not found! not enough items left in searchIn array
            elif searchIn.[i] = searchFor.[f]  then
                if f = 0 then i  // found ,  exit!
                else find (i - 1) (f - 1)
            else find (i - 1 + fLast - f) fLast    // set back search to i-1  before first match
        find tillIdx fLast

    //let testFind() = 
    //    let i =  "abcde".ToCharArray()
    //    let l =  i.LastIndex
    //    let ab =  "ab".ToCharArray()
    //    let de =  "de".ToCharArray()
    //
    //    clearFeshLog()
    //    blue "%b" ( 0 = findArray ab 0 l i)
    //    blue "%b" (-1 = findArray ab 1 l i)
    //    blue "%b" ( 0 = findLastArray ab 0 l i)
    //    blue "%b" (-1 = findLastArray ab 1 l i)
    //    blue "%b" (-1 = findArray de 0 (l-1)  i)
    //    blue "%b" ( 3 = findArray de 0 l   i)
    //    blue "%b" (-1 = findLastArray de 0 (l-1)  i)
    //    blue "%b" ( 3 = findLastArray de 0 l   i)
    //    green "-------------"

