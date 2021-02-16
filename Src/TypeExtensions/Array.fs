namespace FsEx

open System
open System.Runtime.CompilerServices


[<AutoOpen>]
module TypeExtensionsArray =  

    
    type ``[]``<'T>  with //Generic Array
        
        /// Gets an item at index 
        /// (use Array.GetNeg(i) member if you want to use negative indices too)
        member this.Get index = 
            if index >= this.Length then ArgumentOutOfRangeException.Raise "Cant get index %d from Array of %d items: %A" index this.Length this
            this.[index]
            
        /// Sets an item at index 
        /// (use Array.SetNeg(i) member if you want to use negative indices too)
        member this.Set index value = 
            if index >= this.Length then ArgumentOutOfRangeException.Raise "Cant set index %d to %A in Array of %d items: %A " index value this.Length  this
            this.[index] <- value

        /// Gets an item in the Array by index.
        /// Allows for negtive index too ( -1 is last item,  like Python)
        /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
        member this.GetNeg index = 
            let len = this.Length
            let ii =  if index < 0 then len + index else index
            if ii<0 || ii >= len then ArgumentOutOfRangeException.Raise "Array.GetNeg: Can't get index %d from Array of %d items: %A" index this.Length this
            this.[ii]        

        /// Sets an item in the Array by index.
        /// Allows for negtive index too ( -1 is last item,  like Python)
        /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
        member this.SetNeg index value = 
            let len = this.Length
            let ii =  if index < 0 then len + index else index
            if ii<0 || ii >= len then ArgumentOutOfRangeException.Raise "Array.SetNeg: Can't set index %d to %A rom Array of %d items: %A" index value this.Length this
            this.[ii] <- value        
   
        /// Any index will return a value.
        /// Array is treated as an endless loop in positive and negative direction   
        member this.GetLooped index = 
            let len = this.Length
            if len=0 then ArgumentOutOfRangeException.Raise "Array.GetLooped: Can't get index %d from Array of 0 items" index
            let t = index % len
            let ii = if t >= 0 then t  else t + len 
            this.[ii]              

        /// Any index will set a value.
        /// Array is treated as an endless loop in positive and negative direction   
        member this.SetLooped index value  = 
            let len = this.Length
            if len=0 then ArgumentOutOfRangeException.Raise "Array.SetLooped: Can't Set index %d to %A in Array of 0 items" index value
            let t = index % len
            let ii = if t >= 0 then t  else t + len 
            this.[ii] <- value

       
        /// Allows for negtive index too ( -1 is last item,  like Python)
        [<Extension;Obsolete>] 
        member this.GetItem index =  // TODO Delete
            let i = negIdx index this.Length
            this.[i]
    
     
        /// Allows for negtive index too ( -1 is last item, like Python)
        [<Extension;Obsolete>]
        member this.SetItem index value =  // TODO Delete
            let i = negIdx index this.Length
            this.[i] <- value 



        /// Get (or set) the last item in the Array.
        /// equal to this.[this.Length - 1]
        [<Extension>]
        member this.Last
            with get() = 
                if this.Length = 0 then IndexOutOfRangeException.Raise "Array.Last: Can not get Last item of empty %A" (NiceString.toNiceStringFull this)
                this.[this.Length - 1]
            and set (v:'T) =
                if this.Length = 0 then IndexOutOfRangeException.Raise "Array.Last: Can not set Last item of empty %A to %A" (NiceString.toNiceStringFull this) v
                this.[this.Length - 1] <- v

        /// Get (or set) the second last item in the Array.
        /// equal to this.[this.Length - 2]
        [<Extension>]
        member this.SecondLast 
            with get() = 
                if this.Length < 2 then  IndexOutOfRangeException.Raise "Array.SecondLast: Can not get SecondLast item of %s" (NiceString.toNiceStringFull this)
                this.[this.Length - 2]
            and set (v:'T) =
                if this.Length < 2 then  IndexOutOfRangeException.Raise "Array.SecondLast: Can not set SecondLast item of %s to %A" (NiceString.toNiceStringFull this) v
                this.[this.Length - 2] <- v


        /// Get (or set) the third last item in the Array.
        /// equal to this.[this.Length - 3]
        [<Extension>]
        member this.ThirdLast 
            with get() =  
                if this.Length < 3 then  IndexOutOfRangeException.Raise "Array.ThirdLast: Can not get ThirdLast item of %s"  (NiceString.toNiceStringFull this)
                this.[this.Length - 3]
            and set (v:'T) =
                if this.Length < 3 then  IndexOutOfRangeException.Raise "Array.ThirdLast: Can not set ThirdLast item of %s to %A"  (NiceString.toNiceStringFull this) v
                this.[this.Length - 3] <- v           
            
        /// Get (or set) Sets the first item in the Array.
        /// equal to this.[0]
        [<Extension>]
        member this.First 
            with get() =  
                if this.Length = 0 then IndexOutOfRangeException.Raise "Array.First: Can not get First item of empty Array %A "(NiceString.toNiceStringFull this)
                this.[0]
            and set (v:'T) =
                if this.Length = 0 then IndexOutOfRangeException.Raise "Array.First: Can not set First item of empty Array %A to %A" (NiceString.toNiceStringFull this) v
                this.[0] <- v           

        /// Get (or set) the second item in the Array.
        /// equal to this.[1]
        [<Extension>]
        member this.Second 
            with get() = 
                if this.Length < 2 then IndexOutOfRangeException.Raise  "Array.Second: Can not get Second item of %s"  (NiceString.toNiceStringFull this)
                this.[1]
            and set (v:'T) =
                if this.Length < 2 then IndexOutOfRangeException.Raise  "Array.Second: Can not set Second item of %s to %A"  (NiceString.toNiceStringFull this) v
                this.[1] <- v           

        /// Get (or set) the third item in the Array.
        /// equal to this.[2]
        [<Extension>]
        member this.Third     
            with get() =
                if this.Length < 3 then IndexOutOfRangeException.Raise "Array.Third: Can not get Third item of %s" (NiceString.toNiceStringFull this)
                this.[2]
            and set (v:'T) =
                if this.Length < 3 then IndexOutOfRangeException.Raise "Array.Third: Can not set Third item of %s to %A" (NiceString.toNiceStringFull this) v
                this.[2] <- v          

       


        //member this.GetSlice(startIdx, endIdx) = // overides of existing methods are unfortunatly silently ignored and not possible. see https://github.com/dotnet/fsharp/issues/3692#issuecomment-334297164                

        /// Allows for negative indices too. ( -1 is last item, like Python)
        /// The resulting array includes the end index.
        /// The built in slicing notaion (e.g. a.[1..3]) for arrays does not allow for negative indices. (and can't be overwritten)
        /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
        [<Extension>]
        member this.Slice(startIdx:int , endIdx: int ) : 'T array=
            let count = this.Length
            let st  = if startIdx< 0 then count + startIdx        else startIdx
            let len = if endIdx  < 0 then count + endIdx - st + 1 else endIdx - st + 1

            if st < 0 || st > count - 1 then 
                let err = sprintf "array.Slice: Start index %d is out of range. Allowed values are -%d upto %d for Array of %d items" startIdx count (count-1)  count
                raise (IndexOutOfRangeException(err))

            if st+len > count then 
                let err = sprintf "array.Slice: End index %d is out of range. Allowed values are -%d upto %d for Array of %d items" startIdx count (count-1)  count
                raise (IndexOutOfRangeException(err)) 
        
            if len < 0 then
                let en = if endIdx<0 then count+endIdx else endIdx
                let err = sprintf "array.Slice: Start index '%A' (= %d) is bigger than end index '%A'(= %d) for Array of %d items" startIdx st endIdx en  count
                raise (IndexOutOfRangeException(err)) 
        
            Array.init len (fun i -> this.[st+i])
        

         
        /// A property like the ToString() method, 
        /// But with richer formationg for collections
        [<Extension>] 
        member this.ToNiceString = NiceString.toNiceString this