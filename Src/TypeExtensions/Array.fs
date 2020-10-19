namespace FsEx

open System
open System.Runtime.CompilerServices


[<AutoOpen>]
module TypeExtensionsArray =  

    //[<Extension>] //Error 3246
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


        /// Gets the index of the last item in the array.
        /// equal to this.Length - 1
        [<Extension>]
        member inline this.LastIndex = 
            if this.Length = 0 then failwithf "array.LastIndex: Cannot get LastIndex of empty Array"
            this.Length - 1
       
        /// Gets the last item in the array.
        /// equal to this.[this.Length - 1]
        [<Extension>]
        member inline this.Last = 
            if this.Length = 0 then failwithf "array.Last: Cannot get Last item of empty Array"
            this.[this.Length - 1]
        
        /// Gets the second last item in the array.
        /// equal to this.[this.Length - 2]
        [<Extension>]
        member inline this.SecondLast = 
            if this.Length < 2 then failwithf "array.SecondLast: Can not get SecondLast item of %s"  (NiceString.toNiceStringFull this)
            this.[this.Length - 2]
        
        /// Gets the third last item in the array.
        /// equal to this.[this.Length - 2]
        [<Extension>]
        member inline this.ThirdLast = 
            if this.Length < 3 then failwithf "array.ThirdLast: Can not get ThirdLast item of %s"  (NiceString.toNiceStringFull this)
            this.[this.Length - 3]
                    
        /// Gets the first item in the array.
        /// equal to this.[0]
        [<Extension>]
        member inline this.First = 
            if this.Length = 0 then failwithf "array.First: Can not get First item of empty Array"
            this.[0]
        
        /// Gets the second item in the array.
        /// equal to this.[1]
        [<Extension>]
        member inline this.Second = 
            if this.Length < 2 then failwithf "array.Second: Can not get Second item of %s"  (NiceString.toNiceStringFull this)
            this.[1]
        
        /// Gets the third item in the array.
        /// equal to this.[2]
        [<Extension>]
        member inline this.Third = 
            if this.Length < 3 then failwithf "array.Third: Can not get Third item of %s"  (NiceString.toNiceStringFull this)
            this.[2]
        


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