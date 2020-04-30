namespace FsEx

open System
open System.Runtime.CompilerServices


[<AutoOpen>]
module TypeExtensionsArray =  

    //[<Extension>] //Error 3246
    type ``[]``<'T>  with //Generic Array
        
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
        
        
        /// Gets item in the array by index.
        /// Allows for negtive index too ( -1 is last item,  like Python)
        /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
        [<Extension>] 
        member this.GetItem index = 
            let i = negIdx index this.Length
            this.[i]
    
        
        /// Sets item in the array by index.
        /// Allows for negtive index too ( -1 is last item, like Python)
        /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
        [<Extension>]
        member this.SetItem index value = 
            let i = negIdx index this.Length
            this.[i] <- value 

        //member this.GetSlice(startIdx, endIdx) = // overides of existing methods are unfortunatly silently ignored and not possible. see https://github.com/dotnet/fsharp/issues/3692#issuecomment-334297164                

        /// Allows for negative indices too. ( -1 is last item, like Python)
        /// The resulting array includes the end index.
        /// The built in slicing notaion (e.g. a.[1..3]) for arrays does not allow for negative indices.
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