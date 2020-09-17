namespace FsEx

open System
open System.Collections.Generic
open System.Runtime.CompilerServices

[<AutoOpen>]
module TypeExtensionsIList =  

    //[<Extension>] //Error 3246
    type Collections.Generic.IList<'T>  with 
        
        /// Gets the index of the last item in the IList.
        /// equal to this.Length - 1   
        [<Extension>]
        member inline this.LastIndex = 
            if this.Count = 0 then failwithf "this.LastIndex: Can not get LastIndex of empty IList"
            this.Count - 1

        /// Gets the last item in the IList.
        /// equal to this.[this.Length - 1]
        [<Extension>]
        member inline this.Last = 
            if this.Count = 0 then failwithf "this.Last: Can not get Last item of empty IList"
            this.[this.Count - 1]

        /// Gets the second last item in the IList.
        /// equal to this.[this.Length - 2]
        [<Extension>]
        member inline this.SecondLast = 
            if this.Count < 2 then failwithf "this.SecondLast: Can not get SecondLast item of %s"  (NiceString.toNiceStringFull this)
            this.[this.Count - 2]

        /// Gets the third last item in the IList.
        /// equal to this.[this.Length - 2]
        [<Extension>]
        member inline this.ThirdLast = 
            if this.Count < 3 then failwithf "this.ThirdLast: Can not get ThirdLast item of %s"  (NiceString.toNiceStringFull this)
            this.[this.Count - 3]


        /// Gets the first item in the IList.
        /// equal to this.[0]
        [<Extension>]
        member inline this.First = 
            if this.Count = 0 then failwithf "this.First: Can not get First item of empty IList"
            this.[0]

        /// Gets the second item in the IList.
        /// equal to this.[1]
        [<Extension>]
        member inline this.Second = 
            if this.Count < 2 then failwithf "this.Second: Can not get Second item of %s"  (NiceString.toNiceStringFull this)
            this.[1]

        /// Gets the third item in the IList.
        /// equal to this.[2]
        [<Extension>]
        member inline this.Third = 
            if this.Count < 3 then failwithf "this.Third: Can not get Third item of %s"  (NiceString.toNiceStringFull this)
            this.[2]

            
        /// Gets an item in the IList by index.
        /// Allows for negtive index too ( -1 is last item,  like Python)
        /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
        [<Extension>] 
        member this.GetItem index = if index<0 then this.[this.Count+index]   else this.[index]
    
        /// Sets an item in the IList by index.
        /// Allows for negtive index too ( -1 is last item, like Python)
        /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
        [<Extension>] 
        member this.SetItem index value = if index<0 then this.[this.Count+index]<-value   else this.[index]<-value  // only on IList

        /// Gets a subrange of the IList
        /// Allows for negative indices too. ( -1 is last item, like Python)
        /// The resulting array includes the end index.
        /// The built in slicing notaion (e.g. a.[1..3]) for arrays does not allow for negative indices.
        /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
        [<Extension>]
        member this.Slice(startIdx:int, endIdx:int) : 'T ResizeArray = // to use slicing notation e.g. : xs.[ 1 .. -1] // don't overload .GetSlive .[ x ... y] directly, this would be a casting horror for Lists and arrays wher neg indices  Slices dont work
            let count = this.Count
            let st  = if startIdx< 0 then count + startIdx        else startIdx
            let len = if endIdx  < 0 then count + endIdx - st + 1 else endIdx - st + 1

            if st < 0 || st > count - 1 then 
                let err = sprintf "Slice: Start index %d is out of range. Allowed values are -%d upto %d for IList of %d items" startIdx count (count-1)  count
                raise (IndexOutOfRangeException(err))

            if st+len > count then 
                let err = sprintf "Slice: End index %d is out of range. Allowed values are -%d upto %d for IList of %d items" startIdx count (count-1)  count
                raise (IndexOutOfRangeException(err)) 
        
            if len < 0 then
                let en = if endIdx<0 then count+endIdx else endIdx
                let err = sprintf "Slice: Start index '%A' (= %d) is bigger than end index '%A'(= %d) for IList of %d items" startIdx st endIdx en  count
                raise (IndexOutOfRangeException(err))
        
            ResizeArray.init len (fun i -> this.[st+i])
        
    


