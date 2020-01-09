namespace FsEx

open System
open System.Runtime.CompilerServices


[<AutoOpen>]
module TypeExtensionsArray =  

    //[<Extension>] //Error 3246
    type ``[]``<'T>  with //Generic Array
        
        /// like this.Length - 1
        [<Extension>]
        member inline this.LastIndex = 
            if this.Length = 0 then failwithf "this.LastIndex: Cannot get LastIndex of empty Array"
            this.Length - 1
       
        /// last item in Array
        [<Extension>]
        member inline this.Last = 
            if this.Length = 0 then failwithf "this.Last: Cannot get Last item of empty Array"
            this.[this.Length - 1]

        [<Extension>]
        member inline this.SecondLast = 
            if this.Length < 2 then failwithf "this.SecondLast: Can not get SecondLast item of %s"  (NiceString.toNiceStringFull this)
            this.[this.Length - 2]

        [<Extension>]
        member inline this.ThirdLast = 
            if this.Length < 3 then failwithf "this.ThirdLast: Can not get ThirdLast item of %s"  (NiceString.toNiceStringFull this)
            this.[this.Length - 3]

            
        [<Extension>]
        member inline this.First = 
            if this.Length = 0 then failwithf "this.First: Can not get First item of empty Array"
            this.[0]

        [<Extension>]
        member inline this.Second = 
            if this.Length < 2 then failwithf "this.Second: Can not get Second item of %s"  (NiceString.toNiceStringFull this)
            this.[1]

        [<Extension>]
        member inline this.Third = 
            if this.Length < 3 then failwithf "this.Third: Can not get Third item of %s"  (NiceString.toNiceStringFull this)
            this.[2]
        
        [<Extension>] 
        ///Allows for negtive index too (like Python)
        member this.GetItem index = 
            let i = negIdx index this.Length
            this.[i]
    
        [<Extension>] 
        ///Allows for negtive index too (like Python)
        member this.SetItem index value = 
            let i = negIdx index this.Length
            this.[i] <- value 

        //member this.GetSlice(startIdx, endIdx) = // overides of existing methods are unfortunatly silently ignored and not possible. see https://github.com/dotnet/fsharp/issues/3692#issuecomment-334297164                

        ///Allows for negative indices too.
        ///The resulting array includes the end index.
        [<Extension>]
        member this.Slice(startIdx:int , endIdx: int ) : 'T array=
            let count = this.Length
            let st  = if startIdx< 0 then count + startIdx        else startIdx
            let len = if endIdx  < 0 then count + endIdx - st + 1 else endIdx - st + 1

            if st < 0 || st > count - 1 then 
                let err = sprintf "Slice: Start index %d is out of range. Allowed values are -%d upto %d for Array of %d items" startIdx count (count-1)  count
                raise (IndexOutOfRangeException(err))

            if st+len > count then 
                let err = sprintf "Slice: End index %d is out of range. Allowed values are -%d upto %d for Array of %d items" startIdx count (count-1)  count
                raise (IndexOutOfRangeException(err)) 
        
            if len < 0 then
                let en = if endIdx<0 then count+endIdx else endIdx
                let err = sprintf "Slice: Start index '%A' (= %d) is bigger than end index '%A'(= %d) for Array of %d items" startIdx st endIdx en  count
                raise (IndexOutOfRangeException(err)) 
        
            Array.init len (fun i -> this.[st+i])
        

        [<Extension>]  
        ///A property like the ToString() method, 
        ///But with richer formationg for collections
        member this.ToNiceString = NiceString.toNiceString this