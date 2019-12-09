namespace FsEx

open System
open System.Runtime.CompilerServices


[<AutoOpen>]
module TypeExtensionsArray =  

    //[<Extension>] //Error 3246
    type ``[]``<'T>  with //Generic Array
        
        /// like this.Length - 1
        [<Extension>]
        member inline this.LastIndex = this.Length - 1

        /// last item in Array
        [<Extension>]
        member inline this.Last = this.[this.Length - 1]
    
        [<Extension>] 
        ///Allows for negtive index too (like Python)
        member this.GetItem index = if index<0 then this.[this.Length+index]   else this.[index]
    
        [<Extension>] 
        ///Allows for negtive index too (like Python)
        member this.SetItem index value = if index<0 then this.[this.Length+index]<-value   else this.[index]<-value 

        //member this.GetSlice(startIdx, endIdx) = // overides of existing methods are unfurtrunatly silently ignored and not possible. see https://github.com/dotnet/fsharp/issues/3692#issuecomment-334297164                

        ///Allows for negative indices too.
        ///The resulting array includes the end index.
        [<Extension>]
        member this.Slice(startIdx,endIdx) =
            let count = this.Length
            let st  = if startIdx<0 then count+startIdx else startIdx
            let len = if endIdx<0 then count+endIdx-st+1 else endIdx-st+1

            if st < 0 || st > count-1 then 
                let err = sprintf "Slice: Start index %d is out of range. Allowed values are -%d upto %d for Array of %d chars" startIdx count (count-1)  count
                raise (IndexOutOfRangeException(err))

            if st+len > count then 
                let err = sprintf "Slice: End index %d is out of range. Allowed values are -%d upto %d for Array of %d chars" startIdx count (count-1)  count
                raise (IndexOutOfRangeException(err)) 
        
            if len < 0 then
                let en = if endIdx<0 then count+endIdx else endIdx
                let err = sprintf "Slice: Start index '%A' (= %d) is bigger than end index '%A'(= %d) for Array of %d items" startIdx st endIdx en  count
                raise (IndexOutOfRangeException(err)) 
        
            Array.init len (fun i -> this.[st+i])
        
