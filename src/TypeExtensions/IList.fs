namespace FsEx

open System
open System.Collections.Generic

[<AutoOpen>]
module TypeExtensionsIList =  

    [<EXT>]
    type Collections.Generic.IList<'T>  with 

        /// like this.Count - 1
        [<EXT>]
        member inline this.LastIndex = this.Count - 1

        /// last item in Array
        [<EXT>]
        member inline this.Last = this.[this.Count - 1]
    
        [<EXT>] 
        ///Allows for negtive index too (like Python)
        member this.GetItem index = if index<0 then this.[this.Count+index]   else this.[index]
    
        [<EXT>] 
        ///Allows for negtive index too (like Python)
        member this.SetItem index value = if index<0 then this.[this.Count+index]<-value   else this.[index]<-value  // only on IList


        ///Allows for negative indices too.
        ///The resulting List includes the item at slice ending index.
        [<EXT>]
        member this.GetSlice(startIdx,endIdx) : 'T ResizeArray = // to use slicing notation e.g. : xs.[ 1 .. -1]
            let count = this.Count
            let st  = match startIdx with None -> 0        | Some i -> if i<0 then count+i      else i
            let len = match endIdx   with None -> count-st | Some i -> if i<0 then count+i-st+1 else i-st+1
    
            if st < 0 || st > count-1 then 
                let err = sprintf "GetSlice: Start index %d is out of range. Allowed values are -%d upto %d for List of %d items" startIdx.Value count (count-1) count
                raise (IndexOutOfRangeException(err))
    
            if st+len > count then 
                let err = sprintf "GetSlice: End index %d is out of range. Allowed values are -%d upto %d for List of %d items" endIdx.Value count (count-1) count
                raise (IndexOutOfRangeException(err)) 

            if len < 0 then
                let en =  match endIdx  with None -> count-1 | Some i -> if i<0 then count+i else i
                let err = sprintf "GetSlice: Start index '%A' (= %d) is bigger than end index '%A'(= %d) for List of %d items" startIdx st endIdx en  count
                raise (IndexOutOfRangeException(err)) 
        
            ResizeArray.init len (fun i -> this.[st+i])
        
    


