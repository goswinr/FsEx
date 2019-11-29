namespace FsEx

open System


[<AutoOpen>]
module TypeExtensionsCollections =   
    
    [<EXT>]
    type Object with 
        [<EXT>]  
        ///A property like the ToString() method, 
        ///but with richer formationg for collections.
        member obj.ToNiceString = NiceString.toNiceString obj
    
    [<EXT>]
    type System.String with
        [<EXT>]
        member inline s.Last = s.[s.Length - 1] 
        
        [<EXT>]
        member s.LastX i = s.Substring(s.Length-i,i) 
        
        ///Allows for negtive index too (like python)
        [<EXT>]         
        member this.GetItem index = if index<0 then this.[this.Length+index]   else this.[index]          
        
        ///Allows for negative indices too.
        [<EXT>]
        member s.Slice(startIdx,endIdx) =
            let count = s.Length
            let st  = if startIdx<0 then count+startIdx else startIdx
            let len = if endIdx<0 then count+endIdx-st+1 else endIdx-st+1
    
            if st < 0 || st > count-1 then 
                let err = sprintf "GetSlice: Start index %d is out of range. Allowed values are -%d upto %d for String '%s' of %d chars" startIdx count (count-1) s count
                raise (IndexOutOfRangeException(err))
    
            if st+len > count then 
                let err = sprintf "GetSlice: End index %d is out of range. Allowed values are -%d upto %d for String '%s' of %d chars" startIdx count (count-1) s count
                raise (IndexOutOfRangeException(err)) 
            
            if len < 0 then
                let en = if endIdx<0 then count+endIdx else endIdx
                let err = sprintf "GetSlice: Start index '%A' (= %d) is bigger than end index '%A'(= %d) for String '%s' of %d items" startIdx st startIdx en s count
                raise (IndexOutOfRangeException(err)) 
            
            s.Substring(st,len) 
        

    
    [<EXT>]
    type Collections.Generic.Dictionary<'K,'V> with           
        [<EXT>]
        member inline  d.SetValue k v =
            d.[k] <-v        
       
        [<EXT>] 
        member inline d.GetValue k  =
             d.[k]
        
        ///Get a value and remove it from Dictionary, like *.pop() in Python         
        [<EXT>] 
        member inline d.Pop k  =
            let v= d.[k]
            d.Remove(k)|> ignore
            v

        ///Returns a seq of key and value tuples
        [<EXT>] 
        member inline d.Items =
            seq { for KeyValue(k, v) in d -> k, v}

    [<EXT>]
    type Collections.Generic.List<'T>  with        
        [<EXT>]
        member inline this.LastIndex = this.Count - 1

        [<EXT>]
        member inline this.Last = this.[this.Count - 1]

        [<EXT>] 
        ///Allows for negtive slice index too ( -1 = last element), 
        ///returns a shallow copy including the end index.
        member this.GetSlice(startIdx, endIdx) =    // maps onto slicing operator .[1..3]
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
                let err = sprintf "GetSlice: Start index '%A' (= %d) is bigger than end index '%A'(= %d) for List of %d items" startIdx st startIdx en  count
                raise (IndexOutOfRangeException(err)) 
                
            this.GetRange(st, len)
        
        [<EXT>] 
        ///Allows for negtive index too (like python)
        member this.GetItem index = if index<0 then this.[this.Count+index]   else this.[index]
        
        [<EXT>] 
        ///Allows for negtive index too (like python)
        member this.SetItem index value = if index<0 then this.[this.Count+index]<-value   else this.[index]<-value 

    
    [<EXT>]
    type ``[]``<'T>  with //Generic Array
        [<EXT>]
        member inline this.LastIndex = this.Length - 1

        [<EXT>]
        member inline this.Last = this.[this.Length - 1]
        
        [<EXT>] 
        ///Allows for negtive index too (like python)
        member this.GetItem index = if index<0 then this.[this.Length+index]   else this.[index]
        
        [<EXT>] 
        ///Allows for negtive index too (like python)
        member this.SetItem index value = if index<0 then this.[this.Length+index]<-value   else this.[index]<-value 

        //member this.GetSlice(startIdx, endIdx) = // overides of existing methods are ignored / not possible https://github.com/dotnet/fsharp/issues/3692#issuecomment-334297164

        ///Allows for negative indices too.
        [<EXT>]
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
                let err = sprintf "Slice: Start index '%A' (= %d) is bigger than end index '%A'(= %d) for Array of %d items" startIdx st startIdx en  count
                raise (IndexOutOfRangeException(err)) 
            
            Array.init len (fun i -> this.[st+1])
            
