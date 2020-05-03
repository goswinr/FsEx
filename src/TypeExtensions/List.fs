namespace FsEx



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] //need this so doesn't hide Seq class in C# assemblies (should consider for other extension modules as well)
module List = 
    
    let internal indexFromBack ix (xs: 'T list) =
        if List.isEmpty xs then failwithf "can't get index from back %d from empty list" ix
        else        
            // there are two ways to get an item indexed from the back:
            // (1) iterate all items and keep a buffer
            // (2) iterate once to find length, and second time to find item ( no buffer)
            // using (1) here:            
            let ar = Array.zeroCreate (ix+1)// buffer
            let mutable i = 0
            let mutable k = 0
            let rec  loop (ys: 'T list) = 
                match ys with            
                | [h] -> 
                    k <-i % (ix+1)//loop index
                    ar.[k]<- h 
                    i <- i+1
                
                | h :: tail -> 
                    k <-i % (ix+1)//loop index
                    ar.[k]<- h 
                    i <- i+1
                    loop tail    
                
                | _ -> () // cought already above
    
            loop xs
            if ix >= i then failwithf "can't get index from back %d from  list of %d items" ix i 
            ar.GetItem(k-ix)
        
    /// Gets an item in the list by index.
    /// Allows for negtive index too ( -1 is last item,  like Python)
    let getItem index xs = 
        if index >= 0 then List.item index xs
        else indexFromBack ( 1 - index ) xs
            
        