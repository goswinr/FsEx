namespace FsEx

open System
open System.Collections.Generic

open ExtensionsIList // for a.GetNeg


/// Adds extension members on on  Collections.Generic.IEnumerable
/// for geting first, second, last and similar indices.
/// Also adds functionality for negative indices and l.Slice(startIdx:int , endIdx: int) that works  with negative numbers
module ExtensionsSeq = 

    let internal indexFromBack ix (xs: 'T seq) = 
        match xs with
        | :? ('T[]) as a ->     a.GetNeg(a.Length - 1 - ix)
        | :? ('T Rarr) as a  -> a.GetNeg(a.Count  - 1 - ix) //ResizeArray and other collections
        | :? ('T IList) as a -> a.GetNeg(a.Count  - 1 - ix) //ResizeArray and other collections
        | _ ->
            // ther are two ways to get an item indexed from the back:
            // (1) iterate all items and keep a buffer
            // (2) iterate once to find length, and second time to find item ( no buffer)
            // using (1) here:
            use e = xs.GetEnumerator()
            if e.MoveNext() then
                let ar = Array.zeroCreate (ix+1) // use array for buffer
                let mutable i = 0
                let mutable k = 0
                while (e.MoveNext()) do
                    k <-i % (ix+1) //loop index
                    ar.[k] <- e.Current
                    i <- i+1
                if ix > i then IndexOutOfRangeException.Raise "Seq.indexFromBack: Can't get index from back %d from  seq of %d items" ix (i+1)
                ar.GetNeg(k-ix)
            else
                IndexOutOfRangeException.Raise "Seq.indexFromBack: Can't get index from back %d from empty seq" ix


    type IEnumerable<'T>  with

        /// Gets an item by index position in the Seq
        /// Allows for negative index too (like Python)
        member this.GetNeg (index) = 
            try
                if index >= 0 then Seq.item index this
                else indexFromBack ( 1 - index ) this
            with
            | :? InvalidOperationException  as ex -> IndexOutOfRangeException.Raise "seq.GetNeg(%d): Failed to get %dth item of %s : %s" index index (NiceString.toNiceStringFull this) ex.Message
            | ex -> raise ex //some other error raised while constructing lazy seq


        ///Returns Seq.length - 1
        member inline this.LastIndex = 
            if Seq.isEmpty this then IndexOutOfRangeException.Raise "seq.LastIndex: Failed to get LastIndex of empty Seq"
            (Seq.length this) - 1

        /// Gets the last item in the Seq
        member this.Last = 
            indexFromBack 0 this

        /// Gets the second last item in the Seq
        member this.SecondLast = 
            indexFromBack 1 this

        /// Gets the third last item in the Seq
        member this.ThirdLast = 
            indexFromBack 2 this

        /// Gets the first item in the Seq
        member inline this.First = 
            if Seq.isEmpty this then IndexOutOfRangeException.Raise "seq.First: Failed to get LastIndex of empty Seq"
            Seq.head this

        /// Gets the second item in the Seq
        member inline this.Second = 
            try
                this|> Seq.skip 1 |> Seq.head
            with
            | :? InvalidOperationException  as ex -> IndexOutOfRangeException.Raise "seq.Second: Failed to get second item of %s : %s"  (NiceString.toNiceStringFull this) ex.Message
            | ex -> raise ex  //some other error raised while constructing lazy seq

        /// Gets the third item in the Seq
        member inline this.Third = 
            try
                this|> Seq.skip 2 |> Seq.head
            with
            | :? InvalidOperationException  as ex -> IndexOutOfRangeException.Raise "seq.Third: Failed to get third item of %s : %s"  (NiceString.toNiceStringFull this) ex.Message
            | ex -> raise ex   //some other error raised while constructing lazy seq

        /// Seq.Slice
        /// Allows for negative indices too, like Python, -1 is the last element.
        /// The resulting seq includes the item at slice-ending-index. like F# range expressions include the last integer e.g.: 0..5
        member this.Slice(startIdx:int , endIdx: int) : 'T seq = // don't overload .GetSlice .[ x ... y] directly, this would be a casting horror for Lists and arrays where neg indices don't work
            let count = lazy(Seq.length this)
            let st  = if startIdx< 0 then count.Value + startIdx        else startIdx
            let len = if endIdx  < 0 then count.Value + endIdx - st + 1 else endIdx - st + 1
            try
                this|> Seq.skip st |> Seq.take len
            with
            | :? InvalidOperationException  as ex ->
                let en =  if endIdx < 0 then count.Value + endIdx else endIdx
                IndexOutOfRangeException.Raise "seq.GetSlice: Start index '%A' (= %d) and end index '%A'(= %d) for Seq of %d items failed for %s : %s" startIdx st endIdx en  count.Value (NiceString.toNiceStringFull this) ex.Message
            | ex -> raise ex //some other error raised while constructing lazy seq


        /// A property like the ToString() method,
        /// But with richer formating for collections
        member obj.ToNiceString = 
            NiceString.toNiceString obj

