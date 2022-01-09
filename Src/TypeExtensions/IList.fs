namespace FsEx

open System

/// Adds extension members on Collections.Generic.IList
/// for getting and setting first, second, last and similar indices.
/// Also adds functionality for negative indices and l.Slice(startIdx:int , endIdx: int) that works  with negative numbers
module ExtensionsIList = 

    type Collections.Generic.IList<'T>  with

        /// Gets an item at index
        /// (use IList.GetNeg(i) member if you want to use negative indices too)
        member inline this.Get index = 
            if index >= this.Count then ArgumentOutOfRangeException.Raise "Cant get index %d from IList of %d items: %A" index this.Count this
            this.[index]

        /// Sets an item at index
        /// (use IList.SetNeg(i) member if you want to use negative indices too)
        member inline this.Set index value = 
            if index >= this.Count then ArgumentOutOfRangeException.Raise "Cant set index %d to %A in IList of %d items: %A " index value this.Count  this
            this.[index] <- value

        /// Gets an item in the IList by index.
        /// Allows for negative index too ( -1 is last item,  like Python)
        /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
        member inline this.GetNeg index = 
            let len = this.Count
            let ii =  if index < 0 then len + index else index
            if ii<0 || ii >= len then ArgumentOutOfRangeException.Raise "IList.GetNeg: Failed to get index %d from IList of %d items: %A" index this.Count this
            this.[ii]

        /// Sets an item in the IList by index.
        /// Allows for negative index too ( -1 is last item,  like Python)
        /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
        member inline this.SetNeg index value = 
            let len = this.Count
            let ii =  if index < 0 then len + index else index
            if ii<0 || ii >= len then ArgumentOutOfRangeException.Raise "IList.SetNeg: Failed to set index %d to %A from IList of %d items: %A" index value this.Count this
            this.[ii] <- value

        /// Any index will return a value.
        /// IList is treated as an endless loop in positive and negative direction
        member inline this.GetLooped index = 
            let len = this.Count
            if len=0 then ArgumentOutOfRangeException.Raise "IList.GetLooped: Failed to get index %d from IList of 0 items" index
            let t = index % len
            let ii = if t >= 0 then t  else t + len
            this.[ii]

        /// Any index will set a value.
        /// IList is treated as an endless loop in positive and negative direction
        member inline this.SetLooped index value  = 
            let len = this.Count
            if len=0 then ArgumentOutOfRangeException.Raise "IList.SetLooped: Failed to Set index %d to %A in IList of 0 items" index value
            let t = index % len
            let ii = if t >= 0 then t  else t + len
            this.[ii] <- value


        /// Gets the index of the last item in the IList.
        /// equal to this.Count - 1
        member inline this.LastIndex = 
            if this.Count = 0 then IndexOutOfRangeException.Raise "this.LastIndex: Failed to get LastIndex of empty IList"
            this.Count - 1

        /// Get (or set) the last item in the IList.
        /// equal to this.[this.Count - 1]
        member inline this.Last
            with get() = 
                if this.Count = 0 then IndexOutOfRangeException.Raise "IList.Last: Failed to get last item of empty IList %A" (NiceString.toNiceStringFull this)
                this.[this.Count - 1]
            and set (v:'T) = 
                if this.Count = 0 then IndexOutOfRangeException.Raise "IList.Last: Failed to set last item of empty IList %A to %A" (NiceString.toNiceStringFull this) v
                this.[this.Count - 1] <- v

        /// Get (or set) the second last item in the IList.
        /// equal to this.[this.Count - 2]
        member inline this.SecondLast
            with get() = 
                if this.Count < 2 then  IndexOutOfRangeException.Raise "IList.SecondLast: Failed to get second last item of %s" (NiceString.toNiceStringFull this)
                this.[this.Count - 2]
            and set (v:'T) = 
                if this.Count < 2 then  IndexOutOfRangeException.Raise "IList.SecondLast: Failed to set second last item of %s to %A" (NiceString.toNiceStringFull this) v
                this.[this.Count - 2] <- v


        /// Get (or set) the third last item in the IList.
        /// equal to this.[this.Count - 3]
        member inline this.ThirdLast
            with get() = 
                if this.Count < 3 then  IndexOutOfRangeException.Raise "IList.ThirdLast: Failed to get third last item of %s"  (NiceString.toNiceStringFull this)
                this.[this.Count - 3]
            and set (v:'T) = 
                if this.Count < 3 then  IndexOutOfRangeException.Raise "IList.ThirdLast: Failed to set third last item of %s to %A"  (NiceString.toNiceStringFull this) v
                this.[this.Count - 3] <- v

        /// Get (or set) Sets the first item in the IList.
        /// equal to this.[0]
        member inline this.First
            with get() = 
                if this.Count = 0 then IndexOutOfRangeException.Raise "IList.First: Failed to get first item of empty IList  %A "(NiceString.toNiceStringFull this)
                this.[0]
            and set (v:'T) = 
                if this.Count = 0 then IndexOutOfRangeException.Raise "IList.First: Failed to set first item of empty IList %A to %A" (NiceString.toNiceStringFull this) v
                this.[0] <- v

        /// Get (or set) the second item in the IList.
        /// equal to this.[1]
        member inline this.Second
            with get() = 
                if this.Count < 2 then IndexOutOfRangeException.Raise  "IList.Second: Failed to get second item of %s"  (NiceString.toNiceStringFull this)
                this.[1]
            and set (v:'T) = 
                if this.Count < 2 then IndexOutOfRangeException.Raise  "IList.Second: Failed to set second item of %s to %A"  (NiceString.toNiceStringFull this) v
                this.[1] <- v

        /// Get (or set) the third item in the IList.
        /// equal to this.[2]
        member inline this.Third
           with get() = 
               if this.Count < 3 then IndexOutOfRangeException.Raise "IList.Third: Failed to get third item of %s" (NiceString.toNiceStringFull this)
               this.[2]
           and set (v:'T) = 
               if this.Count < 3 then IndexOutOfRangeException.Raise "IList.Third: Failed to set third item of %s to %A" (NiceString.toNiceStringFull this) v
               this.[2] <- v


        /// Gets a subrange of the IList
        /// Allows for negative indices too. ( -1 is last item, like Python)
        /// The resulting IList includes the end index.
        /// The built in slicing notation (e.g. a.[1..3]) for ILists does not allow for negative indices.
        /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
        member inline this.Slice(startIdx:int, endIdx:int) : 'T Rarr = // to use slicing notation e.g. : this.[ 1 .. -1] // don't overload .GetSlive .[ x ... y] directly, this would be a casting horror for Lists and ILists wher neg indices  Slices dont work
            let count = this.Count
            let st  = if startIdx< 0 then count + startIdx        else startIdx
            let len = if endIdx  < 0 then count + endIdx - st + 1 else endIdx - st + 1

            if st < 0 || st > count - 1 then
                let err = sprintf "Slice: Start index %d is out of range. Allowed values are -%d up to %d for IList of %d items" startIdx count (count-1)  count
                raise (IndexOutOfRangeException(err))

            if st+len > count then
                let err = sprintf "Slice: End index %d is out of range. Allowed values are -%d up to %d for IList of %d items" startIdx count (count-1)  count
                raise (IndexOutOfRangeException(err))

            if len < 0 then
                let en = if endIdx<0 then count+endIdx else endIdx
                let err = sprintf "Slice: Start index '%A' (= %d) is bigger than end index '%A'(= %d) for IList of %d items" startIdx st endIdx en  count
                raise (IndexOutOfRangeException(err))

            let rarr = Rarr (count)
            for i = 0 to len - 1 do rarr.Add (this.[st+i])
            rarr




