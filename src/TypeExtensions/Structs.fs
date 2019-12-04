namespace FsEx

open System




[<AutoOpen>]
module TypeExtensionsStructs =      

    [<EXT>]
    type Int32 with  
        [<EXT>] member inline x.ToDouble = float(x)
        [<EXT>] member inline x.ToByte = byte(x)

    [<EXT>]
    type Byte with  
        [<EXT>] member inline x.ToDouble = float(x)
        [<EXT>] member inline x.ToInt = int(x)
    
    
    [<EXT>]
    type Double with  
        ///converts int to float including rounding: 
        ///int(round(x))
        [<EXT>] member inline x.ToInt = int(round(x))

        /// with automatic formating of display precision depending on float size
        [<EXT>] member x.ToNiceString = NiceString.floatToString x        
        
    [<EXT>]
    type Single with  
        /// with automatic formating of display precision depending on float size
        [<EXT>] member x.ToNiceString = NiceString.singleToString x

    [<EXT>]
    type Drawing.Color with        
        ///Compare to another color only by Alpha, Red, Green and Blue values ignoring other fields such as IsNamedColor        
        [<EXT>] 
        member inline this.EqualsARGB(other:Drawing.Color)=
            this.A = other.A && 
            this.R = other.R && 
            this.G = other.G && 
            this.B = other.B        
        
        ///Compare two colors only by Alpha, Red, Green and Blue values ignoring other fields such as IsNamedColor
        [<EXT>]
        static member inline AreEqualARGB (this:Drawing.Color)(other:Drawing.Color)=
            this.EqualsARGB(other)
    
    [<EXT>]
    type DateTime with
        [<EXT>]static member todayStr = DateTime.Now.ToString("yyyy-MM-dd")
        [<EXT>]static member nowStr =   DateTime.Now.ToString("yyyy-MM-dd_HH-mm")
        [<EXT>]static member nowStrUtc =   DateTime.UtcNow.ToString("yyyy-MM-dd_HH-mm_UTC")
        
        ///* UTC time, inludes 3 digits of miliseconds
        [<EXT>]static member nowStrLong =   DateTime.UtcNow.ToString("yyyy-MM-dd_HH-mm-ss-fff")