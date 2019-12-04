namespace FsEx

open System
open System.Runtime.CompilerServices



[<AutoOpen>]
module TypeExtensionsStructs =      

    [<Extension>]
    type Int32 with  
        [<Extension>] member inline x.ToDouble = float(x)
        [<Extension>] member inline x.ToByte = byte(x)

    [<Extension>]
    type Byte with  
        [<Extension>] member inline x.ToDouble = float(x)
        [<Extension>] member inline x.ToInt = int(x)
    
    
    [<Extension>]
    type Double with  
        ///converts int to float including rounding: 
        ///int(round(x))
        [<Extension>] member inline x.ToInt = int(round(x))

        /// with automatic formating of display precision depending on float size
        [<Extension>] member x.ToNiceString = NiceString.floatToString x        
        
    [<Extension>]
    type Single with  
        /// with automatic formating of display precision depending on float size
        [<Extension>] member x.ToNiceString = NiceString.singleToString x

    [<Extension>]
    type Drawing.Color with        
        ///Compare to another color only by Alpha, Red, Green and Blue values ignoring other fields such as IsNamedColor        
        [<Extension>] 
        member inline this.EqualsARGB(other:Drawing.Color)=
            this.A = other.A && 
            this.R = other.R && 
            this.G = other.G && 
            this.B = other.B        
        
        ///Compare two colors only by Alpha, Red, Green and Blue values ignoring other fields such as IsNamedColor
        [<Extension>]
        static member inline AreEqualARGB (this:Drawing.Color)(other:Drawing.Color)=
            this.EqualsARGB(other)
    
    [<Extension>]
    type DateTime with
        [<Extension>]static member todayStr = DateTime.Now.ToString("yyyy-MM-dd")
        [<Extension>]static member nowStr =   DateTime.Now.ToString("yyyy-MM-dd_HH-mm")
        [<Extension>]static member nowStrUtc =   DateTime.UtcNow.ToString("yyyy-MM-dd_HH-mm_UTC")
        
        ///* UTC time, inludes 3 digits of miliseconds
        [<Extension>]static member nowStrLong =   DateTime.UtcNow.ToString("yyyy-MM-dd_HH-mm-ss-fff")
