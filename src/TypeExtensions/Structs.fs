namespace FsEx

open System
open System.Runtime.CompilerServices



[<AutoOpen>]
module TypeExtensionsStructs =      

    //[<Extension>] //Error 3246
    type Int32 with  
        [<Extension>] member inline x.ToFloat = float(x)
        [<Extension>] member inline x.ToByte = byte(x)

    //[<Extension>] //Error 3246
    type Byte with  
        [<Extension>] member inline x.ToFloat = float(x)
        [<Extension>] member inline x.ToInt = int(x)
    
    
    //[<Extension>] //Error 3246
    type Double with  
        /// converts int to float including rounding: 
        /// int(round(x))
        [<Extension>] member inline x.ToInt = int(round(x))

        /// with automatic formating of display precision depending on float size
        [<Extension>] member x.ToNiceString = NiceString.floatToString x        
        
    //[<Extension>] //Error 3246
    type Single with  
        /// with automatic formating of display precision depending on float size
        [<Extension>] member x.ToNiceString = NiceString.singleToString x

    
    //[<Extension>] //Error 3246
    type DateTime with
        
        /// Current date as yyyy-MM-dd
        [<Extension>]static member todayStr =    DateTime.Now.ToString("yyyy-MM-dd")
        
        /// Current local date and time as yyyy-MM-dd_HH-mm
        [<Extension>]static member nowStr =      DateTime.Now.ToString("yyyy-MM-dd_HH-mm")
        
        /// Current UTC date and time as yyyy-MM-dd_HH-mm_UTC
        [<Extension>]static member nowStrUtc =   DateTime.UtcNow.ToString("yyyy-MM-dd_HH-mm_UTC")
        
        /// Current UTC date and time as yyyy-MM-dd_HH-mm-fff. inludes 3 digits of miliseconds
        [<Extension>]static member nowStrLong =   DateTime.UtcNow.ToString("yyyy-MM-dd_HH-mm-ss-fff")
