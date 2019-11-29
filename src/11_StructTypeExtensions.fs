namespace FsEx

open System



type EXT = Runtime.CompilerServices.ExtensionAttribute

[<AutoOpen>]
module TypeExtensionsStructs =      


    type Int32 with  
        [<EXT>] member inline x.ToDouble = float(x)
        [<EXT>] member inline x.ToByte = byte(x)


    type Byte with  
        [<EXT>] member inline x.ToDouble = float(x)
        [<EXT>] member inline x.ToInt = int(x)
    
   
    type Double with  
        ///converts int to float including rounding: 
        ///int(round(x))
        [<EXT>] member inline x.ToInt = int(round(x))

        /// with automatic formating of display precision depending on float size
        [<EXT>] member x.ToNiceString = NiceString.floatToString x        
        

    type Single with  
        /// with automatic formating of display precision depending on float size
        [<EXT>] member x.ToNiceString = NiceString.singleToString x


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
    
