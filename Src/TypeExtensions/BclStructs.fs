namespace FsEx

open System
open System.Runtime.CompilerServices



[<AutoOpen>]
module TypeExtensionsBclStructs =      

 
    type Int32 with  
        
        /// Calls float(x)
        [<Extension>] member inline x.ToFloat = float(x)
        
        /// Calls byte(x)
        [<Extension>] member inline x.ToByte = byte(x)
        
        /// Same as i.ToString() but as propertry, not method.
        /// So without the need for the brackets. (for nicer inlining)
        [<Extension>] member inline x.AsString = x.ToString()
        
        /// With thousands separators
        [<Extension>] member x.ToNiceString = x.ToString() |> NiceString.Floats.formatThousands        

 
    type Byte with  

        /// Calls float(x)
        [<Extension>] member inline x.ToFloat = float(x)

        /// Calls byte(x)
        [<Extension>] member inline x.ToInt = int(x)    
    
 
    type Double with  
        
        /// Converts float to int including rounding .
        /// int(round(x))
        [<Extension>] member inline x.ToInt = int(round(x))

        /// With automatic formating of display reduced precision depending on float size
        /// also includes thousands separators
        [<Extension>] member x.ToNiceString = NiceString.Floats.floatToString x        
        
        /// Similar to f.ToString() 
        /// with automatic formating to never use scientific notation
        /// will have maximum 15 decimal places
        /// f.ToString( "0.###############") 
        [<Extension>] member x.AsString = x.ToString( "0.###############")         

        /// Format without digits behind coma
        [<Extension>] member x.AsString0 = x.ToString( "0.") 

        /// Format with one digits behind coma
        [<Extension>] member x.AsString1 = x.ToString( "0.#") 

        /// Format with two digits behind coma
        [<Extension>] member x.AsString2 = x.ToString( "0.##") 

    type Single with  

        /// with automatic formating of display reduced precision depending on single size
        /// also includes thousands separators
        [<Extension>] member x.ToNiceString = NiceString.Floats.singleToString x

        /// Similar to f.ToString() 
        /// with automatic formating to never use scientific notation
        /// will have maximum 7 decimal places
        /// f.ToString( "0.###############") 
        [<Extension>] member x.AsString = x.ToString( "0.#######") 
    
        /// Format without digits behind coma
        [<Extension>] member x.AsString0 = x.ToString( "0.") 

        /// Format with one digits behind coma
        [<Extension>] member x.AsString1 = x.ToString( "0.#") 

        /// Format with two digits behind coma
        [<Extension>] member x.AsString2 = x.ToString( "0.##") 
    
    type Decimal with  
           
        /// Converts decimal to int including rounding .
        /// int(round(x))
        [<Extension>] member inline x.ToInt = int(round(x))

        /// With automatic formating of display reduced precision depending on float size
        /// also includes thousands separators
        [<Extension>] member x.ToNiceString = NiceString.Floats.floatToString (float x)        
           
        /// Similar to f.ToString() 
        /// with automatic formating to never use scientific notation
        /// will have maximum 15 decimal places
        /// f.ToString( "0.###############") 
        [<Extension>] member x.AsString = x.ToString( "0.###############") 

        
        /// Format without digits behind coma
        [<Extension>] member x.AsString0 = x.ToString( "0.") 

        /// Format with one digits behind coma
        [<Extension>] member x.AsString1 = x.ToString( "0.#") 

        /// Format with two digits behind coma
        [<Extension>] member x.AsString2 = x.ToString( "0.##") 

    type DateTime with
        
        /// Current local date as yyyy-MM-dd
        [<Extension>]static member todayStr =    DateTime.Now.ToString("yyyy-MM-dd")
        
        /// Current local date and time as yyyy-MM-dd_HH-mm
        [<Extension>]static member nowStr =      DateTime.Now.ToString("yyyy-MM-dd_HH-mm")
        
        /// Current UTC date and time as yyyy-MM-dd_HH-mm_UTC
        /// (with _UTC suffix)
        [<Extension>]static member nowStrUtc =   DateTime.UtcNow.ToString("yyyy-MM-dd_HH-mm_UTC")
        
        /// Current UTC date and time as yyyy-MM-dd_HH-mm-fff
        /// (inludes 3 digits of miliseconds)
        [<Extension>]static member nowStrLong =   DateTime.UtcNow.ToString("yyyy-MM-dd_HH-mm-ss-fff")
