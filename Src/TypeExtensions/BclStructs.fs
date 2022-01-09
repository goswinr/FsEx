namespace FsEx

open System


/// This module is set to auto open.
[<AutoOpen>] // so that extension become available on opening FsEx
module ExtensionsBclStructs = 

    let private deDE = Globalization.CultureInfo("de-DE")
    let private invC = Globalization.CultureInfo.InvariantCulture

    type Int32 with

        /// Calls float(x)
        member inline x.ToFloat = float(x)

        /// Calls byte(x)
        member inline x.ToByte = byte(x)

        /// Same as i.ToString() but as property, not method.
        /// So without the need for the brackets. (for nicer inlining)
        member inline x.AsString = x.ToString()

        /// As string, with thousands separators.
        member x.ToNiceString = NiceFormat.int x


        /// Converts an Int32 to a string of 32 characters of '█' and '-'.
        member x.AsBinaryString: string = 
            // or System.Convert.ToString (n,2)
            let b = Array.zeroCreate 32
            let mutable pos = 31
            let mutable i = 0
            while i < 32 do
                if ((x &&& (1 <<< i)) <> 0) then  b.[pos] <- '█'
                else                              b.[pos] <- '-'
                pos <- pos - 1
                i   <- i + 1
            (* // testing :
            for i = 0 to 100 do
                //printfn "%08d: %s" i <| asBinaryString i
                //printfn "%08d: %s" i <| asBinaryString -i
                printfn "%08d: %s" i <| asBinaryString (i <<< 16)
                printfn "%08d: %s" i <| asBinaryString (-i <<< 16)
                *)
            new System.String(b)


    type Int64 with

        /// Calls float(x)
        member inline x.ToFloat = float(x)

        /// Calls byte(x)
        member inline x.ToByte = byte(x)

        /// Same as i.ToString() but as property, not method.
        /// So without the need for the brackets. (for nicer inlining)
        member inline x.AsString = x.ToString()

        /// With thousands separators
        member x.ToNiceString = NiceFormat.int64 x

        /// Converts an Int64 to a string of 64 characters of '█' and '-'.
        member x.AsBinaryString: string = 
            // or System.Convert.ToString (n,2)
            let b = Array.zeroCreate 64
            let mutable pos = 63
            let mutable i = 0
            while i < 64 do
                if ((x &&& (1L <<< i)) <> 0L) then  b.[pos] <- '█'
                else                                b.[pos] <- '-'
                pos <- pos - 1
                i   <- i + 1
            new System.String(b)

    type Byte with

        /// Calls float(x)
        member inline x.ToFloat = float(x)

        /// Calls byte(x)
        member inline x.ToInt = int(x)


    type Double with

        /// Converts float to int including rounding .
        /// int(round(x))
        member inline x.ToInt = int(round(x))

        /// With automatic formating of display reduced precision depending on float size
        /// also includes thousands separators
        member x.ToNiceString = NiceFormat.float x

        /// Similar to f.ToString() using InvariantCulture
        /// with automatic formating to never use scientific notation
        /// will have maximum 15 decimal places
        /// f.ToString( "0.###############" , InvariantCulture )
        member x.AsString = x.ToString( "0.###############",invC)

        /// Similar to f.ToString()
        /// but with German culture using comma (,) as decimal separator
        /// with automatic formating to never use scientific notation
        /// will have maximum 15 decimal places
        /// f.ToString( "0.###############" , CultureInfo("de-DE") )
        member x.AsStringDE = x.ToString( "0.###############",deDE)

        /// Format without digits behind coma
        member x.AsString0 = x.ToString( "0")

        /// Format with one digits behind coma
        member x.AsString1 = x.ToString( "0.0",invC)

        /// Format with two digits behind coma
        member x.AsString2 = x.ToString( "0.00",invC)

    type Single with

        /// with automatic formating of display reduced precision depending on single size
        /// also includes thousands separators
        member x.ToNiceString = NiceFormat.single x

        /// Similar to f.ToString()
        /// with automatic formating to never use scientific notation
        /// will have maximum 7 decimal places
        /// f.ToString( "0.###############" , InvariantCulture)
        member x.AsString = x.ToString( "0.#######" , invC)

        /// Similar to f.ToString()
        /// but with German culture using comma (,) as decimal separator
        /// with automatic formating to never use scientific notation
        /// will have maximum 7 decimal places
        /// f.ToString( "0.###############" , CultureInfo("de-DE"))
        member x.AsStringDE = x.ToString( "0.#######" , deDE)

        /// Format without digits behind coma
        member x.AsString0 = x.ToString( "0")

        /// Format with one digits behind coma
        member x.AsString1 = x.ToString( "0.0" , invC)

        /// Format with two digits behind coma
        member x.AsString2 = x.ToString( "0.00" , invC)

    type Decimal with

        /// Converts decimal to int including rounding .
        /// int(round(x))
        member inline x.ToInt = int(round(x))

        /// With automatic formating of display reduced precision depending on float size
        /// also includes thousands separators
        member x.ToNiceString = NiceFormat.decimal x

        /// Similar to f.ToString()
        /// with automatic formating to never use scientific notation
        /// will have maximum 15 decimal places
        /// f.ToString( "0.###############" , InvariantCulture)
        member x.AsString = x.ToString( "0.###############" , invC)

        /// Similar to f.ToString()
        /// but with German culture using comma (,) as decimal separator
        /// with automatic formating to never use scientific notation
        /// will have maximum 15 decimal places
        /// f.ToString( "0.###############" , CultureInfo("de-DE"))
        member x.AsStringDE = x.ToString( "0.###############" , deDE)

        /// Format without digits behind coma
        member x.AsString0 = x.ToString( "0")

        /// Format with one digits behind coma
        member x.AsString1 = x.ToString( "0.0" , invC)

        /// Format with two digits behind coma
        member x.AsString2 = x.ToString( "0.00" , invC)

    type DateTime with

        /// Current local date as yyyy-MM-dd
        static member todayStr = 
            let n= DateTime.Now
            n.ToString("yyyy-MM-dd")

        /// Current local date and time as yyyy-MM-dd_HH-mm
        static member nowStr = 
            let n= DateTime.Now
            n.ToString("yyyy-MM-dd_HH-mm")

        /// Current UTC date and time as yyyy-MM-dd_HH-mm_UTC
        /// (with _UTC suffix)
        static member nowStrUtc = 
            let n= DateTime.UtcNow
            n.ToString("yyyy-MM-dd_HH-mm_UTC")

        /// Current UTC date and time as yyyy-MM-dd_HH-mm-fff
        /// (includes 3 digits of milliseconds)
        static member nowStrLong = 
            let n= DateTime.UtcNow
            n.ToString("yyyy-MM-dd_HH-mm-ss-fff")

        /// Formats with .ToString("yyyy-MM-dd_HH-mm-ss-fff")
        /// (includes 3 digits of milliseconds)
        member t.formatYMDHMSF = 
            t.ToString("yyyy-MM-dd_HH-mm-ss-fff")

        /// Formats with .ToString("yyyy-MM-dd_HH-mm-ss")
        /// including seconds
        member t.formatYMDHMS = 
            t.ToString("yyyy-MM-dd_HH-mm-ss")

        /// Formats with .ToString("yyyy-MM-dd_HH-mm")
        /// including minutes
        member t.formatYMDHM = 
            t.ToString("yyyy-MM-dd_HH-mm")

        /// Formats with .ToString("yyyy-MM-dd")
        /// Date only, no time of day
        member t.formatYMD = 
            t.ToString("yyyy-MM-dd")
