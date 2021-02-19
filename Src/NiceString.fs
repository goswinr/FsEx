namespace FsEx


open System
open FsEx.SaveIgnore //so that  |> ignore  can only be used on value types
open System


// TODO use 
// http://www.fssnip.net/cV/title/A-Generic-PrettyPrinter-for-Record-types



module NiceString  =
    

    module NiceStringSettings = 
        /// set this to change the printing of floats larger than 10'000
        let mutable thousandSeparator = '\'' // = just one quote '

        /// set this to change how deep the content of nested seq is printed (printFull ignores this)
        /// default = 3
        let mutable toNiceStringMaxDepth = 3

        /// set this to change how how many items per seq are printed (printFull ignores this)
        /// default = 5
        let mutable toNiceStringMaxItemsPerSeq = 5

        /// set this to change how many characters of a string might be printed at onece.
        /// default = 5000
        let mutable maxCharsInString = 5000

        /// Allows to inject an optional formater that gets called befor main formater
        /// This formater shall return None if the main formater should be used
        /// use externalFormater for types defined in other assemblies 
        let mutable externalFormater : obj -> option<string> = // TODO  functions as value cannot be generic !!! how else cou this function be changed at a later moment  in NiceStringImplementation??
            fun _ -> None   
        
        let mutable mainFormater: obj -> string = 
            sprintf "%A"
                

        let mutable mainFormaterFull: obj -> string = 
            sprintf "%A" 
                

    module internal Floats  = 

        let internal formatThousands (s:string) =
            let last = s.Length - 1         
            let sb= Text.StringBuilder()
            let inline add (c:char) = sb.Append(c) |> ignoreObj
            for i = 0 to last do
                if i = 0 || i = last then 
                    add s.[i]
                elif i = 1 && s.[0] = '-' then 
                    add s.[i]
                else
                    if (last - i + 1) % 3 = 0 then 
                        add NiceStringSettings.thousandSeparator
                        add s.[i]
                    else                
                        add s.[i]
            sb.ToString() 

        /// Formating with automatic precision 
        /// e.g.: 0 digits behind comma if above 1000 
        let internal floatToString  (x:float) =
            if   Double.IsNaN x then "NaN"
            elif x = Double.NegativeInfinity then "Negative Infinity"
            elif x = Double.PositiveInfinity then "Positive Infinity"
            elif x = -1.23432101234321e+308 then "-1.23432e+308 ( = RhinoMath.UnsetValue)" // for https://developer.rhino3d.com/api/RhinoCommon/html/F_Rhino_RhinoMath_UnsetValue.htm
            elif x = 0.0 then "0.0" // not "0" as in sprintf "%g"
            else
                let  a = abs x
                if   a > 10000. then sprintf "%.0f" x |> formatThousands
                elif a > 1000.  then sprintf "%.0f" x
                elif a > 100.   then sprintf "%.1f" x 
                elif a > 10.    then sprintf "%.2f" x 
                elif a > 1.     then sprintf "%.3f" x 
                else                 sprintf "%g"   x  

        /// Formating with automatic precision 
        /// e.g.: 0 digits behind comma if above 1000
        let internal singleToString  (x:float32) =
            if   Single.IsNaN x then "NaN"
            elif x = Single.NegativeInfinity then "Negative Infinity"
            elif x = Single.PositiveInfinity then "Positive Infinity"            
            elif x = -1.234321e+38f then "-1.2343e+38 ( = RhinoMath.UnsetSingle)" // for https://developer.rhino3d.com/api/RhinoCommon/html/F_Rhino_RhinoMath_UnsetSingle.htm
            elif x = 0.0f then "0.0" // not "0" as in sprintf "%g"
            else
                let  a = abs x
                if   a > 10000.f then sprintf "%.0f" x |> formatThousands
                elif a > 1000.f  then sprintf "%.0f" x
                elif a > 100.f   then sprintf "%.1f" x 
                elif a > 10.f    then sprintf "%.2f" x 
                elif a > 1.f     then sprintf "%.3f" x 
                else                  sprintf "%g"   x  



    /// Nice formating for floats , some Rhino Objects and sequences of any kind, first four items are printed out.
    /// set NiceString.toNiceStringMaxItemsPerSeq to other value if more or less shall be shown (default is 4)
    /// set NiceString.toNiceStringMaxDepth to change how deep nested lists are printed (default is 3)
    /// see values in NiceString.NiceStringSettings
    let toNiceString (x:'T) = NiceStringSettings.mainFormater x


    /// Nice formating for floats , some Rhino Objects and sequences of any kind, all items including nested items are printed out.
    let toNiceStringFull (x:'T) = NiceStringSettings.mainFormaterFull x

  