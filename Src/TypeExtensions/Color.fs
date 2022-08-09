namespace FsEx

open System
open System.Drawing

/// TODO : don't use System.Drawing !? https://docs.microsoft.com/en-us/dotnet/core/compatibility/core-libraries/6.0/system-drawing-common-windows-only

/// This module is set to auto open.
/// It provides EqualsARGB member
[<AutoOpen>]
module AutoOpenExtensionsColor = 

    type Drawing.Color with

        /// Compares to another color only by Alpha, Red, Green and Blue values ignoring other fields such as IsNamedColor
        member inline this.EqualsARGB(other:Drawing.Color)= 
            this.A = other.A &&
            this.R = other.R &&
            this.G = other.G &&
            this.B = other.B


/// Utility functions on System.Drawing.Color
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] //need this so doesn't hide Color class in C# assemblies
[<RequireQualifiedAccess>]
module Color = 
    open FsEx.CompareOperators

    let private Rand = 
        let now = DateTime.Now
        let seed = now.TimeOfDay
        new Random(int <| seed.TotalMilliseconds * 0.001295473) // random seed different than other modules
    
    
    //let inline private (!!) (x:float<_>) :float  = float x
    //let inline private (!^) (x:float) = LanguagePrimitives.FloatWithMeasure x

    /// Compares two colors only by Alpha, Red, Green and Blue values ignoring other fields such as IsNamedColor
    let isEqualARGB (this:Color) (other:Color) = 
        this.EqualsARGB(other)

    /// Given Hue, Saturation, Luminance in range of 0.0 to 1.0, returns a Drawing.Color
    let fromHSL (hue:float, saturation:float, luminance:float) = 
        // from http://stackoverflow.com/questions/2942/hsl-in-net
        // or http://bobpowell.net/RGBHSB.aspx
        // allow some numerical error:
        if not (-0.001 <. hue        .< 1.001) then ArgumentOutOfRangeException.Raise "FsEx.Color.fromHSL: H is bigger than 1.0 or smaller than 0.0: %f" hue
        if not (-0.001 <. saturation .< 1.001) then ArgumentOutOfRangeException.Raise "FsEx.Color.fromHSL: S is bigger than 1.0 or smaller than 0.0: %f" saturation
        if not (-0.001 <. luminance  .< 1.001) then ArgumentOutOfRangeException.Raise "FsEx.Color.fromHSL: L is bigger than 1.0 or smaller than 0.0: %f" luminance
        let H = UtilMath.clamp 0. 1. hue
        let S = UtilMath.clamp 0. 1. saturation
        let L = UtilMath.clamp 0. 1. luminance
        let v = if L <= 0.5 then L * (1.0 + S) else L + S - L * S
        let r,g,b = 
            if v > 0.001 then
                let m = L + L - v
                let sv = (v - m) / v
                let h = H * 5.999999 // so sextant never actually becomes 6
                let sextant = int h
                let fract = h - float sextant
                let vsf = v * sv * fract
                let mid1 = m + vsf
                let mid2 = v - vsf
                match sextant with
                    | 0 -> v,   mid1,    m
                    | 1 -> mid2,   v,    m
                    | 2 -> m,      v, mid1
                    | 3 -> m,   mid2,    v
                    | 4 -> mid1,   m,    v
                    | 5 -> v,      m, mid2
                    | x -> ArgumentException.RaiseBase "FsEx.Color.fromHSL: Error in internal HLS Transform, sextant is %A at HSL %A" x (H,S,L)
            else
                L,L,L // default to gray value

        Color.FromArgb (int <| 255.* r ,  int <| 255.* g , int <| 255.* b )

    /// Returns a color value from gradient blue to green  to yellow to red, excludes purple
    /// Input value to range from 0.0 to 1.0
    /// Will fail on too small or too big values,
    /// but up to a tolerance of 0.001 values will be clamped to 0 or 1.
    let gradientFromInterval (v) = 
        if not (-0.001 <. v .< 1.001) then ArgumentOutOfRangeException.Raise " FsEx.Color.FromInterval: v is bigger than 1.0 or smaller than 0.0: %f" v
        let v = UtilMath.clamp 0. 1. v
        let v = (1.0 - v) * 0.7 // 0.66666666 // to NOT make full color circle, that means to exclude the purple values.
        fromHSL (v,1.0,0.5)

    /// Returns a color value from gradient blue to green to yellow to red, excludes purple
    /// Input value to range from 0.0 to 1.0
    /// Will clamp on too small or too big values.
    let gradientFromIntervalClamped (v) = 
        let v = UtilMath.clamp 0. 1. v
        let v = (1.0 - v) * 0.7 // 0.66666666 // to NOT make full color circle, that means to exclude the purple values.
        fromHSL (v,1.0,0.5)

    /// Given a Drawing.Color , returns Hue,Saturation,Luminance in range of 0.0 to 1.0
    let HSLfromColor (color:Color) = 
            color.GetHue()/360.0f |> float,
            color.GetSaturation() |> float,
            color.GetBrightness() |> float
    
    
    let mutable private lastHue = 0.0
    let mutable private lumUp = false
    
    /// Generates a Random color with high saturation probability, excluding yellow colors
    /// These are ideal for layer color in Rhino3d CAD app
    /// Using golden-ratio-loop subsequent colors will have very distinct hues
    let rec randomForRhino () =  //TODO test and improve color boundary an color distribution 
        lastHue <- lastHue + 0.6180334 // golden angle conjugate
        lastHue <- lastHue % 1.0 // loop it between 0.0 and 1.0    
        let mutable s = Rand.NextDouble() 
        s <- s*s*s*s  //  0.0 - 1.0 increases the probability that the number is small( )
        s <- s*0.7    //  0.0 - 0.7 make sure it is less than 0.6
        s <- 1.1 - s  //  1.1 - 0.6  
        s <- UtilMath.clamp01 s //  1.0 - 0.6 
        let mutable l = Rand.NextDouble() 
        l <- l * l     //  0.0 - 1.0 increases the probability that the number is small( )
        l <- l * 0.35 * s   //  0.0 - 0.25 , and scale down with saturation too
        l <- if lumUp then lumUp<-false;  0.5+l*1.1 else lumUp<-true ;  0.5-l //alternate luminance up or down,  mor on the bright side
        if l > 0.3 && lastHue > 0.10 && lastHue < 0.22 then  // exclude yellow unless dark
            randomForRhino() 
        else    
            fromHSL(lastHue, s, l)

    /// Make a color lighter by percentage (value between 0.0 to 1.0) (1.0 = white, 0.0 = current color)
    let makeLighter v c = 
        if not (-0.001 <. v .< 1.001) then ArgumentOutOfRangeException.Raise "FsEx.Color.makeLighter: v is bigger than 1.0 or smaller than 0.0: %f" v
        let v = UtilMath.clamp 0. 1. v
        let h,s,l = HSLfromColor c
        let delta = 1.0 - l
        fromHSL (h,s,l + delta * v )

    /// Make a color darker by percentage (value between 0.0 to 1.0) (1.0 = black, 0.0 = current color)
    let makeDarker v c = 
        if not (-0.001 <. v .< 1.001) then ArgumentOutOfRangeException.Raise "FsEx.Color.makeDarker: v is bigger than 1.0 or smaller than 0.0: %f" v
        let v = UtilMath.clamp 0. 1. v
        let h,s,l = HSLfromColor c
        fromHSL (h,s, l - l * v)
