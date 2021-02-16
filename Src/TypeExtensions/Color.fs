namespace FsEx

open System
open System.Drawing
open System.Runtime.CompilerServices



[<AutoOpen>]
module TypeExtensionsColor =      


    
    type Drawing.Color with        
        /// Compare to another color only by Alpha, Red, Green and Blue values ignoring other fields such as IsNamedColor        
        [<Extension>] 
        member inline this.EqualsARGB(other:Drawing.Color)=
            this.A = other.A && 
            this.R = other.R && 
            this.G = other.G && 
            this.B = other.B        
        
        /// Compare two colors only by Alpha, Red, Green and Blue values ignoring other fields such as IsNamedColor
        [<Extension>]
        static member inline AreEqualARGB (this:Drawing.Color)(other:Drawing.Color)=
            this.EqualsARGB(other)


module Color =
        open FsEx.CompareOperators

        let internal Rand = new Random(int <| System.DateTime.Now.TimeOfDay.TotalMilliseconds * 0.0129547) // random seed different than other modules
        
        /// Given Hue,Saturation,Luminance in range of 0.0 to 1.0, returns a Drawing.Color 
        let colorFromHSL (H,S,L) = 
            // from http://stackoverflow.com/questions/2942/hsl-in-net
            // or http://bobpowell.net/RGBHSB.aspx
            // allow some numerical error:
            if -0.001 >. H .> 1.001 then failwithf " *colorFromHSL: H is bigger than 1.0 or smaller than 0.0: %f" H
            if -0.001 >. S .> 1.001 then failwithf " *colorFromHSL: S is bigger than 1.0 or smaller than 0.0: %f" S
            if -0.001 >. L .> 1.001 then failwithf " *colorFromHSL: L is bigger than 1.0 or smaller than 0.0: %f" L
            let H = UtilMath.clamp 0. 1. H
            let S = UtilMath.clamp 0. 1. S
            let L = UtilMath.clamp 0. 1. L
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
                        | x -> failwithf "Error in HLS Transform, sextant is %A at HSL %A" x (H,S,L)
                else
                    L,L,L // default to gray value

            Color.FromArgb (int <| 255.* r ,  int <| 255.* g , int <| 255.* b )

        /// Returns a color value from gradient blue to green  to yellow to red, excludes purple        
        /// Input value to range from 0.0 to 1.0   
        /// Will fail on to small or too big values,
        /// but up to a tolerance of  0.001 outside of this range values wil be clamped)
        let colorFromInterval v =  
            if -0.001 >. v .> 1.001 then failwithf " *colorFromInterval: v is bigger than 1.0 or smaller than 0.0: %f" v
            let v = UtilMath.clamp 0. 1. v  
            let v = (1.0 - v) * 0.7 // 0.66666666 // to NOT make full color cirle, that means to exclude the purple values.
            colorFromHSL (v,1.0,0.5)

        /// Given a Drawing.Color , returns Hue,Saturation,Luminance in range of 0.0 to 1.0
        let HSLfromColor (color:Color) = color.GetHue()/360.0f |> float, 
                                         color.GetSaturation() |> float, 
                                         color.GetBrightness() |> float
    
        /// Generates a Random color 
        let randomColor () =  Color.FromArgb (Rand.Next(0,256), Rand.Next(0,256), Rand.Next(0,256))    
    
        /// Generates a Random color with high saturation probability, exluding yellow colors
        /// These are ideal for layer color in Rhino3d CAD app
        let rec randomColorForRhino () =
            let hue = Rand.NextDouble()       
            let sat = UtilMath.randomStandardDeviation 1.0 0.3  |> (fun x -> if x > 1. then 2.-x else x ) |>  UtilMath.clamp 0.1 1.0 
            let lum = UtilMath.randomStandardDeviation 0.5 0.1                                            |>  UtilMath.clamp 0.2 0.8 // to avoid total white (1.0) or black (0.0)
            if     hue < 0.19 && hue > 0.12 // to avoid a color close to yellow
                && sat > 0.8  && lum > 0.3 
                && lum < 0.7  then 
                    randomColorForRhino ()
            else
                colorFromHSL (hue,sat,lum)
        
        /// Make a color lighter by perecentage (value between 0.0 to 1.0) (1.0 = white, 0.0 = current color) 
        let makeLighter v c =
            if -0.001 >. v .> 1.001 then failwithf " *makeLighter: v is bigger than 1.0 or smaller than 0.0: %f" v
            let v = UtilMath.clamp 0. 1. v            
            let h,s,l = HSLfromColor c
            let delta = 1.0 - l
            colorFromHSL (h,s,l + delta * v)

        /// Make a color darker by perecentage (value between 0.0 to 1.0) (1.0 = black, 0.0 = current color) 
        let makeDarker v c =
            if -0.001 >. v .> 1.001 then failwithf " *makeDarker: v is bigger than 1.0 or smaller than 0.0: %f" v
            let v = UtilMath.clamp 0. 1. v 
            let h,s,l = HSLfromColor c            
            colorFromHSL (h,s, l - l * v)