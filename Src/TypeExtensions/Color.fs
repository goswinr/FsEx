namespace FsEx

open System
open FsEx.CompareOperators

module internal ColorUtil = 
    
    let Rand = 
        let now = DateTime.Now
        let seed = now.TimeOfDay
        new Random(int <| seed.TotalMilliseconds * 0.001295473) // random seed different than other modules
    
    let mutable lastHue = 0.0
    
    let mutable lumUp = false
    
    /// Generates a Random color with high saturation probability, excluding yellow colors
    /// These are ideal for layer color in Rhino3d CAD app
    /// Using golden-ratio-loop subsequent colors will have very distinct hues
    let rec randomForRhino () =  
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
            lastHue, s, l

/// don't use System.Drawing ! https://docs.microsoft.com/en-us/dotnet/core/compatibility/core-libraries/6.0/system-drawing-common-windows-only

/// A Red-Green-Blue Color made up of 3 bytes.
/// ( NOT using System.Drawing.Color internally.)
[<Struct;NoComparison>]
type Color =
    /// Gets the Red part of this Red-Green-Blue Color 
    val Red : byte
    
    /// Gets the Green part of this Red-Green-Blue Color 
    val Green : byte

    /// Gets the Blue part of this Red-Green-Blue Color 
    val Blue : byte

    /// Create a new Red-Green-Blue Color 
    new (r,g,b) = {Red=r; Green=g; Blue=b}  
    
    /// Format FsEx.Color into string including type name.
    override c.ToString() = sprintf "FsEx.Color: Red=%d Green=%d Blue=%d" c.Red c.Green c.Blue

    /// Accepts any type that has a R, G and B (UPPERCASE) member that can be converted to a byte. 
    /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
    static member inline ofRGB c :Color = 
        let r = ( ^T : (member R : _) c)
        let g = ( ^T : (member G : _) c)
        let b = ( ^T : (member B : _) c)
        try Color(byte r, byte g, byte b) 
        with e -> ArgumentException.RaiseBase "FsEx.Color.ofRGB: %O could not be converted to a FsEx.Color:\r\n%A" c e

    /// Compares to another color only by Red, Green and Blue values ignoring other fields such as IsNamedColor in System.Drawing.Color
    /// Accepts any two type that have a R, G and B (UPPERCASE) member that can be converted to a int. 
    /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
    static member inline areSameRGB a b :bool = 
        let ra = ( ^T : (member R : _) a)
        let ga = ( ^T : (member G : _) a)
        let ba = ( ^T : (member B : _) a)

        let rb = ( ^T : (member R : _) b)
        let gb = ( ^T : (member G : _) b)
        let bb = ( ^T : (member B : _) b)
        try 
            int ra = int rb && int ga = int gb && int ba = int bb 
        with e -> 
            ArgumentException.RaiseBase "FsEx.Color.areSameRGB: %O or %O could not be converted to a FsEx.Color:\r\n%A" a b e

    /// Compares to another color only by Alpha, Red, Green and Blue values ignoring other fields such as IsNamedColor in System.Drawing.Color
    /// Accepts any two type that have a A, R, G and B (UPPERCASE) member that can be converted to a int. 
    /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
    static member inline areSameARGB a b :bool = 
        let aa = ( ^T : (member A : _) a)
        let ra = ( ^T : (member R : _) a)
        let ga = ( ^T : (member G : _) a)
        let ba = ( ^T : (member B : _) a)

        let ab = ( ^T : (member A : _) b)
        let rb = ( ^T : (member R : _) b)
        let gb = ( ^T : (member G : _) b)
        let bb = ( ^T : (member B : _) b)
        try 
            int aa = int ab && int ra = int rb && int ga = int gb && int ba = int bb 
        with e -> 
            ArgumentException.RaiseBase "FsEx.Color.areSameARGB: %O or %O could not be converted to a FsEx.Color:\r\n%A" a b e


    /// Given Hue, Saturation, Luminance in range of 0.0 to 1.0, returns a FsEx.Color
    /// Will fail with ArgumentOutOfRangeException on too small or too big values,
    /// but up to a tolerance of 0.001 values will be clamped to 0 or 1.
    static member fromHSL (hue:float, saturation:float, luminance:float) = 
        // from http://stackoverflow.com/questions/2942/hsl-in-net
        // or http://bobpowell.net/RGBHSB.aspx
        // allow some numerical error:
        if not (-0.001 <. hue        .< 1.001) then ArgumentOutOfRangeException.Raise "FsEx.Color.createFromHSL: H is bigger than 1.0 or smaller than 0.0: %f" hue
        if not (-0.001 <. saturation .< 1.001) then ArgumentOutOfRangeException.Raise "FsEx.Color.createFromHSL: S is bigger than 1.0 or smaller than 0.0: %f" saturation
        if not (-0.001 <. luminance  .< 1.001) then ArgumentOutOfRangeException.Raise "FsEx.Color.createFromHSL: L is bigger than 1.0 or smaller than 0.0: %f" luminance
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
                    | x -> ArgumentException.RaiseBase "FsEx.Color.createFromHSL: Error in internal HLS Transform, sextant is %d at Hue=%g, Saturation=%g, Luminance=%g" x H S L
            else
                L,L,L // default to gray value
        Color (byte(round(255.* r)) ,  byte(round(255.* g)) , byte(round(255.* b)) )
    
    /// Converts an RGB color value to HSL. Hue, Saturation and  Luminance
    /// Returns Hue,Saturation,Luminance each in in range from 0.0 to 1.0
    static member toHSL (c:Color) : float*float*float =
        /// adapted from https://stackoverflow.com/a/29316972/969070
        let  r = float c.Red / 255.
        let  g = float c.Green / 255.
        let  b = float c.Blue / 255.
        let max = 
            if r > g && r > b then r 
            elif g > b then g 
            else b
        let min = 
            if r < g && r < b then  r 
            elif g < b then  g 
            else b
        let mutable h = 0.0 
        let mutable s = 0.0 
        let mutable l = 0.0 
        l <- (max + min) / 2.0
        if max <> min then 
            let d = max - min;
            s <- if  l > 0.5 then  d / (2.0 - max - min) else  d / (max + min)

            if r > g && r > b then 
                h <- (g - b) / d + (if g < b then 6.0 else 0.0)
            elif (g > b) then 
                h <- (b - r) / d + 2.0
            else
                h <- (r - g) / d + 4.0
            h <- h /  6.0 
        h, s, l

    /// Returns a color value from gradient blue to green to yellow to red, excludes purple
    /// Input value to range from 0.0 to 1.0
    /// Will fail with ArgumentOutOfRangeException on too small or too big values,
    /// but up to a tolerance of 0.001 values will be clamped to 0 or 1.
    static member gradientFromInterval (v) = 
        if not (-0.001 <. v .< 1.001) then ArgumentOutOfRangeException.Raise " FsEx.Color.gradientFromInterval: v is bigger than 1.0 or smaller than 0.0: %f" v
        let v = UtilMath.clamp 0. 1. v
        let v = (1.0 - v) * 0.7 // 0.66666666 // to NOT make full color circle, that means to exclude the purple values.
        Color.fromHSL (v,1.0,0.5)

    /// Returns a color value from gradient blue to green to yellow to red, excludes purple
    /// Input value to range from 0.0 to 1.0
    /// Will clamp on too small or too big values.
    static member gradientFromIntervalClamped (v) = 
        let v = UtilMath.clamp 0. 1. v
        let v = (1.0 - v) * 0.7 // 0.66666666 // to NOT make full color circle, that means to exclude the purple values.
        Color.fromHSL (v,1.0,0.5)

    /// Generates a Random color with high saturation probability, excluding yellow colors
    /// These are ideal for layer color in Rhino3d CAD app.
    /// The colors returned by subsequent calls to this functions will have very distinct hues.
    /// This is achieved by using a golden-ratio-loop and an internal cache of the last generated color.
    static member randomForRhino () =  
        let h, s, l = ColorUtil.randomForRhino()
        Color.fromHSL (h,s,l)

    /// Make a color lighter by percentage (value between 0.0 to 1.0) (1.0 = white, 0.0 = current color)
    static member makeLighter v c = 
        if not (-0.001 <. v .< 1.001) then ArgumentOutOfRangeException.Raise "FsEx.Color.makeLighter: v is bigger than 1.0 or smaller than 0.0: %f" v
        let v = UtilMath.clamp 0. 1. v
        let h,s,l = Color.toHSL c
        let delta = 1.0 - l
        Color.fromHSL (h,s,l + delta * v )

    /// Make a color darker by percentage (value between 0.0 to 1.0) (1.0 = black, 0.0 = current color)
    static member makeDarker v c = 
        if not (-0.001 <. v .< 1.001) then ArgumentOutOfRangeException.Raise "FsEx.Color.makeDarker: v is bigger than 1.0 or smaller than 0.0: %f" v
        let v = UtilMath.clamp 0. 1. v
        let h,s,l = Color.toHSL c
        Color.fromHSL (h,s, l - l * v)
