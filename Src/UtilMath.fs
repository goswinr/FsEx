namespace FsEx

open System
open System.Globalization


/// Math Utils
/// when opened shadows the built in trigonometric asin and acos function to include clamping if values are slightly above -1.0 or 1.0
module UtilMath =
    
    /// American Englisch culture (used for float parsing)
    let enUs = CultureInfo.GetCultureInfo("en-us")
    
    /// German culture (used for float parsing)
    let deAt = CultureInfo.GetCultureInfo("de-at")
    
    /// First tries to parses float with En-Us CultureInfo (period as decimal separator),
    /// if this fails tries to parse parses float with De-At CultureInfo (comma as decimal separator)
    let tryParseFloatEnDe (x:string) : float option=
        match Double.TryParse(x, NumberStyles.Float, enUs) with
        | true, f -> Some f
        | _ ->  match Double.TryParse(x, NumberStyles.Any, deAt) with
                | true, f -> Some f
                | _ -> None
    
    /// First tries to parses float with En-Us CultureInfo (period as decimal separator),
    /// if this fails tries to parse parses float with De-At CultureInfo (comma as decimal separator)
    let parseFloatEnDe (x:string) : float =
        match tryParseFloatEnDe x  with
        | Some f -> f
        | None ->   failwithf "Could not parse '%s' into a floating point number using englisch and german culture settings" x    


    /// A very tolerant custom float parser
    /// ignores all non numeric characters ( expect leading '-' )
    /// and considers '.' and  ',' as decimal point
    /// does not allow for scientific notation
    let tryParseFloatTolerant(s:string) =
        let sb = Text.StringBuilder(s.Length)
        for c in s do
            if c >= '0' && c <= '9' then sb.Append(c) |> ignore
            elif c = '.' then sb.Append(c) |> ignore
            elif c = '-' && sb.Length = 0  then sb.Append(c) |> ignore //only add minus at start
            elif c = ',' then sb.Append('.') |> ignore // german formating
        match Double.TryParse(sb.ToString(), NumberStyles.Float, enUs) with
        | true, f -> Some f
        | _ ->   None 

    
    // never used::

    // Get Float from any input. This helper enables more generic code in parsing sequences
    //let inline floatOfObj (o:^T) = 
    //    match box o with 
    //    | :? float   as x -> x
    //    | :? int     as x -> float (x)
    //    | :? single  as x -> float (x)
    //    | :? int64   as x -> float (x)
    //    | :? decimal as x -> float (x)
    //    | :? string  as x -> parseFloatEnDe (x)
    //    | _               -> 
    //        try 
    //            float(o)
    //        with _ -> 
    //            failwithf "Could not convert %s '%A' into a floating point number" (o.GetType().Name) o   
    
    // better define yourself when needed:

    // Allows ints to be multiplied by floats, returns int
    // <c>int(round(float(i) * f))</c> 
    //let inline ( *. ) (i:int) (f:float) = int(round(float(i) * f)) // or do it like this:https://stackoverflow.com/questions/2812084/overload-operator-in-f/2812306#2812306
    
    // Gives a float from int / int division
    // <c>(float(i)) / (float(j))</c> 
    //let inline ( ./. ) (i:int) (j:int) = (float(i)) / (float(j)) // or do it like this:https://stackoverflow.com/questions/2812084/overload-operator-in-f/2812306#2812306

    
    /// Test is a floating point value is Infinity or Not a Number
    let inline isNanOrInf f = Double.IsInfinity f || Double.IsNaN f
    
    /// To make sure a value is within a given range
    /// min -> max -> value -> clamped value.  
    let inline clamp min max value = 
        if max < min then failwithf "clamp: max value %g must be bigger than min %g" max min
        if value > max then max elif value < min then min else value

    /// To make sure a value is between 0.0 and 1.0 range    
    let inline clamp01 (value:float<'T>)  =         
        if   value > 1.0<_> then 1.0<_> 
        elif value < 0.0<_> then 0.0<_> 
        else value

    let internal rand = System.Random () 

    /// Checks if a number is between or on a lower and upper bound value . 
    /// x >= minVal && x <= maxVal
    let inline isInRange (minVal:'T) (maxVal:'T) (x:'T) = 
        x >= minVal && x <= maxVal

    /// Checks if a number is bigger or smaller than a lower and upper bound value . 
    /// x < minVal || x > maxVal
    let inline isNotInRange (minVal:'T) (maxVal:'T) (x:'T)  = 
        x < minVal || x > maxVal

    /// Given mean and standardDeviation returns a random value from this Gaussian distribution
    /// if mean is 0 and stDev is 1 then 99% of values are  are within -2.3 to +2.3 ; 70% within -1 to +1
    let randomStandardDeviation mean standardDeviation =
        let u1 = rand.NextDouble()
        let u2 = rand.NextDouble()
        let randStdNormal = Math.Sqrt(-2.0 * Math.Log(u1)) * Math.Sin(2.0 * Math.PI * u2) //random normal(0, 1)
        //random normal(mean, stdDev^2)
        mean + standardDeviation * randStdNormal 
      
    /// Compares two numbers within a relative tolerance for equality. 
    /// The comparing tolerance is calculated as:  
    /// let mi = min (abs a) (abs b)
    /// abs(a-b) < relativeTolerance * mi
    let inline equalsWithRelativeTolerance (relativeTolerance:float) (valueA:float<'T>) (valueB:float<'T>)  = 
        let mi = min (abs valueA) (abs valueB)
        abs(valueA - valueB) < relativeTolerance * mi
    
    /// Compares two numbers to be within a tolerance for equality
    /// abs(a-b) < absoluteTolerance
    let inline equalsWithTolerance (absoluteTolerance:float<'T>) (valueA:float<'T>) (valueB:float<'T>)  = 
        abs(valueA - valueB) < absoluteTolerance
    
    /// Shadows the built in 'acos' (Invers Cosine) function to include clamping if values are slightly above -1.0 or 1.0
    /// Tolerance: 0.00001 
    /// This is useful on dot products from unit vectors
    /// returns angel in Radians
    let acos x =
        if x < -1.00001 then failwithf "acos failed on %f , input must be between -1.00001 and +1.00001" x
        if x >  1.00001 then failwithf "acos failed on %f , input must be between -1.00001 and +1.00001" x
        else x |> clamp -1.0 1.0 |> System.Math.Acos

    /// Shadows the built in 'asin' (Invers Sine) function to include clamping if values are slightly above -1.0 or 1.0
    /// Tolerance: 0.00001 
    /// returns angel in Radians
    let asin x =
        if x < -1.00001 then failwithf "asin failed on %f , input must be between -1.00001 and +1.00001" x
        if x >  1.00001 then failwithf "asin failed on %f , input must be between -1.00001 and +1.00001" x
        else x |> clamp -1.0 1.0 |> System.Math.Asin

    /// Converts Angels from Degrees to Radians
    let inline toRadians degrees = 0.0174532925199433 * degrees // 0.0174532925199433 = Math.PI / 180. 

    /// Converts Angels from Radians to Degrees
    let inline toDegrees radians = 57.2957795130823 * radians // 57.2957795130823 = 180. / Math.PI

    /// start + ( (ende-start) * rel )
    let inline interpolate (start:float<'T>)  (ende:float<'T>)  (rel:float) = 
        start + ( (ende-start) * rel )
    
    /// multiplicative inverse or reciprocal:
    /// 1/x
    let inline reciprocal (x:float<'T>) : float</'T> = 1. / x

    /// multiplicative inverse or reciprocal:
    /// 1/x
    let inline inverse (x:float<'T>) : float</'T> = 1. / x

    /// Given the min and max value and a test value,  (val-min) / (max-min)
    /// Returns the relative  position  of the test value between min (= 0.0) and (max = 1.0),
    /// can also be bigger than 1.0
    let inline normalize rangeStart rangeStop valueAt =  (rangeStart-valueAt)/(rangeStart-rangeStop) // div 0 ?
    
    /// Rounds to the next biggest (away from zero) number on logaritmic scale.
    /// Define scale by giving amount of steps(int) to double or half a value.
    let roundUpToNextLogSteps (stepsToDouble:int) x =
        if isNanOrInf x then failwithf "*** roundUpToNextLogSteps failed on: %f" x
        elif stepsToDouble < 0 then failwithf "*** roundUpToNextLogSteps failed on: int stepsToDouble must be bigger than 0" 
        elif x = 0.0 then 0.0
        else
            let logBase =  2. ** (1./float stepsToDouble) 
            if x > 0.0 then   logBase ** (Math.Ceiling (Math.Log (    x, logBase)))
            else            -(logBase ** (Math.Ceiling (Math.Log (abs x, logBase)))) // with negative sign, (log fails on negative numbers)

    /// Rounds to the next smaller (closer to zero) number on logaritmic scale
    /// Define scale by giving amount of steps(int) to double or half a value.
    let roundDownToNextLogSteps (stepsToDouble:int) x =
        if isNanOrInf x then failwithf "*** roundDownToNextLogSteps failed on: %f" x
        elif stepsToDouble < 0 then failwithf "*** roundDownToNextLogSteps failed on: int stepsToDouble must be bigger than 0" 
        elif x = 0.0 then 0.0
        else
            let logBase =  2. ** (1./float stepsToDouble) 
            if x > 0.0 then   logBase ** (Math.Floor (Math.Log (    x, logBase)))
            else            -(logBase ** (Math.Floor (Math.Log (abs x, logBase)))) // with negative sign, (log fails on negative numbers)
    
    /// Converts an Int32 to a string of 32 characters of '1' or '0'.
    let asBinaryString (n:int) : string =  
        // or System.Convert.ToString (n,2)
        let b = Array.zeroCreate 32
        let mutable pos = 31
        let mutable i = 0
        while i < 32 do  
            if ((n &&& (1 <<< i)) <> 0) then  b.[pos] <- '█' //'1'                
            else                              b.[pos] <- '0'                
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


    /// Numeric Steping: Converts floats to ints, devides by precicion.
    /// = int (v / prec)
    let inline precInt (prec:float) (v:float) : int = int (v / prec)
    
    /// Numeric Steping:Converts floats to ints within defined integer step sizes. 
    /// Always rounding down like the int function
    /// = int (v / float prec) * prec
    let inline stepedInt (prec:int) (v:float) : int = int (v / float prec) * prec
    
    /// Numeric Steping:Converts floats to floats within defined float step sizes. 
    /// Always rounding down like the int function)
    /// = float (int (v / prec)) * prec
    let inline stepedFloat (prec:float) (v:float) : float = float (int (v / prec)) * prec

    /// Numeric Steping:Converts floats to floats within defined float step sizes. 
    /// Always rounding mid point  like the round function)
    /// =  (round (v / prec)) * prec
    let inline stepedFloatMid (prec:float) (v:float) : float = (round (v / prec)) * prec
  
    /// This float range function ensures that the end is always included.
    /// The F# build in range fails for example on [0.0 .. 0.1 .. 0.2 ] , it equals [0.0 .. 0.1 .. 0.3 ]
    /// It increases the stop value by the smallest step possible 15 times, to ensure end value is included in returned seq.
    let floatRange (start:float<'T>, step:float<'T> , stop:float<'T>) : seq<float<'T>> =        
        if step = LanguagePrimitives.FloatWithMeasure<'T> 0.0 then  failwithf "UtilMath.floatRange:stepsize cannot be zero: start: %f step: %f stop: %f " start step stop
        let range = stop - start 
                    |> float
                    |> BitConverter.DoubleToInt64Bits //https://float.exposed/0x3ff0000000000000
                    |> (+) 15L 
                    |> BitConverter.Int64BitsToDouble
                    |> LanguagePrimitives.FloatWithMeasure
        let steps = range/step 
        if steps < 0.0 then failwithf "UtilMath.floatRange:stop value cannot be reached: start: %f step: %f stop: %f " start step stop
        let rec frange (start, i, steps) =
            seq { if i <= steps then 
                    yield start + i*step
                    yield! frange (start, (i + 1.0 ), steps) }
        frange (start, 0.0 , steps)   

        

