namespace FsEx

open System
open System.Globalization


module UtilMath =
    
    /// American Englisch culture (used for float parsing)
    let enUs = CultureInfo.GetCultureInfo("en-us")
    /// German culture (used for float parsing)
    let deAt = CultureInfo.GetCultureInfo("de-at")
    
    /// First tries to parses english float (period as decimal separator),
    /// if this fails tries to parse german floats,(comma as decimal separator)
    let tryParseFloatEnDe (x:string) : float option=
        match Double.TryParse(x, NumberStyles.Float, enUs) with
        | true, f -> Some f
        | _ ->  match Double.TryParse(x, NumberStyles.Any, deAt) with
                | true, f -> Some f
                | _ -> None
    
    /// First tries to parses english float (period as decimal separator),
    /// if this fails tries to parse german floats,(comma as decimal separator)
    let parseFloatEnDe (x:string) : float =
        match tryParseFloatEnDe x  with
        | Some f -> f
        | None ->   failwithf "Could not parse '%s' into a floating point number using englisch and german culture settings" x

        
    /// Get Float from any input. This helper enables more generic code in parsing sequences
    let inline floatOfObj (o:^T) = 
        match box o with 
        | :? float   as x -> x
        | :? int     as x -> float (x)
        | :? single  as x -> float (x)
        | :? int64   as x -> float (x)
        | :? decimal as x -> float (x)
        | :? string  as x -> parseFloatEnDe (x)
        | _               -> 

            try 
                float(o)
            with _ -> 
                failwithf "Could not convert %s '%A' into a floating point number" (o.GetType().Name) o   
    
    

    /// Allows ints to be multiplied by floats
    ///<c>int(round(float(i) * f))</c> 
    let inline ( *. ) (i:int) (f:float) = int(round(float(i) * f)) // or do it like this:https://stackoverflow.com/questions/2812084/overload-operator-in-f/2812306#2812306
    
    /// Gives a float from int / int division
    ///<c>(float(i)) / (float(j))</c> 
    let inline ( ./. ) (i:int) (j:int) = (float(i)) / (float(j)) // or do it like this:https://stackoverflow.com/questions/2812084/overload-operator-in-f/2812306#2812306

    
    /// Test is a floating point value is Infinity or Not a Number
    let inline isNanOrInf f = Double.IsInfinity f || Double.IsNaN f
    
    /// To make sure a value is within a given range
    /// min -> max -> value -> clamped value.  
    let inline clamp min max value = 
        if max < min then failwithf "clamp: max value %g must be bigger than min %g" max min
        if value > max then max elif value < min then min else value

    let internal rand = System.Random () 

    /// given mean  and standardDeviation returns a random value from this Gaussian distribution
    /// if mean is 0 and stDev is 1 then 99% of values are  are within -2.3 to +2.3 ; 70% within -1 to +1
    let randomStandardDeviation mean standardDeviation =
        let u1 = rand.NextDouble()
        let u2 = rand.NextDouble()
        let randStdNormal = Math.Sqrt(-2.0 * Math.Log(u1)) * Math.Sin(2.0 * Math.PI * u2) //random normal(0, 1)
        //random normal(mean, stdDev^2)
        mean + standardDeviation * randStdNormal 

      
    /// Compares two floating point numbers within a relative tolerance for equality. 
    /// The comparing tolerance is calculated as:  relativeTolerance * (the smaller of the two float arguments).
    let inline areSameRel relativeTolerance (a:float) (b:float)  = 
        let mi = min (abs a) (abs b)
        abs(a-b) < relativeTolerance*mi
    
    /// Compares two floating point numbers to be within a tolerance for equality
    let inline areSame absoluteTolerance (a:float) (b:float)  = 
        abs(a-b) < absoluteTolerance
    
    /// Shadows the built in acos function to include clamping if values are slightly above -1.0 or 1.0
    /// This is useful on dot products from unit vectors
    let acos x =
        if x < -1.00001 then failwithf "acos failed on %g" x
        if x >  1.00001 then failwithf "acos failed on %g" x
        else x |> clamp -1.0 1.0 |> System.Math.Acos

    /// Converts Angels from Degrees to Radians
    let inline toRadians degrees = 0.0174532925199433 * degrees // 0.0174532925199433 = Math.PI / 180. 

    /// Converts Angels from Radians to Degrees
    let inline toDegrees radians = 57.2957795130823 * radians // 57.2957795130823 = 180. / Math.PI

    let inline interpolate start ende (rel:float) = start + ( (ende-start) * rel )


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
    let asBinaryString (n:int) =  
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



    /// NumericSteping: Converts floats to ints, devides by precicion
    let inline precInt (prec:float) (v:float) : int = int (v / prec)
    
    /// NumericSteping:Converts floats to ints within defined integer step sizes, (always rounding down like the int function)
    let inline stepedInt (prec:int) (v:float) : int = int (v / float prec) * prec
    
    /// NumericSteping:Converts floats to floats within defined float step sizes, (always rounding down like the int function)
    let inline stepedFloat (prec:float) (v:float) : float = float (int (v / prec)) * prec
    
    /// This float range function ensures that the end is always included.
    /// because [0.0 .. 0.1 .. 0.2 ] equals [0.0 .. 0.1 .. 0.3 ]
    /// It increases the stop value by the smallest step possible 5 times
    let floatRange (start, step, stop) =
        if step = 0.0 then  failwithf "UtilMath.floatRange:stepsize cannot be zero: start: %g step: %g stop: %g " start step stop
        let range = stop - start 
                    |> BitConverter.DoubleToInt64Bits 
                    |> (+) 5L 
                    |> BitConverter.Int64BitsToDouble
        let steps = range/step
        if steps < 0.0 then failwithf "UtilMath.floatRange:stop value cannot be reached: start: %g step: %g stop: %g " start step stop
        let rec frange (start, i, steps) =
            seq { if i <= steps then 
                    yield start + i*step
                    yield! frange (start, (i + 1.0), steps) }
        frange (start, 0.0, steps)   

        

