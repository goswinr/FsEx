namespace FsEx

open System
open System.Globalization


module UtilMath =
    
    let enUs = CultureInfo.GetCultureInfo("en-us")
    let deAt = CultureInfo.GetCultureInfo("de-at")
    
    ///First tries to parses english float (period as decimal separator),
    ///if this fails tries to parse german floats,(comma as decimal separator)
    let parseFloatEnDe (x:string) =
        match Double.TryParse(x, NumberStyles.Float, enUs) with
        | true, f -> f
        | _ ->  match Double.TryParse(x, NumberStyles.Any, deAt) with
                | true, f -> f
                | _ -> failwithf "Could not parse '%s' into a floating point number using englisch and german culture settings" x
        
    ///Get Float from any input. This helper enables more generic code in parsing sequences
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
    
    let internal rand = System.Random () 

    ///Allows ints to be multiplied by floats
    ///<c>int(round(float(i) * f))</c> 
    let inline ( *. ) (i:int) (f:float) = int(round(float(i) * f)) // or do it like this:https://stackoverflow.com/questions/2812084/overload-operator-in-f/2812306#2812306
    
    ///Gives a float from int / int division
    ///<c>(float(i)) / (float(j))</c> 
    let inline ( /. ) (i:int) (j:int) = (float(i)) / (float(j)) // or do it like this:https://stackoverflow.com/questions/2812084/overload-operator-in-f/2812306#2812306

    ///if first value is 0.0 return second else first
    let internal ifZero1 a b = if a = 0.0 then b else a
    
    ///if second value is 0.0 return first else second
    let internal ifZero2 a b = if b = 0.0 then a else b

    //gives a int from int / float division
    //int(round( float(i) / j ))
    //let inline ( /| ) (i:int) (j:float) = int(round( float(i) / j ))
    
    ///Test is a floating point value is Infinity or Not a Number
    let inline isNanOrInf f = Double.IsInfinity f || Double.IsNaN f

    ///given mean  and standardDeviation returns a random value from this Gaussian distribution
    ///if mean is 0 and stDev is 1 then 99% of values are  are within -2.3 to +2.3 ; 70% within -1 to +1
    let randomStandardDeviation mean standardDeviation =
        let u1 = rand.NextDouble()
        let u2 = rand.NextDouble()
        let randStdNormal = Math.Sqrt(-2.0 * Math.Log(u1)) * Math.Sin(2.0 * Math.PI * u2) //random normal(0, 1)
        //random normal(mean, stdDev^2)
        mean + standardDeviation * randStdNormal 


    let private piOne80 = Math.PI / 180.   // precompute division

    let private one80Pi = 180. / Math.PI // precompute division

    ///converts Angels from Degrees to Radians
    let toRadians degrees = piOne80 * degrees

    ///converts Angels from Radians to Degrees
    let toDegrees radians = one80Pi * radians
    

        

