﻿namespace FsEx

open System
open System.Globalization
open System.Collections.Generic

/// Math Utils.
/// When opened the module shadows the built in trigonometric asin and acos function to include clamping if values are slightly above -1.0 or 1.0.
module UtilMath = 

    /// Test is a floating point number (with Measure)  is NaN (Not a Number) or Infinity.
    let inline isNanOrInf (f:float<'T>) = Double.IsInfinity (float f) || Double.IsNaN (float f)

    /// Test is a floating point number (with Measure)  is NaN (Not a Number)
    let inline isNan (f:float<'T>) = Double.IsNaN (float f)

    /// To make sure a value is within a given range.
    /// minVal -> maxVal -> x -> clamped value.
    let inline clamp (minVal:'T) (maxVal:'T) (x:'T) :'T = 
        if maxVal < minVal then ArgumentOutOfRangeException.Raise "FsEx.UtilMath.clamp: max value %g must be bigger than min %g" maxVal minVal
        if x > maxVal then maxVal elif x < minVal then minVal else x

    /// Shadows the built in 'acos' (Inverse Cosine) function to include clamping if values are slightly above -1.0 or 1.0.
    /// Tolerance: 0.00001
    /// This is useful on dot products from unit vectors.
    /// Returns angel in Radians.
    let inline acos (x:float<'T>) : float =  // no measure on returned float !
        if isNanOrInf x then raise <| ArgumentException "FsEx.UtilMath.acos: given input is NaN or Infinity."
        if x < -1.00001<_> then ArgumentOutOfRangeException.Raise "FsEx.UtilMath.acos failed on %f , input must be between -1.00001 and +1.00001" x
        if x >  1.00001<_>  then ArgumentOutOfRangeException.Raise "FsEx.UtilMath.acos failed on %f , input must be between -1.00001 and +1.00001" x
        else x  |> float|> clamp -1.0 1.0 |> System.Math.Acos
        
    /// Shadows the built in 'asin' (Inverse Sine) function to include clamping if values are slightly above -1.0 or 1.0.
    /// Tolerance: 0.00001
    /// Returns angel in Radians.
    let inline asin (x:float<'T>) : float= // no measure on returned float !
        if isNanOrInf x then raise <| ArgumentException "FsEx.UtilMath.asin: given input is NaN or Infinity."
        if x < -1.00001<_> then ArgumentOutOfRangeException.Raise "FsEx.UtilMath.asin failed on %f , input must be between -1.00001 and +1.00001" x
        if x >  1.00001<_> then ArgumentOutOfRangeException.Raise "FsEx.UtilMath.asin failed on %f , input must be between -1.00001 and +1.00001" x
        else x |> float |> clamp -1.0 1.0  |> System.Math.Asin 

    /// Converts Angels from Degrees to Radians.
    let inline toRadians (degrees:float<'T>):float =  // no measure on returned float !
        if isNanOrInf degrees then raise <| ArgumentException "FsEx.UtilMath.toRadians: given input is NaN or Infinity."
        0.0174532925199433 * float degrees // 0.0174532925199433 = Math.PI / 180.

    /// Converts Angels from Radians to Degrees.
    let inline toDegrees (radians:float<'T>) :float = // no measure on returned float !
        if isNanOrInf radians then raise <| ArgumentException "FsEx.UtilMath.toDegrees: given input is NaN or Infinity."
        57.2957795130823 * float radians // 57.2957795130823 = 180. / Math.PI


    /// American English culture (used for float parsing).
    let private enUs = CultureInfo.GetCultureInfo("en-US")

    /// German culture (used for float parsing).
    let private deAt = CultureInfo.GetCultureInfo("de-DE")

    /// First tries to parses float with En-Us CultureInfo (period as decimal separator),
    /// if this fails tries to parse parses float with De-At CultureInfo (comma as decimal separator).
    let tryParseFloatEnDe (x:string) : option<float> = 
        match Double.TryParse(x, NumberStyles.Float, enUs) with
        | true, f -> Some f
        | _ ->  match Double.TryParse(x, NumberStyles.Any, deAt) with
                | true, f -> Some f
                | _ -> None

    /// First tries to parses float with En-Us CultureInfo (period as decimal separator).
    /// If this fails tries to parse parses float with De-At CultureInfo (comma as decimal separator).
    /// With unit Of Measure Annotation.
    let tryParseFloatEnDeAs<[<Measure>]'T> (x:string) : option<float<'T>> = 
        match Double.TryParse(x, NumberStyles.Float, enUs) with
        | true, f -> Some (LanguagePrimitives.FloatWithMeasure f)
        | _ ->  match Double.TryParse(x, NumberStyles.Any, deAt) with
                | true, f -> Some (LanguagePrimitives.FloatWithMeasure f)
                | _ -> None

    
    /// First tries to parses float with En-Us CultureInfo (period as decimal separator).
    /// If this fails tries to parse parses float with De-At CultureInfo (comma as decimal separator).
    let parseFloatEnDe (x:string) : float = 
        match tryParseFloatEnDe x  with
        | Some f -> f
        | None ->   ArgumentException.RaiseBase "FsEx.UtilMath.parseFloatEnDe Could not parse '%s' into a floating point number using English or German culture settings" x

    /// First tries to parses float with En-Us CultureInfo (period as decimal separator).
    /// If this fails tries to parse parses float with De-At CultureInfo (comma as decimal separator).
    /// With unit Of Measure Annotation.
    let parseFloatEnDeAs<[<Measure>]'T> (x:string) : float<'T> = 
        match tryParseFloatEnDeAs<'T> x  with
        | Some f -> f
        | None ->   ArgumentException.RaiseBase "FsEx.UtilMath.parseFloatEnDe Could not parse '%s' into a floating point number using English or German culture settings" x

    /// A very tolerant custom float parser.
    /// Ignores all non numeric characters ( expect a minus '-' before any digit )
    /// and considers '.' and  ',' as decimal point.
    /// Does NOT allow for scientific notation !
    let tryParseFloatTolerant(s:string) : option<float> =
        let sb = Text.StringBuilder(s.Length)
        for c in s do
            if   c >= '0' && c <= '9'       then sb.Append(c)   |> ignore
            elif c = '.'                    then sb.Append(c)   |> ignore
            elif c = '-' && sb.Length = 0   then sb.Append(c)   |> ignore //only add minus before digits if stringBuilder is still empty
            elif c = ','                    then sb.Append('.') |> ignore // german formatting
        match Double.TryParse(sb.ToString(), NumberStyles.Float, enUs) with
        | true, f -> Some  f
        | _ ->   None
        
    /// A very tolerant custom float parser, with unit Of Measure Annotation.
    /// Ignores all non numeric characters ( expect a minus '-' before any digit )
    /// and considers '.' and  ',' as decimal point.
    /// Does NOT allow for scientific notation !
    let tryParseFloatTolerantAs<[<Measure>]'T>(s:string) : option<float<'T>> =
        let sb = Text.StringBuilder(s.Length)
        for c in s do
            if   c >= '0' && c <= '9'       then sb.Append(c)   |> ignore
            elif c = '.'                    then sb.Append(c)   |> ignore
            elif c = '-' && sb.Length = 0   then sb.Append(c)   |> ignore //only add minus before digits if stringBuilder is still empty
            elif c = ','                    then sb.Append('.') |> ignore // german formatting
        match Double.TryParse(sb.ToString(), NumberStyles.Float, enUs) with
        | true, f -> Some (LanguagePrimitives.FloatWithMeasure f)
        | _ ->   None
  

    /// To make sure a value is between 0.0 and 1.0 range.
    let inline clamp01 (value:'T) :'T = 
        // if isNan value then ArgumentException.RaiseBase "FsEx.UtilMath.clamp01: given input is NaN."
        if   value > LanguagePrimitives.GenericOne< ^T>  then LanguagePrimitives.GenericOne< ^T>
        elif value < LanguagePrimitives.GenericZero< ^T> then LanguagePrimitives.GenericZero< ^T>
        else value


    /// Checks if a number is between or on a lower and upper bound value.
    /// x >= minVal && x <= maxVal
    let inline isInRange (minVal:'T) (maxVal:'T) (x:'T) :bool = 
        x >= minVal && x <= maxVal

    /// Checks if a number is bigger or smaller than a lower and upper bound value .
    /// x < minVal || x > maxVal
    let inline isNotInRange (minVal:'T) (maxVal:'T) (x:'T) :bool = 
        x < minVal || x > maxVal

    /// Compares two numbers within a relative tolerance for equality.
    /// The comparing tolerance is calculated as:
    /// let mi = min (abs a) (abs b)
    /// abs(a-b) < relativeTolerance * mi
    let inline equalsWithRelativeTolerance (relativeTolerance:float) (valueA:float<'T>) (valueB:float<'T>) :bool  = 
        //if isNanOrInf valueA then raise <| ArgumentException("FsEx.UtilMath.equalsWithRelativeTolerance: given valueA is NaN or Infinity") // don't do this, keep it generic
        //if isNanOrInf valueB then raise <| ArgumentException("FsEx.UtilMath.equalsWithRelativeTolerance: given valueB is NaN or Infinity")
        let mi = min (abs valueA) (abs valueB)
        abs(valueA - valueB) < relativeTolerance * mi

    /// Compares two numbers to be within a tolerance for equality.
    /// abs(a-b) < absoluteTolerance
    let inline equalsWithTolerance (absoluteTolerance:'T) (valueA:'T) (valueB:'T) :bool = 
        //if isNanOrInf valueA then raise <| ArgumentException("FsEx.UtilMath.equalsWithTolerance: given valueA is NaN or Infinity") // don't do this, keep it generic
        //if isNanOrInf valueB then raise <| ArgumentException("FsEx.UtilMath.equalsWithTolerance: given valueB is NaN or Infinity")
        abs(valueA - valueB) < absoluteTolerance


    /// Interpolates between start and end value Generic numbers.
    /// Works on any type that implements  + , - and *
    let inline interpolate (start:'T)  (ende:'T)  (rel:'T) :'T = 
        //if isNanOrInf start then raise <| ArgumentException "FsEx.UtilMath.interpolate: given input for 'start' is NaN or Infinity."
        //if isNanOrInf ende  then raise <| ArgumentException "FsEx.UtilMath.interpolate: given input for 'ende' is NaN or Infinity."
        //if isNanOrInf rel   then raise <| ArgumentException "FsEx.UtilMath.interpolate: given input for 'rel' is NaN or Infinity."
        //start + ( (ende - start) * (float rel) )
        start + ( (ende - start) * rel )


    /// Returns a function to find linear interpolations in one table.
    /// The input table is a sorted (increasing) array of tuples of input and respective output values.
    let interpolateTable (table: IList< float<'Input> * float<'Output> > ) : float<'Input> -> float<'Output> = 

        // make sure it is sorted and always increasing
        for tn in Seq.windowed 2 table do            
            let  t= tn.[0]
            let  n= tn.[1]
            if fst t >= fst n then
                ArgumentException.RaiseBase "FsEx.UtilMath.interpolateTable: Table input is not sorted increasing %A >= %A  :\r\n%s" t  n (NiceString.toNiceString table)

        // return the lookup function
        fun (x:float<'Input>) ->
            if x < fst table.[0] then  
                //failwithf "Table query %A is smaller than first element in table %A" x table.[0]
                let stepIn  = fst table.[1] - fst table.[0]
                let stepOut = snd table.[1] - snd table.[0]
                let distIn  = fst table.[0] - x 
                if abs(float stepIn) < 1e-16 then  
                    ArgumentException.RaiseBase "FsEx.UtilMath.interpolateTable: Table query %g is smaller than first element in table %A and the first two Input elements are almost the same, so a meaningful prediction is not possible." x table.[0]
                    snd table.[0]
                else
                    let sc  = distIn / stepIn 
                    snd table.[0] - stepOut * sc
        
            elif x > fst table.[table.Count-1]  then  
                //failwithf "Table query %A is bigger than last element in table %A"   x table.[table.Length-1] 
                let li = table.Count-1
                let stepIn  = fst table.[li] - fst table.[li-1]
                let stepOut = snd table.[li] - snd table.[li-1]
                let distIn  = x - fst table.[li]
                if abs(float stepIn) < 1e-16 then  // last are duplicates 
                    ArgumentException.RaiseBase "FsEx.UtilMath.interpolateTable: Table query %g is bigger than last element in table %A and the last two Input elements are almost the same, so a meaningful prediction is not possible."   x table.[li] 
                    //snd table.[li]
                else
                    let sc  = distIn / stepIn
                    snd table.[li] + stepOut * sc        
            else
                let hiIdx = table |> Seq.findIndex ( fun v ->  fst v >= x) // finding could be optimized with binary search since its sorted    
                if hiIdx = 0 then
                    snd table.[0]
                else
                    let low,lowOut  = table.[hiIdx-1]
                    let hig,higOut  = table.[hiIdx]
                    let delta = hig - low
                    if abs(float delta) < 1e-16 then // tolerance for div by zero (eg. from  duplicate positions in the list)
                        (low+hig) *0.5
                    else
                        let local = x - low
                        let rel = local / delta
                        let deltaOut = higOut - lowOut
                        let res = lowOut + deltaOut * rel
                        res
    
    
    /// Returns a function to find linear interpolations from two sorted(increasing) Lists of input and respective output values.
    /// Includes a check that both list have the same length
    let interpolateLists (input: float<'Input> IList) ( output: float<'Output> IList ) : (float<'T> ->  float<'U>) = 
        if input.Count <> output.Count then
            ArgumentException.RaiseBase "FsEx.UtilMath.interpolateLists: Tables length don't match:\r\n%s\r\nand %s" (NiceString.toNiceString input)  (NiceString.toNiceString output)        
        interpolateTable(Seq.zip input output|> Array.ofSeq)


    /// Multiplicative inverse or reciprocal:
    /// 1/x
    let inline reciprocal (x:float<'T>) : float< /'T > = 
        if isNanOrInf x then raise <| ArgumentException "FsEx.UtilMath.reciprocal: given input is NaN or Infinity."  // don't do this, keep it generic
        let a = abs(x) // don't do this, keep it generic?
        if a < 1e-16<_> then raise <| ArgumentException "FsEx.UtilMath.reciprocal: given input is almost Zero, less than + or - 1e-16."
        if a > 1e24<_>  then raise <| ArgumentException "FsEx.UtilMath.reciprocal: given input is extremely large, more than + or - 1e24."  // don't do this, keep it generic
        1.0 / x
        //if x = LanguagePrimitives.GenericZero< ^T> then  raise <| ArgumentException "FsEx.UtilMath.reciprocal: given input is Zero"
        //LanguagePrimitives.GenericOne< ^T> / x

    /// Multiplicative inverse or reciprocal:
    /// 1/x
    let inline inverse(x:float<'T>) : float< /'T > = 
        if isNanOrInf x then raise <| ArgumentException "FsEx.UtilMath.inverse: given input is NaN or Infinity."  // don't do this, keep it generic
        let a = abs(x) // don't do this, keep it generic?
        if a < 1e-16<_> then raise <| ArgumentException "FsEx.UtilMath.inverse: given input is almost Zero, less than + or - 1e-16."
        if a > 1e24<_>  then raise <| ArgumentException "FsEx.UtilMath.inverse: given input is extremely large, more than + or - 1e24."  // don't do this, keep it generic
        1.0 / x
        //if x = LanguagePrimitives.GenericZero< ^T> then  raise <| ArgumentException "FsEx.UtilMath.reciprocal: given input is Zero" // generic alternative, not used to have safety checks
        //LanguagePrimitives.GenericOne< ^T> / x

    /// Given the min and max value and a test value,  (val-min) / (max-min)
    /// Returns the relative  position of the test value between min (= 0.0) and (max = 1.0),
    /// can also be bigger than 1.0
    let inline normalize (rangeStart:float<'T>) (rangeStop:float<'T>) (valueAt:float<'T>) :float<'T> = 
        if isNanOrInf rangeStart then raise <| ArgumentException "FsEx.UtilMath.normalize: given input for 'rangeStart' is NaN or Infinity."
        if isNanOrInf rangeStop  then raise <| ArgumentException "FsEx.UtilMath.normalize: given input for 'rangeStop' is NaN or Infinity."
        if isNanOrInf valueAt    then raise <| ArgumentException "FsEx.UtilMath.normalize: given input for 'valueAt' is NaN or Infinity."
        let rangeLength = float (rangeStart - rangeStop)
        let a = abs(rangeLength)
        if a < 1e-16 then raise <| ArgumentException "FsEx.UtilMath.normalize: rangeStart-rangeStop is almost Zero, less than + or - 1e-16."
        if a > 1e24  then raise <| ArgumentException "FsEx.UtilMath.normalize: rangeStart-rangeStop is extremely large, more than + or - 1e24."
        //if range = LanguagePrimitives.GenericZero< ^T> then raise <| ArgumentException "FsEx.UtilMath.normalize: rangeStart-rangeStop is Zero" // generic alternative, not used to have safety checks
        (rangeStart-valueAt)/ rangeLength

    /// Rounds to the next bigger (away from zero) number on logarithmic scale.
    /// Define scale by giving amount of steps(int) to double or half a value.
    /// Fails if input is zero
    let roundUpToNextLogSteps (stepsToDouble:int) (x:float<'T>) :float<'T> = 
        if isNanOrInf x then raise <| ArgumentException "FsEx.UtilMath.roundUpToNextLogSteps: given input for 'rangeStart' is NaN or Infinity."
        if stepsToDouble < 0 then ArgumentOutOfRangeException.Raise "FsEx.UtilMath.roundUpToNextLogSteps failed on: int stepsToDouble must be bigger than 0"
        let a = abs(float x)
        if a < 1e-24 then raise <| ArgumentException "FsEx.UtilMath.roundUpToNextLogSteps: input value is almost Zero, less than + or - 1e-24."

        let logBase =  2. ** (1./float stepsToDouble)
        if x > 0.0<_> then    logBase ** (Math.Ceiling (Math.Log (    float  x , logBase))) |> LanguagePrimitives.FloatWithMeasure
        else                -(logBase ** (Math.Ceiling (Math.Log (abs(float  x), logBase))))|> LanguagePrimitives.FloatWithMeasure // with negative sign, (log fails on negative numbers)

    /// Rounds to the next smaller (closer to zero) number on logarithmic scale
    /// Define scale by giving amount of steps(int) to double or half a value.
    /// Fails if input is zero
    let roundDownToNextLogSteps  (stepsToDouble:int) (x:float<'T>) :float<'T> = 
        if isNanOrInf x then raise <| ArgumentException "FsEx.UtilMath.roundDownToNextLogSteps: given input for 'rangeStart' is NaN or Infinity."
        if stepsToDouble < 0 then ArgumentOutOfRangeException.Raise "FsEx.UtilMath.roundDownToNextLogSteps failed on: int stepsToDouble must be bigger than 0 but is %d" stepsToDouble
        let a = abs(float x)
        if a < 1e-24 then raise <| ArgumentException "FsEx.UtilMath.roundDownToNextLogSteps: input value is almost Zero, less than + or - 1e-24."

        let logBase =  2. ** (1./float stepsToDouble)
        if x > 0.0<_> then   logBase ** (Math.Floor (Math.Log (    float  x , logBase)))  |> LanguagePrimitives.FloatWithMeasure
        else               -(logBase ** (Math.Floor (Math.Log (abs(float  x), logBase)))) |> LanguagePrimitives.FloatWithMeasure// with negative sign, (log fails on negative numbers)


    /// Numeric Stepping: Converts floats to ints, divided by precision.
    /// Like rounding floats to integers but with another step size than 1.0
    /// = int (v / precision)
    let inline precisionInt (precision:float) (v:float) : int = 
        if isNanOrInf v    then raise <| ArgumentException "FsEx.UtilMath.precisionInt: given input for 'v' is NaN or Infinity."
        if isNanOrInf precision then raise <| ArgumentException "FsEx.UtilMath.precisionInt: given input for 'precision' is NaN or Infinity."
        if precision < 1e-16    then raise <| ArgumentException "FsEx.UtilMath.precisionInt: precision value is negative or almost Zero, less than +1e-16."
        int (v / precision)

    /// Numeric Stepping:Converts floats to ints within defined integer step sizes.
    /// Always rounding down like the int function
    /// = int (v / float precision) * precision
    let inline steppedInt (precision:int) (v:float) : int = 
        if isNanOrInf v    then raise <| ArgumentException "FsEx.UtilMath.steppedInt: given input for 'v' is NaN or Infinity."
        if precision < 1   then raise <| ArgumentException "FsEx.UtilMath.steppedInt: precision value is negative or Zero."
        int (v / float precision) * precision

    /// Numeric Stepping:Converts floats to floats within defined float step sizes.
    /// Always rounding down like the int function)
    /// = float (int (v / precision)) * precision
    let inline steppedFloat (precision:float) (v:float) : float = 
        if isNanOrInf v    then raise <| ArgumentException "FsEx.UtilMath.steppedFloat: given input for 'v' is NaN or Infinity."
        if isNanOrInf precision then raise <| ArgumentException "FsEx.UtilMath.steppedFloat: given input for 'precision' is NaN or Infinity."
        if precision < 1e-16    then raise <| ArgumentException "FsEx.UtilMath.steppedFloat: precision value is negative or almost Zero, less than +1e-16."
        float (int (v / precision)) * precision

    /// Numeric Stepping:Converts floats to floats within defined float step sizes.
    /// Always rounding mid point  like the round function)
    /// =  (round (v / precision)) * precision
    let inline steppedFloatMid (precision:float) (v:float) : float = 
        if isNanOrInf v    then raise <| ArgumentException "FsEx.UtilMath.steppedFloatMid: given input for 'v' is NaN or Infinity."
        if isNanOrInf precision then raise <| ArgumentException "FsEx.UtilMath.steppedFloatMid: given input for 'precision' is NaN or Infinity."
        if precision < 1e-16    then raise <| ArgumentException "FsEx.UtilMath.steppedFloatMid: precision value is negative or almost Zero, less than +1e-16."
        (round (v / precision)) * precision

    /// This float range function ensures that the end is always included.
    /// The F# build in range fails for example on [0.0 .. 0.1 .. 0.2 ] , it equals [0.0 .. 0.1 .. 0.3 ]
    /// It increases the stop value by the smallest step possible 15 times, to ensure end value is included in returned seq.
    let floatRange (start:float<'T>, step:float<'T> , stop:float<'T>) : seq<float<'T>> = 
        if isNanOrInf start then raise <| ArgumentException "FsEx.UtilMath.floatRange: given input for 'start' is NaN or Infinity."
        if isNanOrInf stop then raise <| ArgumentException "FsEx.UtilMath.floatRange: given input for 'stop' is NaN or Infinity."
        if isNanOrInf step then raise <| ArgumentException "FsEx.UtilMath.floatRange: given input for 'step' is NaN or Infinity."
        if step = LanguagePrimitives.FloatWithMeasure<'T> 0.0 then  ArgumentOutOfRangeException.Raise "FsEx.UtilMath.floatRange: step-size cannot be zero: start: %f step: %f stop: %f " start step stop
        let range = stop - start
                    |> float
                    |> BitConverter.DoubleToInt64Bits //https://float.exposed/0x3ff0000000000000
                    |> (+) 15L
                    |> BitConverter.Int64BitsToDouble
                    |> LanguagePrimitives.FloatWithMeasure
        let steps = range/step
        if steps < 0.0 then ArgumentOutOfRangeException.Raise "FsEx.UtilMath.floatRange:stop value cannot be reached: start: %f step: %f stop: %f " start step stop
        let rec frange (start, i, steps) = 
            seq { if i <= steps then
                    yield start + i*step
                    yield! frange (start, (i + 1.0 ), steps) }
        frange (start, 0.0 , steps)

    let internal rand = System.Random()
    /// Given mean and standardDeviation returns a random value from this Gaussian distribution.
    /// If mean is 0 and stDev is 1 then 99% of values are within -2.3 to +2.3 ; 70% within -1 to +1
    let randomStandardDeviation (mean:float<'T> , standardDeviation:float<'U>) : float<'T> = 
        if isNanOrInf mean then raise <| ArgumentException "FsEx.UtilMath.randomStandardDeviation: given input for 'mean' is NaN or Infinity."
        if isNanOrInf standardDeviation then raise <| ArgumentException "FsEx.UtilMath.randomStandardDeviation: given input for 'standardDeviation' is NaN or Infinity."
        let u1 = 1.0 - rand.NextDouble() // so that 0.0  < x <= 1.0 (never 0.0) see https://docs.microsoft.com/en-us/dotnet/api/system.random.nextdouble?view=netframework-4.7.2#remarks
        let u2 = rand.NextDouble()
        let randStdNormal = Math.Sqrt(-2.0 * Math.Log(u1)) * Math.Sin(2.0 * Math.PI * u2) //random normal(0, 1)
        //random normal(mean, stdDev^2)
        mean + (float standardDeviation|> LanguagePrimitives.FloatWithMeasure)* randStdNormal

