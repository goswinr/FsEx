namespace FsEx

open System

#nowarn "86" // to allow redefinging (=) and (<>) operators in module FastEqualOperators = 


//What if the type must be used as a key in e.g. a dictionary?
//Well if it's System.Collections.Generics.Dictionary you're fine, that doesn't use the F# equality operator.
//But any collection defined in F# that uses this operator will run into boxing issues apparently.
//https://stackoverflow.com/questions/28142655/iequatable-in-f-operator-performance-and-structural-equality/31283508#31283508

/// Opening this module improves the performance of equality tests (=) and (<>) on custom structs significantly.
/// But for types that don't have System.IEquatable<'T> implemented you will now have to use (==) and (!=).
/// Use #nowarn "86" to disable warnings
module  NonBoxingEqualityOperatorOverloads = 
    //https://github.com/dotnet/fsharp/issues/526#issuecomment-119755563
    //https://zeckul.wordpress.com/2015/07/09/how-to-avoid-boxing-value-types-in-f-equality-comparisons/
    let inline private eq<'T when 'T :> System.IEquatable<'T>> (x:'T) (y:'T) = x.Equals y

    /// The Equals operator (=) is overloaded to always use the generic System.IEquatable<'T>
    /// This avoids boxing of custom structs when comparing. A significant performance improvement.
    /// Use (==) for types that don't have System.IEquatable<'T> implemented.
    let inline (=) x y = eq x y

    /// The NotEquals operator (<>) is overloaded to always use the generic System.IEquatable<'T>
    /// This avoids boxing of custom structs when comparing. A significant performance improvement.
    /// Use (!=) for types that don't have System.IEquatable<'T> implemented.
    let inline (<>) x y = not (eq x y)

    /// The original Equals operator (=) is overloaded to always use the generic System.IEquatable<'T>
    /// This avoids boxing of custom structs when comparing. A significant performance improvement.
    /// Use this operator (==) for types that don't have System.IEquatable<'T> implemented or any type if you don't mind performance.
    let inline (==) x y = Microsoft.FSharp.Core.Operators.(=) x y

    /// The original NotEquals operator (<>) is overloaded to always use the generic System.IEquatable<'T>
    /// This avoids boxing of custom structs when comparing. A significant performance improvement.
    /// Use (!=) for types that don't have System.IEquatable<'T> implemented or any type if you don't mind performance.
    let inline (!=) x y = Microsoft.FSharp.Core.Operators.(<>) x y

[<AutoOpen>]
module NonBoxingEqualityOperators = 
    //https://github.com/dotnet/fsharp/issues/526#issuecomment-119755563
    //https://zeckul.wordpress.com/2015/07/09/how-to-avoid-boxing-value-types-in-f-equality-comparisons/
    let inline private eq<'T when 'T :> System.IEquatable<'T>> (x:'T) (y:'T) = x.Equals y

    /// This Equals operator (===) always uses the generic System.IEquatable<'T>
    /// This avoids boxing of custom structs when comparing with (=). A significant performance improvement.
    let inline (===) x y = eq x y

    /// This NotEquals operator (=!=) is overloaded to always use the generic System.IEquatable<'T>
    /// This avoids boxing of custom structs when comparing with (<>). A significant performance improvement.
    let inline (=!=) x y = not (eq x y)

open System.Runtime.CompilerServices // for ; IsByRefLike; IsReadOnly

/// A struct of two integers
/// use operator (===) for fast equality test
/// Avoid using this struct in Fsharp.Core functions like: List.contains value. boxing will occur
/// Better: List.exists (fun v -> v === value)
[<Struct; CustomEquality; NoComparison>]
//[<IsByRefLike; IsReadOnly>] not available in netstandart https://bartoszsypytkowski.com/writing-high-performance-f-code/
type  IntTup = 
    val a :int
    val b :int
    new (a, b) = {a=a; b=b}

    /// multiplies x and y with precMult before casting to two integers
    new (x, y, precMult:float) =                   {a = int (x * precMult);          b = int (y * precMult) }

    /// multiplies x and y with precMult  then adds shiftX and shiftY respectively before casting to two integers
    new (x, y, precMult:float, shiftX, shiftY) =   {a = int (x * precMult + shiftX); b = int (y * precMult + shiftY)}

    override t.GetHashCode() = 
        // http://stackoverflow.com/questions/1835976/what-is-a-sensible-prime-for-hashcode-calculation
        // http://stackoverflow.com/questions/263400/what-is-the-best-algorithm-for-an-overridden-system-object-gethashcode
        //let mutable hash =  2166136261
        //hash <- hash * 16777619 ^ t.a
        //hash <- hash * 16777619 ^ t.b
        //hash

        //let  prime = 92821
        //prime * ( prime + t.a ) + t.b

        //(t.a <<< 16) ^^^ t.a ^^^ (t.b >>> 16) // from comments in answer https://stackoverflow.com/questions/46142734/why-is-hashsetpoint-so-much-slower-than-hashsetstring
        (t.a <<< 16) ^^^ t.b // very good hash for ints up to 65536

    override t.Equals x = 
        // http://msdn.microsoft.com/en-us/library/336aedhh%28v=VS.71%29.aspx
        // https://zeckul.wordpress.com/2015/07/09/how-to-avoid-boxing-value-types-in-f-equality-comparisons/
        match x with
        | :? IntTup as tt -> t.a = tt.a && t.b = tt.b
        | _ -> false

    interface IEquatable<IntTup> with // see http://www.fssnip.net/7a
        // http://stackoverflow.com/questions/1502451/what-needs-to-be-overriden-in-a-struct-to-ensure-equality-operates-properly
        member t.Equals(tt) = t.a = tt.a && t.b = tt.b

    static member inline op_Equality(this : IntTup, other : IntTup) = // so that "open NonStructuralComparison" can be used too
        this.a = other.a && this.b = other.b

    override t.ToString() = 
        sprintf "IntTup(a = %d, b = %d)" t.a t.b

/// A struct of three integers
/// use operator (===) for fast equality test
/// Avoid using this struct in Fsharp.Core functions like: List.contains value. boxing will occur
/// Better: List.exists ( (===) value)
//[<IsByRefLike; IsReadOnly>] not available in netstandart https://bartoszsypytkowski.com/writing-high-performance-f-code/
[<Struct; CustomEquality; NoComparison>] 
type IntTrip = 
    val a :int
    val b :int
    val c :int
    new (a,b,c) = {a=a; b=b; c=c}

    /// multiplies x, y and z with precMult before casting to three integers
    new (x,  y,z, precMult:float) =  {a = int (x * precMult);
                                        b = int (y * precMult);
                                        c = int (z * precMult)}

    /// multiplies x, y and z with precMult  then adds shiftX, shiftY and shiftZ respectively before casting to three integers
    new (x, y, z, precMult:float, shiftX, shiftY, shiftZ) = 
                        {a = int (x * precMult + shiftX);
                        b = int (y * precMult + shiftY);
                        c = int (z * precMult + shiftZ)}

    override t.GetHashCode() = 
        //let  prime = 92821
        //prime * (prime * ( prime + t.a ) + t.b)  + t.c
        (t.a <<< 22) ^^^ (t.b <<< 11) ^^^ t.c // very good hash for ints up to 2048

    override t.Equals x = 
        // http://msdn.microsoft.com/en-us/library/336aedhh%28v=VS.71%29.aspx
        match x with
        | :? IntTrip as tt -> t.a = tt.a && t.b = tt.b && t.c = tt.c
        | _ -> false

    interface IEquatable<IntTrip> with
        // see http://www.fssnip.net/7a
        // http://stackoverflow.com/questions/1502451/what-needs-to-be-overriden-in-a-struct-to-ensure-equality-operates-properly
        // https://zeckul.wordpress.com/2015/07/09/how-to-avoid-boxing-value-types-in-f-equality-comparisons/
        member t.Equals(tt) = t.a = tt.a && t.b = tt.b && t.c = tt.c

    static member inline op_Equality(this : IntTrip, other : IntTrip) = // so that "open NonStructuralComparison" can be used
        this.a = other.a && this.b = other.b && this.c = other.c


    override this.ToString() = 
        sprintf "IntTrip(a = %d, b = %d, c = %d)" this.a this.b this.c

