namespace FsEx              

open System

[<AutoOpen>]
module EquatableModule = 
    // https://github.com/Microsoft/visualfsharp/issues/526#issuecomment-119693551
    // https://zeckul.wordpress.com/2015/07/09/how-to-avoid-boxing-value-types-in-f-equality-comparisons/
    
    /// high performance comparison for non core Struct types
    let inline private eq<'a when 'a :> System.IEquatable<'a>> (x:'a) (y:'a) = x.Equals y

    /// high performance comparison for non core Struct types
    let inline (==) x y = eq x y

    /// high performance comparison for non core Struct types
    let inline (!=) x y = not (eq x y)


module StructTuples =    
                  
    /// Use '==' operater for fast equality (EquatableModule)
    [<Struct; CustomEquality; NoComparison>]
    type  IntTup =      
        val a :int
        val b :int            
        new (a, b) = {a=a; b=b}
        new (x, y, precMult:float) =                   {a = int (x * precMult);          b = int (y * precMult) }
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
            (t.a <<< 16) ^^^ t.b // very god hash for ints up to 65536

        override t.Equals x = 
            // http://msdn.microsoft.com/en-us/library/336aedhh%28v=VS.71%29.aspx
            // https://zeckul.wordpress.com/2015/07/09/how-to-avoid-boxing-value-types-in-f-equality-comparisons/
            match x with
            | :? IntTup as tt -> t.a = tt.a && t.b = tt.b
            | _ -> false 
                       
        interface IEquatable<IntTup> with // see http://www.fssnip.net/7a  
            // http://stackoverflow.com/questions/1502451/what-needs-to-be-overriden-in-a-struct-to-ensure-equality-operates-properly
            member t.Equals(tt) = t.a = tt.a && t.b = tt.b
        
        override t.ToString() = sprintf "IntTup(a = %d, b = %d)" t.a t.b 
       
    /// Use '==' operater for fast equality (EquatableModule)
    type [<Struct; CustomEquality; NoComparison>] IntTrip =        
        val a :int
        val b :int
        val c :int                    
        new (a,b,c) = {a=a; b=b; c=c}
        new (x,  y,z, precMult:float) =  {a = int (x * precMult);         
                                          b = int (y * precMult);
                                          c = int (z * precMult)}
        new (x, y, z, precMult:float, shiftX, shiftY, shiftZ) = {a = int (x * precMult + shiftX); 
                                                                 b = int (y * precMult + shiftY); 
                                                                 c = int (z * precMult + shiftZ)}
        
       
        override t.GetHashCode() =      //http://stackoverflow.com/questions/1835976/what-is-a-sensible-prime-for-hashcode-calculation
            //let  prime = 92821          // http://stackoverflow.com/questions/892618/create-a-hashcode-of-two-numbers
            //prime * (prime * ( prime + t.a ) + t.b)  + t.c
            (t.a <<< 22) ^^^ (t.b <<< 11) ^^^ t.c // very god hash for ints up to 2048

        override t.Equals x = // http://msdn.microsoft.com/en-us/library/336aedhh%28v=VS.71%29.aspx
            match x with
            | :? IntTrip as tt -> t.a = tt.a && t.b = tt.b && t.c = tt.c
            | _ -> false 
                       
        interface IEquatable<IntTrip> with // see http://www.fssnip.net/7a  
            // http://stackoverflow.com/questions/1502451/what-needs-to-be-overriden-in-a-struct-to-ensure-equality-operates-properly
            // https://zeckul.wordpress.com/2015/07/09/how-to-avoid-boxing-value-types-in-f-equality-comparisons/
            member t.Equals(tt) = t.a = tt.a && t.b = tt.b && t.c = tt.c                                                                                                               
        
        override this.ToString() = sprintf "IntTrip(a = %d, b = %d, c = %d)" this.a this.b this.c

   