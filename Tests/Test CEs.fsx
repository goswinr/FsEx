//#r "nuget: FsEx, 0.9.0"
#r @"C:\GitHub\FsEx\bin\Release\netstandard2.0\FsEx.dll"

open System
open FsEx


type D(t:string) =
    interface IDisposable with
        member this.Dispose() =
            eprintfn $"Disposing {t}"

//let xs = 
//    rarr{
//        for i=2 to 9 do  
//            try 
//                print i
//                if i%2=0 then  
//                    fail() 
//                $"'{i}', "
//            with e ->  
//                 "E"
            
            
//            use st = new D($"{i}") 
//            "S"
//        }
 
let s = stringBuffer{ 
    for i=2 to 9 do  
        try 
            print i
            if i%2=0 then  
                fail() 
            $"'{i}', "
        with e ->  
             yield! "E"
        
        
        use st = new D($"{i}") 
        yield! "S"
        }
 
//printFull xs      
print s