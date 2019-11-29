﻿namespace FsEx

open System
open System.Globalization


module  Util = 
    
    let inline notNull (value : 'T) = match value with | null -> false   | _ -> true// Fsharp core does it like this too. dont use RefrenceEquals
    
    ///Returns the value on the left unless it is null, then it returns the value on the right.
    let inline (|?) a b = if Object.ReferenceEquals(a, null) then b else a // a more fancy version: https://gist.github.com/jbtule/8477768#file-nullcoalesce-fs

    ///apply function ( like |> ) but ignore result. return original input
    let inline (|>>) a f =  f a |> ignore ; a

    let fail() = failwith "Generic fail (inner exception should show more helpful message)"   
 
    ///Get first element of triple (tuple of three element)
    let inline t1 (a, _,_) = a
    ///Get second element of triple (tuple of three element)
    let inline t2 (_, b, _) = b
    ///Get third element of triple (tuple of three element)
    let inline t3 (_,_, c) = c    
     


      
 module DynamicOperator =
    // taken from http://www.fssnip.net/2V/title/Dynamic-operator-using-Reflection
    open System.Reflection
    open Microsoft.FSharp.Reflection

    // Various flags that specify what members can be called 
    // NOTE: Remove 'BindingFlags.NonPublic' if you want a version that can call only public methods of classes
    let private staticFlags   = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static 
    let private instanceFlags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance
    let private ctorFlags = instanceFlags
    let inline private asMethodBase(a:#MethodBase) = a :> MethodBase

    // The operator takes just instance and a name. Depending on how it is used
    // it either calls method (when 'R is function) or accesses a property
    let (?) (o:obj) name : 'R =
      // The return type is a function, which means that we want to invoke a method
      if FSharpType.IsFunction(typeof<'R>) then

        // Get arguments (from a tuple) and their types
        let argType, resType = FSharpType.GetFunctionElements(typeof<'R>)
        // Construct an F# function as the result (and cast it to the
        // expected function type specified by 'R)
        FSharpValue.MakeFunction(typeof<'R>, fun args ->
      
          // We treat elements of a tuple passed as argument as a list of arguments
          // When the 'o' object is 'System.Type', we call static methods
          let methods, instance, args = 
            let args = 
              // If argument is unit, we treat it as no arguments,
              // if it is not a tuple, we create singleton array,
              // otherwise we get all elements of the tuple
              if argType = typeof<unit> then [| |]
              elif not(FSharpType.IsTuple(argType)) then [| args |]
              else FSharpValue.GetTupleFields(args)

            // Static member call (on value of type System.Type)?
            if (typeof<System.Type>).IsAssignableFrom(o.GetType()) then 
              let methods = (unbox<Type> o).GetMethods(staticFlags) |> Array.map asMethodBase
              let ctors = (unbox<Type> o).GetConstructors(ctorFlags) |> Array.map asMethodBase
              Array.concat [ methods; ctors ], null, args
            else 
              o.GetType().GetMethods(instanceFlags) |> Array.map asMethodBase, o, args
        
          // A simple overload resolution based on the name and the number of parameters only
          // TODO: This doesn't correctly handle multiple overloads with same parameter count
          let methods = 
            [ for m in methods do
                if m.Name = name && m.GetParameters().Length = args.Length then yield m ]
        
          // If we find suitable method or constructor to call, do it!
          match methods with 
          | [] -> failwithf "No method '%s' with %d arguments found" name args.Length
          | _::_::_ -> failwithf "Multiple methods '%s' with %d arguments found" name args.Length
          | [:? ConstructorInfo as c] -> c.Invoke(args)
          | [ m ] -> m.Invoke(instance, args) ) |> unbox<'R>

      else
        // The result type is not an F# function, so we're getting a property
        // When the 'o' object is 'System.Type', we access static properties
        let typ, flags, instance = 
          if (typeof<System.Type>).IsAssignableFrom(o.GetType()) 
            then unbox o, staticFlags, null
            else o.GetType(), instanceFlags, o
      
        // Find a property that we can call and get the value
        let prop = typ.GetProperty(name, flags)
        if prop |> isNull && instance |> isNull then 
          // The syntax can be also used to access nested types of a type
          let nested = typ.Assembly.GetType(typ.FullName + "+" + name)
          // Return nested type if we found one
          if nested |> isNull then 
            failwithf "Property or nested type '%s' not found in '%s'." name typ.Name 
          elif not ((typeof<'R>).IsAssignableFrom(typeof<System.Type>)) then
            let rname = (typeof<'R>.Name)
            failwithf "Cannot return nested type '%s' as a type '%s'." nested.Name rname
          else nested |> box |> unbox<'R>
        else
          // Call property and return result if we found some
          let meth = prop.GetGetMethod(true)
          if prop |> isNull then failwithf "Property '%s' found, but doesn't have 'get' method." name
          try meth.Invoke(instance, [| |]) |> unbox<'R>
          with _ -> failwithf "Failed to get value of '%s' property (of type '%s')" name typ.Name