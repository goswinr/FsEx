namespace FsEx

open System.Collections.Generic


/// A type abbreviation for System.Collections.Generic.HashSet<'T>
/// so that it is available in FsEx namespace.
/// ( with a lowercase 's')
type Hashset<'T> = HashSet<'T>

/// AutoOpen Extensions for HashSet.
/// Provides DoesNotContain and similar methods.
[<AutoOpen>]
module AutoOpenExtensionsHashset = 

    // This type extension should be alway availabe that s why it is in this Auitoopen module
    type HashSet<'T> with

        /// Returns: not (set.Contains(key))
        member inline h.DoesNotContain(key:'T) = 
            not (h.Contains(key))

        /// Like this.Add(key), but returning unit.
        /// h.Add(key)|> ignore
        member inline h.add(key:'T) = 
            h.Add(key)|> ignore

        /// Like this.Remove(key), but returning unit.
        /// h.Remove(key)|> ignore
        member inline h.remove(key:'T) = 
            h.Remove(key)|> ignore
