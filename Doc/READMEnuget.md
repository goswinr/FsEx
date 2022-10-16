

# FsEx

![code size](https://img.shields.io/github/languages/code-size/goswinr/FsEx.svg) 
[![license](https://img.shields.io/github/license/goswinr/FsEx)](LICENSE)

![Logo](https://raw.githubusercontent.com/goswinr/FsEx/main/Doc/logo.png)

FsEx is an FSharp extension and utility library mostly focused on collections and strings. 


### It Includes: 

- [A thin wrapper over the `ResizeArray` type called `Rarr`](https://www.fuget.org/packages/FsEx/0.11.0/lib/netstandard2.0/FsEx.dll/FsEx/Rarr%601). It provides structural equality ,additional members and more detailed error messages when the List is used with out of bound indices.
- [A `Rarr` module](https://www.fuget.org/packages/FsEx/0.11.0/lib/netstandard2.0/FsEx.dll/FsEx/RarrModule) that provides all the functions for `Rarr` that the  `Array` module from `FSharp.Core` has. Including those for parallel computing.
- A thin wrapper over the generic `Dictionary` type called `Dict`. It provides additional members and more detailed error messages when a given Key is not found.
- An implementation of [defaultdict](https://docs.python.org/3/library/collections.html#collections.defaultdict) called `DefDict` , a dictionary that creates a key when its accessed but missing.
- [A comprehensive module](https://www.fuget.org/packages/FsEx/0.11.0/lib/netstandard2.0/FsEx.dll/FsEx/StringModule) for working with `string`, including nice error messages.
- Useful extension members to existing collections like seq, array, Dictionary, StringBuilder.
- Computational Expressions for building `Rarr`, `strings`, and CSV-files. Like they exist for `seq`, `list` and `array` in `FSharp.Core` 
- Pretty printing for collections
- and more ..

[See full API documentation on fuget.org](https://www.fuget.org/packages/FsEx)

### License
[MIT](https://raw.githubusercontent.com/goswinr/FsEx/main/LICENSE.txt)

### Changelog

`0.11.1`
- add Seq.iterIndexed
- add Seq.repeat
- add FloatMathOperators module

`0.11.0`
- improve randomness of random color
- rename Dict and DefaultDict members Add and Set
- add getOrSetDefault to Dict
- add String.addPrefix String.addSuffix
- add Seq.tryFindDuplicate and Seq.duplicates
- rename Dict.setOnce to Dict.setIfKeyAbsent 
- remove cmp
- Array.second, Array.last and similar functions
- better Error messages
- add IO.createDirectoriesOfFilePath
- better toNiceString
- add String.beforeOrInput, .afterOrInput and .betweenOrInput
- remove dependency on System.Drawing
- rename Rarr.find to Rarr.findValue
- add Rarr.getInternalList
- add Rarr.mapFromIList .mapFromSeq and .mapFromArray
- add rarr.InternalList to access internal ResizeArray
- add Rarr.trim
- prev-this-next for Rarr

`0.10.1` 
- add MathUtil.interpolateTable function
- improve NiceString formatting of F# records
- rename stringbuffer to str
- improve move and rename String.Truncate functions to NiceFormat module

`0.9.0` 
- Fix xml doc that was broken in 0.8.0
- Lower minimum FSharp.Core version to  4.5.2
- add tryWith,tryFinally and Using to ComputationalExpressionsBuilders
- fix typos in docstring 

`0.8.0` 
- update Process Module for running external Processes
- improve float formatting in toNiceString module
- add more colors for printing via Seff editor
- fix typos in docstrings

`0.7.0` 
- Add Process Module for running external Processes
- Fix Rarr.rotate
- Add Rarr.isNotEmpty
- Add IO.getAllFilesInParentsByPattern
- Typo in Timer μs is actually  ms
- Add String.truncate, skip and take methods

`0.5.1` 
- Updated `Rarr` and `RarrModule`
- Updated `StringModule`
- Improved Exception messages.
- nicer pretty printing via `print` function
    
`0.4.0` 
- first public release