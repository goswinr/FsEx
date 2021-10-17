
# FsEx

[![FsEx on nuget.org](https://img.shields.io/nuget/v/FsEx)](https://www.nuget.org/packages/FsEx/)
[![FsEx on fuget.org](https://www.fuget.org/packages/FsEx/badge.svg)](https://www.fuget.org/packages/FsEx)
[![release](https://img.shields.io/github/release/goswinr/FsEx.svg)](https://github.com/goswinr/FsEx/releases) 
![code size](https://img.shields.io/github/languages/code-size/goswinr/FsEx.svg) 
[![license](https://img.shields.io/github/license/goswinr/FsEx)](LICENSE)

![Logo](https://raw.githubusercontent.com/goswinr/FsEx/main/Doc/logo.png)

FsEx is an FSharp extension and utility library. 


### It Includes: 

- [A thin wrapper over the `ResizeArray` type called `Rarr`](https://www.fuget.org/packages/FsEx/0.7.0/lib/netstandard2.0/FsEx.dll/FsEx/Rarr%601). It provides structural equality ,additional members and more detailed error messages when the List is used with out of bound indices.
- [A `Rarr` module](https://www.fuget.org/packages/FsEx/0.7.0/lib/netstandard2.0/FsEx.dll/FsEx/RarrModule) that provides all the functions for `Rarr` that the  `Array` module from `FSharp.Core` has. Including those for parallel computing.
- A thin wrapper over the generic `Dictionary` type called `Dict`. It provides additional members and more detailed error messages when a given Key is not found.
- An implementation of [defaultdict](https://docs.python.org/3/library/collections.html#collections.defaultdict) called `DefDict` , a dictionary that creates a key when its accessed but missing.
- [A comprehensive module](https://www.fuget.org/packages/FsEx/0.7.0/lib/netstandard2.0/FsEx.dll/FsEx/StringModule) for working with `string`, including nice error messages.
- Useful extension members to existing collections like seq, array, Dictionary, Stringbuilder.
- Computational Expressions for building `Rarr`, `strings`, and CSV-files. Like they exist for `seq`, `list` and `array` in `FSharp.Core` 
- Pretty printing for collections
- and more ..

[See full API documentation on fuget.org](https://www.fuget.org/packages/FsEx)

### License
[MIT](https://raw.githubusercontent.com/goswinr/FsEx/main/LICENSE.txt)

### Changelog
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
    
> 0.4.0
- first release