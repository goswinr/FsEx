
<!-- in VS Code press Ctrl + Shift + V to see a preview-->

# FsEx

[![FsEx on nuget.org](https://img.shields.io/nuget/v/FsEx)](https://www.nuget.org/packages/FsEx/)
[![FsEx on fuget.org](https://www.fuget.org/packages/FsEx/badge.svg)](https://www.fuget.org/packages/FsEx)

FsEx is an FSharp extension and utility library. 

![Logo](Doc/logo.png "Logo")


#### It Includes 

- A thin wrapper over the `ResizeArray` type called `Rarr`. It provides additional members and more detailed error messages when the List is used with out of bound indices.
- A `Rarr` module that provides similar functions like the `Array` module from `FSharp.Core`
- A thin wrapper over the generic `Dictionary` type called `Dict`. It provides additional members and more detailed error messages when a given Key is not found.
- An implementation of [defaultdict](https://docs.python.org/3/library/collections.html#collections.defaultdict) called `DefDict`, a dictionary that creates a key when its accessed but missing.
- Useful extension members to existing collections like seq, array, dictionary.
- Computaional Expressions vor building `Rarr`, `strings`, and CSV fles. Like they exist for `seq`, `list` and `array` in `FSharp.Core` 
- pretty printing for collections
- and more ..
