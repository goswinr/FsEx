#r @"D:\Git\FsEx\bin\Release\netstandard2.0\FsEx.dll"

open System
open FsEx

    
do 
    /// Trims strings to maximum line count.
    /// Adds note about trimmed line count if there are more [ ... and %d more lines.]
    /// Does not include surrounding quotes
    /// If string is null returns "<*null string*>"
    let stringTruncatedToMaxLines (maxLineCount:int) (s:string) :string = 
        //if maxLineCount < 1 then FsExStringException.Raise  "maxLineCount:%d cant be negative or null but is %d" maxLineCount
        if isNull s then  
            "<*null string*>"
        elif s.Length < 2 then  
            s
        else
            let mutable found = if s.[0]= '\n' then 1 else 0
            let mutable i = 0
            let mutable stopPos = 0
            while i >= 0 do
                if i+1=s.Length then // end of string reached with a '\n'
                    i<- -1
                else
                    i <- s.IndexOf('\n', i+1)
                    found <- found + 1
                    if found = maxLineCount then 
                        stopPos <- i

            if stopPos > 0  && found - maxLineCount > 1 then // if there is just one more lien print it instead of the note
                str{ 
                    s.Substring(0,stopPos+1)
                    "[< ... and "
                    found - maxLineCount
                    " more lines.>]"
                    }
            else
                s   
       
    let strings =  
        [ 
            
        """{
  "firstName": "John",
  "lastName": "Smith",
  "isAlive": true,
  "age": 27,
  "address": {
    "streetAddress": "21 2nd Street",
    "city": "New York",
    "state": "NY",
    "postalCode": "10021-3100"
  },
  "phoneNumbers": [
    {
      "type": "home",
      "number": "212 555-1234"
    },
    {
      "type": "office",
      "number": "646 555-4567"
    }
  ],
  "children": [],
  "spouse": null
}"""

        """John1
John2
John3
John4
John5
John6
John7"""

        ""
        "\n"
        "\r\n"
        "\n\n"
        "\r\n\r\n"
        " \r\n\r\n"
        " "
        "\n "
        ]
    
    clearSeffLog() 
    for k=1 to 8 do 
        Printfn.red $"line count{k}"
        for s in strings do  
            s |> stringTruncatedToMaxLines k |> print
    
    