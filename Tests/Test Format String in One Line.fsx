#r @"D:\Git\FsEx\bin\Release\netstandard2.0\FsEx.dll"

open System
open FsEx

    
do  
       /// join string into one line , clears leading whitespac to maxiumum one
    let stringInOneLine(s:string) =
        if isNull s then  "<*null string*>"
        else
            let sb = Text.StringBuilder(s.Length)
            let rec loop isBeginning i = 
                if i<s.Length then 
                    match s.[i] with 
                    |'\n' ->  
                        if sb.Length>0 && sb.Chars(sb.Length-1) <> ' ' then 
                            sb.Append(' ') |> ignore // to have at least on space separating new lines
                        loop true  (i+1)
                    |'\r' ->  loop true  (i+1)
                    |' '  ->   
                        if not isBeginning then  
                            sb.Append(' ')  |> ignore
                        loop true  (i+1)
                    | c  ->  
                        sb.Append(c) |> ignore
                        loop false (i+1)
            loop true 0
            sb.ToString()
       
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

        """John
John
John
John
John
John
John"""

        ""
        "\n"
        "\r\n"
        "\n\n"
        "\r\n\r\n"
        " \r\n\r\n"
        " "
        "\n "
        ]
    
    for s in strings do  
        s |> stringInOneLine |> print
    
   
    
    