namespace FsEx

module MinMaxSort = 
    
    ///If both are equal then the first is returned
    let inline minBy f a b =  if f a > f b then b else a

    ///If both are equal then the first is returned
    let inline maxBy f a b =  if f a < f b then b else a

    let inline min3 (a, b, c) = min a b |> min c

    let inline max3 (a, b, c) = max a b |> max c

    let inline min4 (a, b, c, d) = min a b |> min c |> min d  
    
    let inline max4 (a, b, c, d) = max a b |> max c |> max d

    ///If they are equal then the the order is kept
    let inline sort2 (a, b)  = if a <= b  then a, b else b, a

    ///If they are equal then the the order is kept
    let inline sort2By f (a, b) = if f a <= f b  then a, b else b, a
        
    ///If any are equal then the the order is kept
    let inline sort3 (a, b, c) = 
        if a<=b then 
            if b<=c then a, b, c
            else // c<b
                if a <= c then a, c, b
                else           c, a, b
        else // b<a
            if a<=c then b, a, c
            else //c<a
                if b<=c then b, c, a 
                else         c, b, a
        
    ///If any are equal after Function is applied then the the order is kept
    let inline sort3By f (a, b, c) = 
        if f a <= f b then 
            if f b <= f c then a, b, c
            else // c<b
                if f a <= f c then a, c, b
                else               c, a, b
        else // b<a
            if f a <= f c then b, a, c
            else //c<a
                if f b <= f c then b, c, a 
                else               c, b, a

    let inline cmp a b =
        if   a= b then  0
        elif a<b then -1
        else           1 
        
    ///Gets the positiv differnce between 2 numbers. 
    ///Avoids the integer( or byte) overflow and underflow risk of "abs(a-b)"
    let inline diff a b =
        if   a<b then b-a
        else          a-b 


///Operators for chaining compare operations like: <c> 1 <. x .< 9 </c>
///or <c> 0 <. x .<. y .< 99 </c>
module CompareOperators = 

    ///Point must be at middle of expression: like this: min <=. x .<= max
    let inline (<=.) left middle = (left <= middle, middle)

    ///Point must be at middle of expression: like this: min <=. x .<= max
    let inline (>=.) left middle = (left >= middle, middle)

    ///Point must be at middle of expression: like this: min <=. x .<= max
    let inline (.<=) (leftResult, middle) right = leftResult && (middle <= right)

    ///Point must be at middle of expression: like this: min <=. x .<= max
    let inline (.>=) (leftResult, middle) right = leftResult && (middle >= right)

    ///for inner expressions: like this: min <. x .<. y .< max
    let inline (.<=.) (leftResult, middle) right = leftResult && (middle <= right), right

    ///for inner expressions: like this: min <. x .<. y .< max
    let inline (.>=.) (leftResult, middle) right = leftResult && (middle >= right), right

    ///Point must be at middle of expression: like this: min <. x .< max
    let inline (<.) left middle = (left < middle, middle)

    ///Point must be at middle of expression: like this: min <. x .< max
    let inline (>.) left middle = (left > middle, middle)

    ///Point must be at middle of expression: like this: min <. x .< max
    let inline (.<) (leftResult, middle) right = leftResult && (middle < right)

    ///Point must be at middle of expression: like this: min <. x .< max
    let inline (.>) (leftResult, middle) right = leftResult && (middle > right)

    ///For inner expressions: like this: min <. x .<. y .< max
    let inline (.<.) (leftResult, middle) right = leftResult && (middle < right), right

    ///For inner expressions: like this: min <. x .<. y .< max
    let inline (.>.) (leftResult, middle) right = leftResult && (middle > right), right
    
