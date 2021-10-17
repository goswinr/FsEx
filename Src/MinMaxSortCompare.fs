namespace FsEx

/// Small functions for sorting and finding minimum or maximum values
/// of tuple ,triples and quadruples
module MinMaxSort = 

    // make all inline because of https://stackoverflow.com/questions/6104221/why-is-this-f-code-so-slow


    /// Returns the smallest element.
    /// Elements are compared by applying the predicate function first.
    /// If both are equal then the first element is returned.
    let inline minBy f a b =  if f a > f b then b else a

    /// Returns the biggest element.
    /// Elements are compared by applying the predicate function first.
    /// If both are equal then the first element is returned.
    let inline maxBy f a b =  if f a < f b then b else a

    /// Returns the smallest of three elements.
    let inline min3 (a, b, c) = min a b |> min c

    /// Returns the biggest of three elements.
    let inline max3 (a, b, c) = max a b |> max c

    /// Returns the smallest of four elements.
    let inline min4 (a, b, c, d) = min a b |> min c |> min d

    /// Returns the biggest of four elements.
    let inline max4 (a, b, c, d) = max a b |> max c |> max d

    /// Sort two elements.
    /// If they are equal then the the order is kept
    let inline sort2 (a, b)  = if a <= b  then a, b else b, a

    /// Sort two elements.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the the order is kept
    let inline sort2By f (a, b) = if f a <= f b  then a, b else b, a

    /// Sort three elements.
    /// If any are equal then the the order is kept
    let inline sort3 (a, b, c) = 
        if a <= b then
            if b <= c then      a, b, c
            else // c<b
                if a <= c then  a, c, b
                else            c, a, b
        else // b<a
            if a <= c then      b, a, c
            else //c<a
                if b <= c then  b, c, a
                else            c, b, a

    /// Sort three elements.
    /// Elements are compared by applying the predicate function first.
    /// If any are equal after function is applied then the the order is kept
    let inline sort3By f (aa, bb, cc) = 
        let a = f aa
        let b = f bb
        let c = f cc
        if a <= b then
            if b <= c then      aa, bb, cc
            else // c<b
                if a <= c then  aa, cc, bb
                else            cc, aa, bb
        else // b<a
            if a <= c then      bb, aa, cc
            else //c<a
                if b <= c then  bb, cc, aa
                else            cc, bb, aa

    /// Compare two elements. Returns -1, 0 or 1
    /// if   a= b then  0
    /// elif a<b  then -1
    /// else            1
    let inline cmp a b = 
        if   a= b then  0
        elif a<b  then -1
        else            1

    /// Gets the positiv differnce between 2 numbers.
    /// Avoids the integer( or byte) overflow and underflow risk of "abs(a-b)"
    let inline diff a b = 
        if   a<b then b-a
        else          a-b


/// Operators for chaining compare operations like: <c> 1 <. x .< 9 </c>
/// or <c> 0 <. x .<. y .< 99 </c>
/// Each chained comparison operator will be combined with the previous result via AND (&&) logic
module CompareOperators = 

    /// Point must be at middle of expression: like this: min <=. x .<= max
    let inline (<=.) left middle = (left <= middle, middle)

    /// Point must be at middle of expression: like this: min <. x .< max
    let inline (<.) left middle = (left < middle, middle)

    /// Point must be at middle of expression: like this: min <. x .< max
    let inline (.<) (leftResult, middle) right = leftResult && (middle < right)

    /// Point must be at middle of expression: like this: min <=. x .<= max
    let inline (.<=) (leftResult, middle) right = leftResult && (middle <= right)

    /// For inner expressions: like this: min <. x .<. y .< max
    let inline (.<.) (leftResult, middle) right = leftResult && (middle < right), right

    /// for inner expressions: like this: min <. x .<. y .< max
    let inline (.<=.) (leftResult, middle) right = leftResult && (middle <= right), right


    (*
    this reversed order does not realy make sense since the combinig logic is always AND (&&)

    let x = 2
    10 >. x .> 20  // to test if x is outside of range would return false, but true could be expected (OR logic)

    /// Point must be at middle of expression: like this: min <. x .< max
    let inline (>.) left middle = (left > middle, middle)

    /// Point must be at middle of expression: like this: min <=. x .<= max
    let inline (>=.) left middle = (left >= middle, middle)

    /// Point must be at middle of expression: like this: min <. x .< max
    let inline (.>) (leftResult, middle) right = leftResult && (middle > right)

    /// Point must be at middle of expression: like this: min <=. x .<= max
    let inline (.>=) (leftResult, middle) right = leftResult && (middle >= right)

    /// for inner expressions: like this: min <. x .<. y .< max
    let inline (.>=.) (leftResult, middle) right = leftResult && (middle >= right), right

    /// For inner expressions: like this: min <. x .<. y .< max
    let inline (.>.) (leftResult, middle) right = leftResult && (middle > right), right
    *)

