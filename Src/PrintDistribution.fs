namespace FsEx

open System
open System.Environment


/// For getting a formated string that has bars to show the ditribution
/// of given float values.
[<RequireQualifiedAccess>]
module PrintDistribution = 
    
    /// The separator of printed values, 
    /// Set this to ',' or ';' if you want a CSV file as output.
    let mutable separator = ""

    let private sprintDistributionMain units scaleBars steps (vs:seq<float>) stepSize stepShift =
        if Seq.isEmpty vs then "No Items, No Data" + NewLine
        else
            let mi = Seq.min vs
            let ma = Seq.max vs 
            let range = ma-mi  
            if range < 0.000001 then sprintf "All %d values in range of %g to %g %sTotal Items:;;;;;;;;%d " (Seq.length vs) mi ma NewLine (Seq.length vs)
            else 
                let av = vs|> Seq.filter (Double.IsNaN>>not) |> Seq.average                
                let mid = mi + range*0.5
                let len = Seq.length vs
                let mutable kk = 0
                let mutable lb = 0.
                let mutable ub = 0.
                let mutable txt =""
                //let nl = Environment.NewLine

                let sprintfunc = // for formating floats
                    if abs ma < 0.05 then                   sprintf "<= %9.8f;%s < ;%9.8f;%s; %5.2f; %%;%7d; items "
                    elif abs mi < 360. && abs ma <360. then sprintf "<= %7.3f;%s < ;%7.3f;%s; %5.2f; %%;%7d; items "            
                    else                                    sprintf "<= %9.0f;%s < ;%9.0f;%s; %5.2f; %%;%7d; items "
            
                let last, aRange, aSteps, aMin, aMax  = // switch between type of call
                    if  steps > 0 && stepSize = 0.0 then // do steps by divison 
                        steps-1 , range, float steps, mi, ma

                    elif steps = 0 && stepSize > 0.0 then // do steps by fixed size
                        let minn = (Math.Floor   ((mi-stepShift)/stepSize))*stepSize + stepShift
                        let maxx = (Math.Ceiling ((ma-stepShift)/stepSize))*stepSize + stepShift
                        let rangee = maxx-minn
                        let stepss = round (rangee/stepSize)
                        let div = int (round (rangee / stepSize) ) - 1
                        div , rangee, stepss, minn, maxx

                    else
                        failwithf "* Bad stepsize %A, steps %A combo input sprintDistributionByStepCount" stepSize steps

                for i = 0 to last do
                    kk <- 0
                    lb <- aMin + aRange*(float i / aSteps)
                    ub <- aMin + aRange*(float (i+1) / aSteps)
                    for v in vs do            
                        if lb <= v && v < ub then kk <- kk+ 1
                        elif i = last then // on last loop check count upper bound elements too
                            if UtilMath.areSameRel 0.0001 v ub then kk <- kk+ 1 // within 0.1% for float errors
            
        
                    let percent = 100.* (float kk)/(float len)

                    txt <- txt + sprintfunc lb units ub units percent kk
                
                    // sprintf bars:
                    if kk > 0 then txt <- txt + sprintf "|"
                    for i = 1 to int (percent*scaleBars) do txt <- txt + sprintf "█"
                    txt <- txt +  NewLine
        
                txt <- txt + sprintf "Total Items:;;;;;;;;%d%s" len NewLine
                txt <- txt + sprintf "Average= %.1f %s MiddleValue = %.1f %s%s"  av units mid units NewLine
                txt <- txt + sprintf "Delta from Average: down =  %.1f %s, up= %.1f %s%s"  (av-aMin) units (aMax-av) units NewLine
                txt <- txt + sprintf "Range = %.1f%s" aRange NewLine
                txt <- txt + sprintf "Minimum = %.1f %s, Maximum = %.1f %s%s%s"  aMin units aMax units NewLine NewLine
                txt.Replace(";",separator)
    

    ///<summary>Returns a formated string that has bars to show the ditribution
    /// of the given float values among a given number of buckets or steps.</summary>
    ///<param name="units">(string) Units to add to text. e.g. "mm"</param>
    ///<param name="scaleBars">(float) To have the bars relativy shorter or longer. ~0.5 to ~2.0 are good scales.</param>  
    ///<param name="steps">(int) Amound of steps or buckets. Something between 5 to 30 makes sense.</param>  
    ///<param name="values">(float seq) The values to analyse.</param> 
    ///<returns>a formated string</returns>
    let bySteps units scaleBars steps values =  
        sprintDistributionMain units scaleBars steps values 0.0 0.0
    
    /// Returns a formted string that has bars to show the ditribution of the given float values among buckets (or steps).
    /// You can specify the range or size 
    /// Parameters:
    ///* units as string ("mm"?) -> scale for Bars ( ~0.5 to ~2.0) -> stepSize (float) -> list of floats -> returns string
    
    
    ///<summary>Returns a formated string that has bars to show the ditribution
    /// of the given float values among buckets or steps of a defdined size.</summary>
    ///<param name="units">(string) Units to add to text. e.g. "mm"</param>
    ///<param name="scaleBars">(float) To have the bars relativy shorter or longer. ~0.5 to ~2.0 are good scales.</param>  
    ///<param name="stepsSize">(int) Size or range of each steps or buckets. The Amount of bukets depends on the input.</param>  
    ///<param name="values">(float seq) The values to analyse.</param> 
    ///<returns>a formated string</returns>
    let byStepSize units scaleBars stepsSize values = 
        sprintDistributionMain units scaleBars 0 values stepsSize 0.0
    
    (*    
    <summary>Returns a formated string that has bars to show the ditribution
     of the given float values among buckets or steps of a defdined size.</summary>
    <param name="units">(string) Units to add to text. e.g. "mm"</param>
    <param name="scaleBars">(float) To have the bars relativy shorter or longer. ~0.5 to ~2.0 are good scales.</param>  
    <param name="stepsSize">(int) Size or range of each steps or buckets. The Amount of bukets depends on the input.</param>
    <param name="offset">(int) Offset or Shift for scale. NOT SURE HOW THIS WAS WORKING ?</param>    
    <param name="values">(float seq) The values to analyse.</param> 
    <returns>a formated string</returns>    
    let byStepSizeOffset units scaleBars stepsSize offset values= 
        sprintDistributionMain units scaleBars 0 values stepsSize offset
        *)
