namespace FsEx

open System

module PrintDistribution = 

    let private sprintDistributionMain units scaleBars steps (vs:seq<float>) stepSize stepShift =
        if Seq.isEmpty vs then "No Items, No Data\r\n"
        else
            let mi = Seq.min vs
            let ma = Seq.max vs 
            let range = ma-mi  
            if range < 0.000001 then sprintf "All %d values in range of %g to %g \r\nTotal Items:;;;;;;;;%d " (Seq.length vs) mi ma (Seq.length vs)
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
                    if abs ma < 0.05 then                   sprintf "<= %9.8f;%s < ;%9.8f;%s; %5.2f;%%;%7d;items "
                    elif abs mi < 360. && abs ma <360. then sprintf "<= %7.3f;%s < ;%7.3f;%s; %5.2f;%%;%7d;items "            
                    else                                    sprintf "<= %9.0f;%s < ;%9.0f;%s; %5.2f;%%;%7d;items "
            
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
                    for i = 1 to int (percent*scaleBars) do txt <- txt + sprintf "#" // "█"
                    txt <- txt + sprintf "\r\n"
        
                txt <- txt + sprintf "Total Items:;;;;;;;;%d \r\n" len
                txt <- txt + sprintf "Average= %.1f %s MiddleValue = %.1f %s \r\n"  av units mid units
                txt <- txt + sprintf "Delta from Average: down =  %.1f %s, up= %.1f %s, \r\n"  (av-aMin) units (aMax-av) units
                txt <- txt + sprintf "Range = %.1f \r\n" aRange
                txt <- txt + sprintf "Minimum = %.1f %s, Maximum = %.1f %s \r\n\r\n"  aMin units aMax units
                txt

    ///* units as string ("mm"?) -> scale for Bars ( ~0.5 to ~2.0) -> substeps (int 5 to 30) -> list of floats -> returns string
    let sprintDistribution          units scaleBars steps     (vs:seq<float>) =  sprintDistributionMain units scaleBars steps (vs:seq<float>) 0.0 0.0

    ///* units as string ("mm"?) -> scale for Bars ( ~0.5 to ~2.0) -> stepSize (float) -> list of floats -> returns string
    let sprintDistributionByStepSize units scaleBars stepsSize (vs:seq<float>) = sprintDistributionMain units scaleBars 0 (vs:seq<float>) stepsSize 0.0

    ///* units as string ("mm"?) -> scale for Bars ( ~0.5 to ~2.0) -> stepSize (float) -> offset for scale (float) ->list of floats -> returns string
    let sprintDistributionByStepSizeOffset units scaleBars stepsSize offset (vs:seq<float>) = sprintDistributionMain units scaleBars 0 (vs:seq<float>) stepsSize offset
