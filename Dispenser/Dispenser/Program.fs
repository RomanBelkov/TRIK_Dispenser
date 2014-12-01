open Trik
open Trik.Collections
open System
open System.Threading

let mask = 0xFF
let mainSpd = 80
let supSpd = 15
let del = 15

type Direction = NE | SE | SW | NW

type Sensor = N | S | W | E

let changer currDir sens = 
    match (currDir, sens) with
        | NE, E | SW, S -> NW
        | NE, N | SW, W -> SE
        | SE, E | NW, N -> SW
        | SE, S | NW, W -> NE
        | d, _ -> d

let exit = new EventWaitHandle(false, EventResetMode.AutoReset)

[<EntryPoint>]
let main _ = 
    use model = new Model()
    use buttons = new ButtonPad()
    use m1 = model.Motor.["M1"]
    use m2 = model.Motor.["M2"]
    use m3 = model.Motor.["M3"]
    use m4 = model.Motor.["M4"]
    use red   = model.Servo.["C1"]
    use green = model.Servo.["C2"]
    use blue  = model.Servo.["C3"]
    use ESensor = model.AnalogSensor.["A6"]
    use SSensor = model.AnalogSensor.["A2"]
    use WSensor = model.AnalogSensor.["A3"]
    use NSensor = model.AnalogSensor.["A5"]
    use VMSensor = model.MXNSensor

    use upButtonDispose = 
        buttons.ToObservable()
        |> Observable.filter (fun x -> ButtonEventCode.Up = x.Button)
        |> Observable.subscribe (fun _ -> exit.Set() |> ignore)

    let colSetter (r, g, b) = 
        let grDel = (r - 0) * (r - 0) + (255 - g) * (255 - g) + (b - 0) * (b - 0)
        let blDel = (r - 0) * (r - 0) + (g - 0) * (g - 0) + (255 - b) * (255 - b)
        let ylDel = (255 - r) * (255 - r) + (255 - g) * (255 - g) + (b - 0) * (b - 0)
        let min = [grDel; blDel; ylDel] |> List.min
        match min with
            | x when x = grDel -> red.SetPower 0; blue.SetPower 0; green.SetPower 100
            | x when x = blDel -> red.SetPower 0; green.SetPower 0; blue.SetPower 100
            | x when x = ylDel -> blue.SetPower 0; green.SetPower 30; red.SetPower 100; 
            | _ -> failwith "no way"

    let colorProcessor (colArr : int[]) =
        colArr.[0] 
        |> (fun x -> (mask &&& (x >>> 16), 
                      mask &&& (x >>> 8),
                      mask  &&& x))  
        |> colSetter

    use time = 
        Observable.Interval (TimeSpan.FromMilliseconds 500.)
        |> Observable.map (fun _ -> VMSensor.Read())
        |> Observable.subscribe colorProcessor

    let (eInit, sInit, wInit, nInit) = 
        (ESensor.Read(), SSensor.Read(), WSensor.Read(), NSensor.Read())
     
    let ERead = 
        ESensor.ToObservable() 
        |> Observable.choose (fun x -> if x > eInit + del || x < eInit - del then Some E else None)
    let SRead = 
        SSensor.ToObservable() 
        |> Observable.choose (fun x -> if x > sInit + del || x < sInit - del then Some S else None)
    let WRead = 
        WSensor.ToObservable() 
        |> Observable.choose (fun x -> if x > wInit + del || x < wInit - del then Some W else None)
    let NRead = 
        NSensor.ToObservable() 
        |> Observable.choose (fun x -> if x > nInit + del || x < nInit - del then Some N else None)

    let setter currDir = 
        match currDir with 
            | NE -> m1.SetPower mainSpd; m3.SetPower mainSpd; m2.SetPower -supSpd; m4.SetPower -supSpd
            | SW -> m1.SetPower -mainSpd; m3.SetPower -mainSpd; m2.SetPower supSpd; m4.SetPower supSpd
            | SE -> m2.SetPower -mainSpd; m4.SetPower -mainSpd; m1.SetPower supSpd; m3.SetPower supSpd
            | NW -> m2.SetPower mainSpd; m4.SetPower mainSpd; m1.SetPower supSpd; m3.SetPower supSpd

    let sensors = Observable.mergeList [ERead; SRead; WRead; NRead]
    use res = sensors 
              |> Observable.scan changer NE
              |> Observable.subscribe (fun d -> m1.Stop(); m3.Stop(); m2.Stop(); m4.Stop(); setter d)

    VMSensor.Start()
    VMSensor.Size <- (1, 1)
    buttons.Start()
    setter NE

    exit.WaitOne() |> ignore
    0 
