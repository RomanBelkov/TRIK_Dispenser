open Trik
open Trik.Collections
open System
open System.Threading

let mask = 0xFF
let mainSpd = 60
let supSpd = 50
let tol = 40

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
    use NMotor = model.Motor.["M2"]
    use WMotor = model.Motor.["M1"]
    use EMotor = model.Motor.["M3"]
    use SMotor = model.Motor.["M4"]
    use red   = model.Servo.["C1"]
    use green = model.Servo.["C2"]
    use blue  = model.Servo.["C3"]
    use ESensor = model.AnalogSensor.["A6"]
    use WSensor = model.AnalogSensor.["A2"]
    use SSensor = model.AnalogSensor.["A4"]
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
            | x when x = ylDel -> blue.SetPower 0; green.SetPower 30; red.SetPower 100
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

    let (EInit, SInit, WInit, NInit) = 
        (ESensor.Read(), SSensor.Read(), WSensor.Read(), NSensor.Read())

    let ERead = 
        ESensor.ToObservable() 
        |> Observable.choose (fun x -> if abs (x - EInit) > tol then Some E else None)
    let SRead = 
        SSensor.ToObservable() 
        |> Observable.choose (fun x -> if abs (x - SInit) > tol then Some S else None)
    let WRead = 
        WSensor.ToObservable() 
        |> Observable.choose (fun x -> if abs (x - WInit) > tol then Some W else None)
    let NRead = 
        NSensor.ToObservable() 
        |> Observable.choose (fun x -> if abs (x - NInit) > tol then Some N else None)

    let setter currDir = 
        match currDir with 
            | NE -> NMotor.SetPower mainSpd; SMotor.SetPower mainSpd; WMotor.SetPower supSpd; EMotor.SetPower supSpd
            | SW -> NMotor.SetPower -supSpd; SMotor.SetPower -supSpd; WMotor.SetPower -mainSpd; EMotor.SetPower -mainSpd
            | SE -> NMotor.SetPower mainSpd; SMotor.SetPower mainSpd; WMotor.SetPower -supSpd; EMotor.SetPower -supSpd
            | NW -> NMotor.SetPower -supSpd; SMotor.SetPower -supSpd; WMotor.SetPower mainSpd; EMotor.SetPower mainSpd

    let sensors = Observable.mergeList [ERead; SRead; WRead; NRead]
    use res = sensors 
              |> Observable.scan changer NE
              |> Observable.subscribe (fun d -> NMotor.Stop(); SMotor.Stop(); WMotor.Stop(); EMotor.Stop(); setter d)

    VMSensor.Start()
    VMSensor.Size <- (1, 1)
    buttons.Start()
    setter NE

    exit.WaitOne() |> ignore
    0 
