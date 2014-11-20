open Trik
open Trik.Collections
open System
open System.Threading

//Cardinal direction
type Direction = NE | SE | SW | NW

type Sensor = N | S | W | E

[<AbstractClass; Sealed>]
    type Observable =
     static member mergeList xs = List.reduce Observable.merge xs

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
    let model = new Model()
    let buttons = new ButtonPad()
    let m1 = model.Motor.["M1"]
    let m2 = model.Motor.["M2"]
    let m3 = model.Motor.["M4"]
    let m4 = model.Motor.["M3"]
    let ESensor = model.AnalogSensor.["A6"]
    let SSensor = model.AnalogSensor.["A2"]
    let WSensor = model.AnalogSensor.["A3"]
    let NSensor = model.AnalogSensor.["A5"]

    let initRead = 
        ((ESensor.Read() + SSensor.Read() + WSensor.Read() + NSensor.Read()) / 4) - 25
     
    let ERead = ESensor.ToObservable() |> Observable.map (fun x -> (E, x))
    let SRead = SSensor.ToObservable() |> Observable.map (fun x -> (S, x))
    let WRead = WSensor.ToObservable() |> Observable.map (fun x -> (W, x))
    let NRead = NSensor.ToObservable() |> Observable.map (fun x -> (N, x))

    buttons.Start()

    use upButtonDispose = 
        buttons.ToObservable()
        |> Observable.filter (fun x -> ButtonEventCode.Up = x.Button)
        |> Observable.subscribe (fun _ -> exit.Set() |> ignore)

    let setter currDir = 
        match currDir with 
            | NE -> m1.SetPower 60; m3.SetPower 60
            | SW -> m1.SetPower -60; m3.SetPower -60
            | SE -> m2.SetPower -60; m4.SetPower -60
            | NW -> m2.SetPower 60; m4.SetPower 60

    let sensors = Observable.mergeList [ERead; SRead; WRead; NRead]
    use res = sensors 
              |> Observable.choose (fun (s, x) -> if x < initRead then Some s else None)
              |> Observable.scan changer NE
              |> Observable.subscribe (fun d -> m1.Stop(); m3.Stop(); m2.Stop(); m4.Stop(); setter d)

    setter NE

    exit.WaitOne() |> ignore
    0 
