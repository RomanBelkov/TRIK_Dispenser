(*
*   Copyright 2014-2015 Roman Belkov
*
*   Licensed under the Apache License, Version 2.0 (the "License");
*   you may not use this file except in compliance with the License.
*   You may obtain a copy of the License at
*
*       http://www.apache.org/licenses/LICENSE-2.0
*
*   Unless required by applicable law or agreed to in writing, software
*   distributed under the License is distributed on an "AS IS" BASIS,
*   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
*   See the License for the specific language governing permissions and
*   limitations under the License.
*)

open Trik
open Trik.Reactive
open System
open System.Threading

let mask = 0xFF
let mainSpd = 60
let supSpd = 40
let tol = 80

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

//We want to use special parameters for servos because
//special external LED controller is used 
let LEDControllerServo = { stop = 0; zero = 0; min = 0; max = 2000000; period = 2000000 }

[<EntryPoint>]
let main _ = 
    use model = new Model()
    model.ServosConfig.[C1] <- LEDControllerServo
    model.ServosConfig.[C2] <- LEDControllerServo
    model.ServosConfig.[C3] <- LEDControllerServo

    use buttons = model.Buttons
    use NMotor = model.Motors.[M2]
    use WMotor = model.Motors.[M1]
    use EMotor = model.Motors.[M3]
    use SMotor = model.Motors.[M4]
    use red   = model.Servos.[C1]
    use green = model.Servos.[C2]
    use blue  = model.Servos.[C3]
    use ESensor = model.AnalogSensors.[A6]
    use WSensor = model.AnalogSensors.[A2]
    use SSensor = model.AnalogSensors.[A4]
    use NSensor = model.AnalogSensors.[A5]
    use VMSensor = model.MXNSensor

    use upButtonDispose = 
        buttons.ToObservable()
        |> Observable.filter (fun x -> ButtonEventCode.Up = x.Button)
        |> Observable.subscribe (fun _ -> NMotor.Stop(); SMotor.Stop(); WMotor.Stop(); EMotor.Stop(); exit.Set() |> ignore)

    let colSetter (r, g, b) = 
        let grDel = r * r + (255 - g) * (255 - g) + b * b
        let blDel = r * r + g * g + (255 - b) * (255 - b)
        let ylDel = (255 - r) * (255 - r) + (255 - g) * (255 - g) + b * b
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
        Observable.interval (TimeSpan.FromMilliseconds 500.)
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

    let sensors = ERead.Merge [SRead; WRead; NRead]
    use res = sensors 
              |> Observable.scan changer NE
              |> Observable.subscribe (fun d -> NMotor.Stop(); SMotor.Stop(); WMotor.Stop(); EMotor.Stop(); setter d)


    VMSensor.Start()
    VMSensor.Size <- (1, 1)
    buttons.Start()
    setter NE

    exit.WaitOne() |> ignore
    0 
