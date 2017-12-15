module timeExample

type ITime =
    abstract GetHour : unit -> int

type ImageCalculator (time:ITime) =
    member x.GetImageForTimeOfDay() =
        let hour = time.GetHour()
        if hour > 6 && hour < 21
        then "sun.jpg"
        else "moon.jpg"

open Fuchu
open Foq

let suite = 
    testList "time" <|[
        testCase "at 15:00 the sun image should be expected" <| (fun _ ->
            let time = 
                 Mock<ITime>()
                    .Setup(fun mock -> <@ mock.GetHour() @>).Returns(15)
                    .Create()
            let calculator = ImageCalculator(time)
            let image = calculator.GetImageForTimeOfDay()
            Assert.Equal("", "sun.jpg", image)
        );
        testCase "at 01:00 the moon image should be expected" <| (fun _ ->
            let time = 
                Mock<ITime>
                    .With(fun mock -> <@ mock.GetHour() --> 01 @>)
            let calculator = ImageCalculator(time)
            let image = calculator.GetImageForTimeOfDay()
            Assert.Equal("", "moon.jpg", image)
        );
    ]