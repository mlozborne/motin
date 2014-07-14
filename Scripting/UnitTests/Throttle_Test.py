from StartAndKill import StartAndKill
from MsgHandler import *
from Throttle import *

def raw_input(st):
    return input(st)

def stopTrain(self):
    self.setSpeed(1)


def blinkLights(self, n):
    for i in range(n):
        self.setLights(kOn)
        sleep(0.2)
        self.setLights(kOff)
        sleep(0.2)


def tootHorn(self):
    for i in range(3):
        self.setHorn(kOn)
        sleep(1)
        self.setHorn(kOff)
        if i != 2:
            sleep(.2)


if __name__ == "__main__":
    gLog.open()

    #  Start the simulator and controller
    sak = StartAndKill()
    sak.start("simulator")
    sak.start("controller")

    # Create the communication resources for 1 user
    comRes = CommunicationResources(name = 'throttle-test', host = 'localhost', port = 1235, numberOfPackages = 1)
    
    myThrottle = Throttle(name = 'Bill', comPkg = comRes.getNextPackage())
    
    # Tell the throttle to read the layout file
    gLog.print("Main reading layout")
    msg = myThrottle.readLayout("../../runSoftware/Layout.xml")
    sleep(2)

    testing = 1  #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< set test case

    if testing == 1:

        # Initialize train 1111
        gLog.print("Main initializing train")
        msg = myThrottle.initTrain(1111, [5, 1])
        gLog.print("physAdd = {0}, physSlot = {1}, virtAdd = {2}, virtSlot = {3}".
                   format(msg.physAdd, msg.physSlot, msg.virtAdd, msg.virtSlot))

        # Check the response
        if msg.physSlot > 120:
            print("\nABEND: couldn't initialize the train. Response code = {0}".format(msg.physSlot))
            input("press enter to quit")

        # Bell on, blink lights 4 times, bell off, toot horn, speed 100, move some switches
        myThrottle.setBell(kOn)
        myThrottle.doFunction(blinkLights, 4)
        myThrottle.setBell(kOff)
        myThrottle.doFunction(tootHorn)
        myThrottle.setSpeed(100)
        sleep(3)
        myThrottle.throwNextSwitch()
        myThrottle.moveSwitch(12, kClosed)
        sleep(4)
        myThrottle.moveSwitch(12, kThrown)

        # At sensor 59 stop train with a command, tootHorn with a function, and blink lights with a command
        myThrottle.addInterest(PutSensorStateMsg)
        myThrottle.waitFor(PutSensorStateMsg(id = 59, state = kSensorOpen))
        myThrottle.removeInterest(PutSensorStateMsg)
        myThrottle.doCommand([stopTrain])
        myThrottle.doFunction(tootHorn)
        myThrottle.doCommand([blinkLights, 4])

    elif testing == 2:

        # Create a list of commands: initialize train 1111, bell on, blink lights, bell off,
        #                            toot horn, speed 100, moves some switches,
        #                            wait for sensor 59, stop train
        commands = [[initTrain, 1111, [5, 1]],
                    [setBell, kOn], [setLights, kOn], [blinkLights, 4], [setBell, kOff], [tootHorn], [setSpeed, 100],
                    [pause, 3], [throwNextSwitch], [moveSwitch, 12, kClosed], [pause, 4], [moveSwitch, 12, kThrown],
                    [addInterest, PutSensorStateMsg], [waitFor, PutSensorStateMsg(id = 59, state = kSensorOpen)],
                    [removeInterest, PutSensorStateMsg], [stopTrain]]
        doCommands(myThrottle, commands)

        # Create a switch path
        path = ((77, 18, kThrown), (77, 22, kClosed), (77, 15, kThrown), (77, 11, kClosed), (77, 9, kClosed),
                (47, 5, kClosed), (27, 6, kClosed), (17, 7, kClosed), (12, 8, kClosed), (10, 4, kClosed),
                (62, 13, kClosed), (80, 17, kClosed))

        # Create a list of commands: pause 1 second, lights on, speed 100, follow switch path, stop at sensor 94
        commands = ((pause, 1), (setLights, kOn), (setSpeed, 100), (followSwitchPath, path), (atSensorDo, 94, stopTrain))
        myThrottle.doCommands(commands)

    elif testing == 3:

        # Create a command path
        commandPath = ((6, (moveSwitch, 4, kThrown)), (6, (moveSwitch, 12, kThrown)), (6, (moveSwitch, 13, kClosed)),
                       (80, (blinkLights, 4)), (76, (moveSwitch, 17, kClosed)), (94, (setDirection, kBackward)),
                       (2, (setDirection, kForward)), (2, (stopTrain,)))

        # Create a list of commands: initialize train 2222, lights on, move some switches, speed 50,
        #                            follow a command path
        commands = [[initTrain, 2222, [6, 2]], [setLights, kOn], [moveSwitch, 2, kThrown], [moveSwitch, 3, kClosed],
                    [moveSwitch, 4, kClosed], [setSpeed, 100], [followCommandPath, commandPath]]
        doCommands(myThrottle, commands)

    elif testing == 4:

        # Test make some sections usable
        commands = [(makeSectionUsable, 7, 33), (makeSectionUsable, 33, 35), (makeSectionUsable, 59, 80),
                    (makeSectionUsable, 74, 94)]
        doCommands(myThrottle, commands)

    elif testing == 5:

        # Initialize train 4444, lights on, get a path, speed 100, follow the path, stop at sensor 94
        msg = myThrottle.initTrain(4444, [8, 4])
        myThrottle.setLights(kOn)
        myPath = myThrottle.getPath(kBreadthFirst, 4, 8, 94, [81, 52, 76])
        myThrottle.setSpeed(100)
        myThrottle.followSensorPath(myPath)
        myThrottle.atSensorDo(94, stopTrain)

    elif testing == 6:

        # Initialize train 4444, create a path, speed 100, follow path,
        # reverse at sensor 94, blink the lights
        msg = myThrottle.initTrain(4444, [8, 4])
        myPath = [8,33,35,62,59,80,76,74,94]
        myThrottle.setSpeed(100)
        myThrottle.followSensorPath(myPath)
        myThrottle.atSensorDo(94, (setDirection, kBackward))
        myThrottle.doFunction(blinkLights, 4)

    elif testing == 7:

        # Initialize train 4444, go to sensor 94 at speed 100, stop at sensor 94
        msg = myThrottle.initTrain(4444, [8, 4])
        myThrottle.atSpeedGoTo(100, 94, [74, 73, 113])
        myThrottle.atSensorDo(94, [stopTrain])

    # Shut down when user press ENTER
    gLog.flush()
    raw_input("press ENTER to quit")
    myThrottle.close()
    sak.kill("controller")
    sak.kill("simulator")
    gLog.close()
