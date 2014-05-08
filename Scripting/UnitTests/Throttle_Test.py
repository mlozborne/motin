from StartAndKill import StartAndKill
from MessageTranslationTypes import *
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
    self.setHorn(kOn)
    sleep(1)

    self.setHorn(kOff)
    sleep(.2)

    self.setHorn(kOn)
    sleep(.2)

    self.setHorn(kOff)
    sleep(.2)

    self.setHorn(kOn)
    sleep(1)

    self.setHorn(kOff)

if __name__ == "__main__":
    gLog.open()

    #  Start the simulator and controller
    sak = StartAndKill()
    sak.start("simulator")
    sak.start("controller")

    # Create the communication resources for 1 user
    comRes = CommunicationResources(name = 'throttle-test', host = 'localhost', port = 1235, numberOfPackages = 2)
    
    throt1111 = Throttle(name = 'Bill', comPkg = comRes.getNextPackage())
    
    # Tell the throttle to read the layout file
    gLog.print("Main reading layout")
    msg = throt1111.readLayout("../../runSoftware/Layout.xml")
    sleep(2)

    # Tell the throttle to initialize train 1111
    # gLog.print("Main initializing train")
    # msg = throt1111.initTrain(1111, [5, 1])
    # gLog.print("physAdd = {0}, physSlot = {1}, virtAdd = {2}, virtSlot = {3}".
    #            format(msg.physAdd, msg.physSlot, msg.virtAdd, msg.virtSlot))
    # if msg.physSlot > 120:
    #     print("\nABEND: couldn't initialize the train. Response code = {0}".format(msg.physSlot))
    #     input("press enter to quit")

    # Use the throttle to send messages to the controller
    # Option 1: using a command list
    testing = 2222
    if testing == 1111:
        commands = [[initTrain, 1111, [5, 1]],
                    [setBell, kOn], [blinkLights, 4], [setBell, kOff], [tootHorn], [setSpeed, 100],
                    [pause, 3], [throwNextSwitch], [moveSwitch, 12, kClosed], [pause, 4], [moveSwitch, 12, kThrown],
                    [addInterest, PutSensorStateMsg], [waitFor, PutSensorStateMsg(id = 59, state = kSensorOpen)],
                    [removeInterest, PutSensorStateMsg], [stopTrain]]
        doCommands(throt1111, commands)
        path = ((77, 18, kThrown), (77, 22, kClosed), (77, 15, kThrown), (77, 11, kClosed), (77, 9, kClosed),
                (47, 5, kClosed), (27, 6, kClosed), (17, 7, kClosed), (12, 8, kClosed), (10, 4, kClosed),
                (62, 13, kClosed), (80, 17, kClosed))
        commands = ((pause, 1), (setLights, kOn), (setSpeed, 50), (followPath, path))
        doCommands(throt1111, commands)
    else:
        throt2222 = Throttle(name = 'Mary', comPkg = comRes.getNextPackage())
        commandPath = ((6, (moveSwitch, 4, kThrown)), (6, (moveSwitch, 12, kThrown)), (6, (moveSwitch, 13, kClosed)),
                       (80, (blinkLights, 4)), (76, (moveSwitch, 17, kClosed)), (94, (setDirection, kBackward)),
                       (2, (setDirection, kForward)), (2, (stopTrain,)))
        commands = [[initTrain, 2222, [6, 2]], [setLights, kOn], [moveSwitch, 2, kThrown], [moveSwitch, 3, kClosed],
                    [moveSwitch, 4, kClosed], [setSpeed, 50], [followCommandPath, commandPath]]
        doCommands(throt2222, commands)

    # Option 2: using throttle commands
    """
    throt.setBell(kOn)
    throt.do(blinkLights, 4)
    throt.setBell(kOff)
    throt.do(tootHorn)
    throt.setSpeed(100)
    sleep(3)
    throt.throwNextSwitch()
    throt.moveSwitch(12, kClosed)
    sleep(4)
    throt.moveSwitch(12, kThrown)

    # Stop the train when it reaches sensor 59
    throt.addInterest(PutSensorStateMsg)
    throt.waitFor(PutSensorStateMsg(id = 59, state = kSensorOpen))
    throt.removeInterest(PutSensorStateMsg)
    throt.do(stopTrain)
    """

    gLog.flush()
    raw_input("press enter to quit")
    throt1111.close()
    sak.kill("controller")
    sak.kill("simulator")
    gLog.close()
