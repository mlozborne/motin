############################################################################
###################### Unit Testing             ############################
############################################################################

import time
from Log import gLog
from MessageTranslationTypes import *
from StartAndKill import StartAndKill
from MsgHandler import MsgSocket
from threading import Thread


def raw_input(st):
    return input(st)

############################################################################


def testSend():
    """
    Start RailroadBig and connect to it at (local host, 1234)
    Register train 1111 and assume response is physical slot 1
    Tell train to blink its lights 5 times
    Will see only the messages sent.
    """
    sak = StartAndKill()
    print("Entering function testSend")
    sak.setPath("../../runSoftware")
    sak.start("simulator")
    time.sleep(3)
    sk = MsgSocket(name = "1")                # sk is the socket to the simulator
    sk.connect('localhost', 1234)
    sk.send(LocoAdrMsg(address=1111))         # find slot for loco 1111 and assume response is slot 1
    time.sleep(1)
    sk.send(MoveSlotsMsg(slot1=1, slot2=1))   # register slot 1
    time.sleep(1)
    for i in range(1, 6):
        # send to slot 1 messages to turn the lights on and off
        sk.send(LocoDirfMsg(slot=1, direction=kBackward, lights=kOn, horn=kOff, bell=kOn))
        time.sleep(0.2)
        sk.send(LocoDirfMsg(slot=1, direction=kForward, lights=kOff, horn=kOff, bell=kOff))
        time.sleep(0.2)
    raw_input("Press enter to kill RailroadBig and leave function testSend")
    sk.close()
    sak.kill("simulator")
    print("Leaving function testSend")

############################################################################


def testReceive():
    """
    Start RailroadBig and connect to it at (local host, 1234).
    Start a separate thread to send messages to the railroad.
        Register train 1111 and assume response is physical slot 1
        Tell train to blink lights and change direction 5 times.
    Should see messages sent and messages received.
    """
    sak = StartAndKill()
    print("Entering function testReceive\n")
    sak.setPath("../../runSoftware")
    sak.start("simulator")
    sk = MsgSocket(name = "1")
    sk.connect('localhost', 1234)
    process = TestBlinkLightsDirectly(sk)
    process.start()
    while True:
        try:
            sk.receive()
        except:
            break
    sk.close()
    print("Leaving function testReceive\n")


class TestBlinkLightsDirectly(Thread):
    def __init__(self, sk):
        Thread.__init__(self, name="BlinkLightsDirectly")
        self.sk = sk

    def run(self):
        print("Starting thread BlinkLightsDirectly\n")
        sak = StartAndKill()
        self.sk.send(LocoAdrMsg(address=1111))
        time.sleep(1)
        self.sk.send(MoveSlotsMsg(slot1=1, slot2=1))
        time.sleep(1)
        for i in range(1, 6):
            self.sk.send(LocoDirfMsg(slot=1, direction=kBackward, lights=kOn, horn=kOff, bell=kOn))
            time.sleep(0.2)
            self.sk.send(LocoDirfMsg(slot=1, direction=kForward, lights=kOff, horn=kOff, bell=kOff))
            time.sleep(0.2)
        raw_input("\nPress enter to kill RailroadBig and stop thread TestBlinkLightsDirectly \n")
        sak.kill("simulator")
        print("Ending thread TestBlinkLightsDirectly\n")

############################################################################


def testTalkingToController():
    """
    Start RailroadBig and Controller.
    Start a separate thread to send messages to the railroad.
        Tell Controller to read layout.xml.
        Initialize train 1111 at position [5,1] and assume response is virtual slot 5.
        Tell train to blink lights and change direction five times.
    Should see messages sent and messages received.
    """
    print("Entering function testTalkingToController")
    sak = StartAndKill()
    sak.setPath("../../runSoftware")
    sak.start("simulator")
    sak.start("controller")
    sk = MsgSocket(name = "1")
    sk.connect('localhost', 1235)
    time.sleep(2)
    sk.send(DoReadLayoutMsg('../../runSoftware/Layout.xml'))
    process = TestBlinkLightsViaController(sk)
    time.sleep(5)
    process.start()
    while True:
        try:
            sk.receive()
        except:
            break
    sk.close()
    print("Leaving function testTalkingToController\n")


class TestBlinkLightsViaController(Thread):
    def __init__(self, sk):
        Thread.__init__(self, name="BlinkLightsViaController")
        self.sk = sk

    def run(self):
        print("Starting thread TestBlinkLightsViaController\n")
        sak = StartAndKill()
        self.sk.send(DoLocoInitMsg(address=1111, sensors=[5, 1]))
        time.sleep(1)
        for i in range(1, 6):
            self.sk.send(LocoDirfMsg(slot=5, direction=kBackward, lights=kOn, horn=kOff, bell=kOn))
            time.sleep(0.2)
            self.sk.send(LocoDirfMsg(slot=5, direction=kForward, lights=kOff, horn=kOff, bell=kOff))
            time.sleep(0.2)
        raw_input("\nPress enter to kill RailroadBig and Controller and stop thread BlinkLightsViaController \n")
        sak.kill("simulator")
        sak.kill("controller")
        print("Ending thread TestBlinkLightsViaController\n")

############################################################################
#      RUN FROM WINDOWS EXPLORER
############################################################################

if __name__ == "__main__":
    gLog.open("1")
    while True:
        print("\n\n\n")
        print("1. Test send")
        print("2. Test receive")
        print("3. Test talking to controller")
        print("Q. Quit")
        option = raw_input(">>> ")
        print("\n\n\n")
        if option == "q":
            break
        elif option == "1":
            gLog.print("testSend()\n")
            testSend()
        elif option == "2":
            gLog.print("testReceive()\n")
            testReceive()
        elif option == "3":
            gLog.print("testTalkingToController()\n")
            testTalkingToController()
        gLog.flush()
        gLog.print("\n\n\n")
    gLog.close()
