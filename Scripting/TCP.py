from socket import *
import time

from MessageTranslationLibrary import *

class RailSocket:
    def __init__(self, host, port):
        self.inBuffer = ""
        self.sock = socket(AF_INET, SOCK_STREAM)
        while True:
            if not self.sock.connect_ex((host, port)): break
            time.sleep(1)
		
    def send(self, msg):
        print "Sending {0}".format(msg)
        self.sock.sendall(makeMsgStr(msg))
		
    def close(self):
        self.sock.close()

    def receive(self):
        if len(self.inBuffer) < 2:
            buf = self.sock.recv(1024)
            self.inBuffer += buf
        strSize = ord(self.inBuffer[0]) + 128 * ord(self.inBuffer[1])
        while strSize + 2 > len(self.inBuffer):
            buf = self.sock.recv(1024)
            self.inBuffer += buf
        strMsg = self.inBuffer[2:2 + strSize]
        self.inBuffer = self.inBuffer[2 + strSize:]
        msg = splitMsgStr(strMsg)
        print "    Receiving {0}".format(msg)
        return msg

############################################################################
###################### Unit Testing             ############################
############################################################################

import StartAndKill as sak
from threading import Thread
from MessageTranslationLibrary import *

############################################################################

def testSend():
    """
    Start RailroadBig and connect to it at (local host, 1234)
    Register train 1111 and assume response is physical slot 1
    Tell train to blink its lights 5 times
    Will see only the messages sent.
    """
    print "Entering function testSend"
    sak.startSimulator()
    sk = RailSocket('localhost', 1234)
    sk.send(LocoAdrMsg(address=1111))
    time.sleep(1)
    sk.send(MoveSlotsMsg(slot1=1, slot2=1))
    time.sleep(1)
    for i in range(1, 6):
        sk.send(LocoDirfMsg(slot=1, direction=kBackward, lights=kOn, horn=kOff, bell=kOn))
        time.sleep(0.2)
        sk.send(LocoDirfMsg(slot=1, direction=kForward, lights=kOff, horn=kOff, bell=kOff))
        time.sleep(0.2)
    x = raw_input("Press enter to kill RailroadBig and leave function testSend")
    sk.close()
    sak.killSimulator()
    print "Leaving function testSend"

############################################################################

def testReceive():
    """
    Start RailroadBig and connect to it at (local host, 1234).
    Start a separate thread to send messages to the railroad.
        Register train 1111 and assume response is physical slot 1
        Tell train to blink lights and change direction 5 times.
    Should see messages sent and messages received.
    """
    print "Entering function testReceive\n"
    sak.startSimulator()
    sk = RailSocket('localhost', 1234)
    process = TestBlinkLightsDirectly(sk)
    process.start()
    while True:
        try:
            sk.receive()
        except:
            print "EXCEPTION in testReceive: sk.receive() failed because Railroad is dead\n"
            break
    print "Leaving function testReceive\n"

class TestBlinkLightsDirectly(Thread):
    def __init__(self, sk):
        Thread.__init__(self, name="BlinkLightsDirectly")
        self.sk = sk

    def run(self):
        print "Starting thread BlinkLightsDirectly\n"
        self.sk.send(LocoAdrMsg(address=1111))
        time.sleep(1)
        self.sk.send(MoveSlotsMsg(slot1=1, slot2=1))
        time.sleep(1)
        for i in range(1, 6):
            self.sk.send(LocoDirfMsg(slot=1, direction=kBackward, lights=kOn, horn=kOff, bell=kOn))
            time.sleep(0.2)
            self.sk.send(LocoDirfMsg(slot=1, direction=kForward, lights=kOff, horn=kOff, bell=kOff))
            time.sleep(0.2)
        x = raw_input("\nPress enter to kill RailroadBig and stop thread BlinkLightsDirectly \n")
        self.sk.close()
        sak.killSimulator()
        print "Ending thread BlinkLightsDirectly\n"

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
    print "Entering function testTalkingToController"
    sak.startSimulator()
    sak.startController()
    #sak.startController(ip = "127.0.0.1", port = "1234", trace = "yes")
    sk = RailSocket('localhost', 1235)
    time.sleep(2)
    sk.send(DoReadLayoutMsg("../runSoftware/Layout.xml"))
    process = TestBlinkLightsViaController(sk)
    time.sleep(5)
    process.start()
    while True:
        try:
            sk.receive()
        except:
            print "EXCEPTION in testTalkingToController: sk.receive() failed because Controller is dead\n"
            break
    print "Leaving function testTalkingToController\n"

class TestBlinkLightsViaController(Thread):
    def __init__(self, sk):
        Thread.__init__(self, name="BlinkLightsViaController")
        self.sk = sk

    def run(self):
        print "Starting thread BlinkLightsViaController\n"
        self.sk.send(DoLocoInitMsg(address=1111, sensors=[5, 1]))
        time.sleep(1)
        for i in range(1, 6):
            self.sk.send(LocoDirfMsg(slot=5, direction=kBackward, lights=kOn, horn=kOff, bell=kOn))
            time.sleep(0.2)
            self.sk.send(LocoDirfMsg(slot=5, direction=kForward, lights=kOff, horn=kOff, bell=kOff))
            time.sleep(0.2)
        x = raw_input("\nPress enter to kill RailroadBig and Controller and stop thread BlinkLightsViaController \n")
        self.sk.close()
        sak.killSimulator()
        sak.killController()
        print "Ending thread BlinkLightsViaController\n"

############################################################################
#      RUN FROM WINDOWS EXPLORER
############################################################################

if __name__ == "__main__":
    while True:
        print "\n\n\n"
        print "1. Test send"
        print "2. Test receive"
        print "3. Test talking to controller"
        print "Q. Quit"
        option = raw_input(">>> ")
        print "\n\n\n"
        if option == "q":
            break
        elif option == "1":
            testSend()
        elif option == "2":
            testReceive()
        elif option == "3":
            testTalkingToController()
       
