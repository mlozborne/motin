import multiprocessing.queues
from MessageTranslationTypes import *
from breezypythongui import DISABLED, EasyFrame, N, NORMAL, W
from tkinter import PhotoImage
from multiprocessing import Process
from Throttle import Throttle
from Log import *

class GuiThrottleProcess(Process):
    def __init__(self, name = "1", inQu = None, outQu = None):
        Process.__init__(self)

        assert(isinstance(name, str))
        assert(isinstance(inQu, multiprocessing.queues.Queue))
        assert(isinstance(outQu, multiprocessing.queues.Queue))

        printLog("GuiThrottleProcess {0}: initializing".format(name))
        self.name = name
        self.inQu = inQu
        self.outQu = outQu
        
    def run(self):
        openLog("GuiThrottleProcess")
        printLog("GuiThrottleProcess {0}: running".format(self.name))
        GuiThrottle(self.inQu, self.outQu).mainloop()
        printLog("GuiThrottleProcess {0}: finished running".format(self.name))
        
class GuiThrottle(EasyFrame):

    def __init__(self, inQu, outQu):
        EasyFrame.__init__(self, title="Throttle")
        printLog("GuiThrottle: initializing ")

        self.throttle = Throttle(name = "1", inQu = inQu, outQu = outQu)

        imageFolder = "C:\\Documents and Settings\\Martin\\Desktop\\Trains SVN\\Scripting\\Gifs\\"

        self.toggles = {'lights': kOff, 'horn': kOff, 'bell': kOff, 
                        'mute': kOff, 'direction': kForward}

        # Label and field for train address
        self.addLabel(text="Train address", row=0, column=0)
        self.addressField = self.addIntegerField(value=0, row=0, column=1, sticky = N+W)

        # Label and field for train position
        self.addLabel(text="Position", row=1, column=0)
        self.positionField = self.addTextField(text="[...]", row=1, column=1, sticky = N+W)

        # Label and field for train state
        self.addLabel(text="State", row=2, column=0)
        self.stateField = self.addTextField("Halted", row=2, column=1, sticky = N+W)

        # Button initialize 
        self.btInitialize = self.addButton(text="  Initialize  ", 
            row=3, column=0, command=self.initTrain)

        # Button direction
        self.btDirection  = self.addButton(text="  Direction  ", 
            row=3, column=1, command=self.changeDirection, state = DISABLED)
        self.gifTrainRight = PhotoImage(file = imageFolder + 'TrainRight.gif')
        self.gifTrainLeft = PhotoImage(file = imageFolder + 'TrainLeft.gif')
        self.btDirection.config(image=self.gifTrainRight,width="60",height="20")

        # Button lights
        self.btLight = self.addButton(text="    Lights    ", 
            row=5, column=0, command=self.changeLights, state = DISABLED)
        self.gifLight = PhotoImage(file = imageFolder + 'Light.gif')
        self.btLight.config(image=self.gifLight,width="60",height="20")

        # Button bell
        self.btBell = self.addButton(text="      Bell      ", 
            row=5, column=1, command=self.changeBell, state = DISABLED)
        self.gifBell = PhotoImage(file = imageFolder + 'bell.gif')
        self.btBell.config(image=self.gifBell,width="60",height="20")

        # Button horn
        self.btHorn = self.addButton(text="     Horn     ", 
            row=6, column=0, command=self.stopHorn, state = DISABLED)
        self.btHorn.bind("<Button>", self.startHorn)
        self.gifHorn = PhotoImage(file= imageFolder + 'horn.gif')
        self.btHorn.config(image=self.gifHorn,width="60",height="20")


        # Button mute
        self.btMute = self.addButton(text="     Mute     ", 
            row=6, column=1, command=self.changeMute, state = DISABLED)

        # Button close next
        self.btCloseNext = self.addButton(text="Close Next", 
            row=7, column=0, command=self.closeNext, state = DISABLED)

        # Button throw next
        self.btThrowNext = self.addButton(text="Throw Next", 
            row=7, column=1, command=self.throwNext, state = DISABLED)

        # Slider speed
        self.slSpeed = self.addScale(label = "Speed",
                                          row = 9, column = 0, columnspan = 2,
                                          from_ = 0, to = 127,
                                          resolution = 1,
                                          length = 250,
                                          tickinterval = 0,
                                          command = None)
        self.slSpeed.set(0)

        # Halt button
        self.btHalt = self.addButton(text="     Halt     ", 
            row=10, column=0, command=self.haltTrain, columnspan = 2, state = DISABLED)

    def initTrain(self):
        msg = self.throttle.initTrain(address = 1111, position = [5, 1])
        printLog("GuiThrottle: physAdd = {0}, physSlot = {1}, virtAdd = {2}, virtSlot = {3}".
            format(msg.physAdd, msg.physSlot, msg.virtAdd, msg.virtSlot))

        if msg.physSlot > 120:
            self.messageBox(title = "ERROR", message = "Error code {0}".format(msg.physAdd))
        else:
            self.throttle.setVirtSlot(msg.virtSlot)
            self.btDirection.config(state = NORMAL)
            self.btBell.config(state = NORMAL)
            self.btCloseNext.config(state = NORMAL)
            self.btThrowNext.config(state = NORMAL)
            self.btLight.config(state = NORMAL)
            self.btHorn.config(state = NORMAL)
            self.btMute.config(state = NORMAL)
            self.btHalt.config(state = NORMAL)
            self.slSpeed.config(command = self.changeSpeed)
            self.btInitialize.config(state = DISABLED)

    def flipToggle(self, key):
        if key == "direction":
            if self.toggles[key] == kForward:
                self.toggles[key] = kBackward
            else:
                self.toggles[key] = kForward
        else:
            if self.toggles[key] == kOff:
                self.toggles[key] = kOn
            else:
                self.toggles[key] = kOff

    def changeDirection(self):
        self.flipToggle("direction")
        if self.toggles['direction'] == kForward:
            self.btDirection.config(image=self.gifTrainRight,width="60",height="20")
        else:
            self.btDirection.config(image=self.gifTrainLeft,width="60",height="20")
        self.throttle.setDirection(self.toggles['direction'])

    def changeLights(self):
        self.flipToggle("lights")
        self.throttle.setLights(self.toggles['lights'])

    def changeBell(self):
        self.flipToggle("bell")
        self.throttle.setBell(self.toggles['bell'])

    def startHorn(self, location):
        self.throttle.setHorn(kOn)

    def stopHorn(self):
        self.throttle.setHorn(kOff)

    def changeMute(self):
        self.flipToggle("mute")
        self.throttle.setMute(self.toggles['mute'])

    def closeNext(self):
        self.throttle.closeNextSwitch()

    def throwNext(self):
        self.throttle.throwNextSwitch()

    def changeSpeed(self, speed):
        self.throttle.setSpeed(int(speed))

    def haltTrain(self):
        self.throttle.setSpeed(1)
        self.slSpeed.set(0)


if __name__ == "__main__":
    GuiThrottleProcess().start()