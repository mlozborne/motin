from breezypythongui import EasyFrame, N, W, NORMAL, DISABLED
from Throttle import Throttle
#from TCP import RailSocket
from MessageTranslationTypes import kOff, kOn

class ThrottleGUI(EasyFrame):

    def __init__(self, sk):
        """Sets up the view.  The model comes in as an argument."""
        EasyFrame.__init__(self, title="Throttle")
        self.sk = sk                        # socket to railroad
        self.lights = kOff
        self.throttle = Throttle(sk)

        # Label and field for train address
        self.addLabel(text="Train address", row=0, column=0)
        self.addressField = self.addIntegerField(value=0, row=0, column=1, sticky = N+W)

        # Label and field for train position
        self.addLabel(text="Position", row=1, column=0)
        self.positionField = self.addTextField(text="[...]", row=1, column=1, sticky = N+W)

        # Label and field for train state
        self.addLabel(text="State", row=2, column=0)
        self.stateField = self.addTextField("Halted", row=2, column=1, sticky = N+W)

        # Initialize button
        self.addButton(text="  Initialize  ", row=3, column=0, command=self.initializeTrain, columnspan = 2)

        # Lights button
        self.lightButton = self.addButton(text="    Lights    ", row=5, column=0, command=self.changeLights, state = DISABLED)

        # Bell button
        self.bellButton = self.addButton(text="      Bell      ", row=5, column=1, command=self.changeBell, state = DISABLED)

        # Horn button
        self.hornButton = self.addButton(text="     Horn     ", row=6, column=0, command=self.changeHorn, state = DISABLED)

        # Mute button
        self.muteButton = self.addButton(text="     Mute     ", row=6, column=1, command=self.changeMute, state = DISABLED)

        # Close next button
        self.closeNextButton = self.addButton(text="Close Next", row=7, column=0, command=self.closeNext, state = DISABLED)

        # Throw next button
        self.throwNextButton = self.addButton(text="Throw Next", row=7, column=1, command=self.throwNext, state = DISABLED)

        # Speed slider
        self.speedSlider = self.addScale(label = "Speed",
                                          row = 9, column = 0, columnspan = 2,
                                          from_ = 0, to = 127,
                                          resolution = 1,
                                          length = 250,
                                          tickinterval = 0,
                                          command = self.changeSpeed)
        self.speedSlider.set(0)

        # Halt button
        self.haltButton = self.addButton(text="     Halt     ", row=10, column=0, command=self.haltTrain, columnspan = 2, state = DISABLED)

    def initializeTrain(self):
        physAdd, physSlot, virtAdd, virtSlot = self.throttle.doLocoInit(1111, [5, 1])
        print("physAdd = {0}, physSlot = {1}, virtAdd = {2}, virtSlot = {3}".format(physAdd, physSlot, virtAdd, virtSlot))
        if physSlot > 120:
            print("\nABEND: couldn't initialize the train. Response code = {0}".format(physSlot))
            input("press enter to quit")
        self.bellButton.config(state = NORMAL)
        self.closeNextButton.config(state = NORMAL)
        self.throwNextButton.config(state = NORMAL)
        self.lightButton.config(state = NORMAL)
        self.hornButton.config(state = NORMAL)
        self.muteButton.config(state = NORMAL)
        self.haltButton.config(state = NORMAL)

    def changeLights(self):
        if self.lights == kOff:
            self.lights = kOn
        else:
            self.lights = kOff
        self.throttle.setLights(self.lights)

    def changeBell(self):
        pass

    def changeHorn(self):
        pass

    def changeMute(self):
        pass

    def closeNext(self):
        pass

    def throwNext(self):
        pass

    def changeSpeed(self, speed):
        pass

    def haltTrain(self):
        pass



