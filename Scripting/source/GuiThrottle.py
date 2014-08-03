import multiprocessing.queues
from breezypythongui import DISABLED, EasyFrame, N, NORMAL, W, EasyDialog, END
from tkinter import PhotoImage
from multiprocessing import Process
from Throttle import *
from Log import gLog
from MsgHandler import AddInterestMsg, CommunicationsPackage

class GuiThrottleProcess(Process):
    def __init__(self, name = None, comPkg = None):
        Process.__init__(self)
        gLog.print("GuiThrottleProcess {0}: initializing".format(name))
        assert(isinstance(name, str))

        assert(isinstance(comPkg, CommunicationsPackage))
        inQu = comPkg.inQu
        inQuNum = comPkg.inQuNum
        outQu = comPkg.outQu

        assert(isinstance(inQu, multiprocessing.queues.Queue))
        assert(inQuNum >= 0)
        assert(isinstance(outQu, multiprocessing.queues.Queue))

        self.name = name
        self.comPkg = comPkg
        
    def run(self):
        gLog.open("GuiThrottleProcess {0}".format(self.name))
        gLog.print("GuiThrottleProcess {0}: running".format(self.name))
        GuiThrottle(name = self.name, comPkg = self.comPkg).mainloop()
        gLog.print("GuiThrottleProcess {0}: finished running".format(self.name))
        

class EnterCommandDialog(EasyDialog):
    def __init__(self, parent, atSensorCommands):
        self.atSensorCommands = atSensorCommands
        EasyDialog.__init__(self, parent, "Enter Command")

    def body(self, master):
        self.addLabel(master, text = "At Sensor #", row = 0, column = 0)
        self.addLabel(master, text = "Do command", row = 1, column = 0)
        self.addLabel(master, text = "Using parameter 1", row = 13, column = 0)
        self.addLabel(master, text = "Using parameter 2", row = 14, column = 0)
        self.addLabel(master, text = "Using parameter 3", row = 15, column = 0)

        self.sensorFld = self.addIntegerField(master, value = 0, row = 0, column = 1)

        self.listBox = self.addListbox(master, row = 1, column = 1, rowspan = 12,
                                       width = 46, height = 12,
                                       listItemSelected = self.listItemSelected)
        self.listBox.insert(END, "forward")
        self.listBox.insert(END, "reverse")
        self.listBox.insert(END, "pause <n>")
        self.listBox.insert(END, "set speed <n>")
        self.listBox.insert(END, "throw switch <n>")
        self.listBox.insert(END, "close switch <n>")
        self.listBox.insert(END, "lights on")
        self.listBox.insert(END, "lights off")
        self.listBox.insert(END, "bell on")
        self.listBox.insert(END, "bell off")
        self.listBox.insert(END, "toot horn")
        self.listBox.insert(END, "at speed go to <speed><destination><excluding>")
        self.listBox.setSelectedIndex(0)

        self.parameter1Fld = self.addIntegerField(master, value = 0, row = 13, column = 1)
        self.parameter2Fld = self.addIntegerField(master, value = 0, row = 14, column = 1)
        self.parameter3Fld = self.addTextField(master, text = "[1,2]", row = 15, column = 1)

    def listItemSelected(self, index):
        return

    def apply(self):
        """When the OK button is clicked, transfers data from the
        fields, build the corresponding command, and append to the list of commands."""
        sensor = self.sensorFld.getNumber()
        commandName = self.listBox.getSelectedItem()
        parameter1 = self.parameter1Fld.getNumber()
        parameter2 = self.parameter2Fld.getNumber()
        parameter3 = self.parameter3Fld.getText()
        if commandName == "forward":
            command = [setDirection, kForward]
        elif commandName == "reverse":
            command = [setDirection, kBackward]
        elif commandName == "pause <n>":
            command = [pause, parameter1]
        elif commandName == "set speed <n>":
            command = [setSpeed, parameter1]
        elif commandName == "throw switch <n>":
            command = [moveSwitch, parameter1, kThrown]
        elif commandName == "close switch <n>":
            command = [moveSwitch, parameter1, kClosed]
        elif commandName == "lights on":
            command = [setLights, kOn]
        elif commandName == "lights off":
            command = [setLights, kOff]
        elif commandName == "bell on":
            command = [setBell, kOn]
        elif commandName == "bell off":
            command = [setBell, kOff]
        elif commandName == "toot horn":
            command = [tootHorn]
        elif commandName == "at speed go to <speed><destination><excluding>":
            command = [atSpeedGoTo, parameter1, parameter2, parameter3]
        else:
            command = None

        if command != None:
            self.atSensorCommands.append([sensor, command])
            self.setModified()


class EditCommandsDialog(EasyDialog):
    def __init__(self, parent, atSensorCommands):
        self.parent = parent
        EasyDialog.__init__(self, parent, "Enter and Edit Commands")

    def buttonbox(self):
        '''add standard button box.
        override if you do not want the standard buttons
        from tkinter simpledialog
        '''
        pass

    def body(self, master):
        self.textArea = self.addTextArea(master, text = "abc", row = 0, column = 0, columnspan = 2,
                                                 width = 50, height = 15)
        numCommands = len(self.parent.atSensorCommands)
        text = ''
        if numCommands > 0:
            for i in range(numCommands - 1):
                text += self.parent.atSensorCommands[i] + '\n'
            text += self.parent.atSensorCommands[numCommands -1]
        self.textArea.setText(text)
        self.okButton = self.addButton(master, text = "   OK   ", row = 1, column = 0, command = self.apply)
        self.cancelButton = self.addButton(master, text = " Cancel ", row = 1, column = 1, command = self.cancel)

    def apply(self):
        """When the OK button is clicked, transfers data from the
        text area and create the list of commands."""
        self.parent.atSensorCommands = self.textArea.getText().splitlines()
        self.cancel()

class GuiThrottle(EasyFrame):

    def __init__(self, name = None, comPkg = None):
        EasyFrame.__init__(self, title="Throttle " + name)
        gLog.print("GuiThrottle {0}: initializing".format(name))

        self.name = "Throttle " + name
        self.comPkg = comPkg
        inQu = comPkg.inQu
        inQuNum = comPkg.inQuNum
        outQu = comPkg.outQu

        self.atSensorCommands = []
        self.inQu = inQu
        self.virtSlot = None
        
        outQu.put(AddInterestMsg(inQuNum, PutInitOutcomeMsg))
        outQu.put(AddInterestMsg(inQuNum, PutTrainPositionMsg))
        outQu.put(AddInterestMsg(inQuNum, PutTrainStateMsg))
        outQu.put(AddInterestMsg(inQuNum, PutSensorStateMsg))
        
        self.readyToReadFromQueue = False

        self.throttle = Throttle(name = self.name, comPkg = self.comPkg)
        self.throttleReady = False

        imageFolder = "..\\Gifs\\"

        self.toggles = {'lights': kOff, 'horn': kOff, 'bell': kOff, 
                        'mute': kOff, 'direction': kForward}

        # Compute default values for address and position fields
        if name == "1":
            address = 1111
            position = "5, 1"
        elif name == "2":
            address = 2222
            position = "6, 2"
        elif name == "3":
            address = 3333
            position = "7, 3"
        elif name == "4":
            address = 4444
            position = "8, 4"
        else:
            address = 0
            position = ""

        # Label and field for train address
        self.addLabel(text="Train address", row=0, column=0)
        self.addressField = self.addIntegerField(value=address, row=0, column=1, sticky=N+W)

        # Label and field for train position
        self.addLabel(text="Position", row=1, column=0)
        self.positionField = self.addTextField(text=position, row=1, column=1, sticky=N+W)

        # Label and field for train state
        self.addLabel(text="State", row=2, column=0)
        self.stateField = self.addTextField("Halted", row=2, column=1, sticky=N+W)

        # Button initialize
        self.btInitialize = self.addButton(text="  Initialize  ", 
                                           row=3, column=0, command=self.initTrain)

        # Button direction
        self.btDirection  = self.addButton(text="  Direction  ", 
                                           row=3, column=1, command=self.changeDirection, state = DISABLED)
        self.gifTrainRight = PhotoImage(file = imageFolder + 'TrainRight.gif')
        self.gifTrainLeft = PhotoImage(file = imageFolder + 'TrainLeft.gif')
        self.btDirection.config(image=self.gifTrainRight, width="60", height="20")

        # Button lights
        self.btLight = self.addButton(text="    Lights    ", 
                                      row=4, column=0, command=self.changeLights, state = DISABLED)
        self.gifLight = PhotoImage(file = imageFolder + 'Light.gif')
        self.btLight.config(image=self.gifLight, width="60", height="20")

        # Button bell
        self.btBell = self.addButton(text="      Bell      ", 
                                     row=4, column=1, command=self.changeBell, state = DISABLED)
        self.gifBell = PhotoImage(file = imageFolder + 'bell.gif')
        self.btBell.config(image=self.gifBell, width="60", height="20")

        # Button horn
        self.btHorn = self.addButton(text="     Horn     ", 
                                     row=5, column=0, command=self.stopHorn, state = DISABLED)
        self.btHorn.bind("<ButtonPress-1>", self.startHorn)
        self.gifHorn = PhotoImage(file= imageFolder + 'horn.gif')
        self.btHorn.config(image=self.gifHorn, width="60", height="20")

        # Button mute
        self.btMute = self.addButton(text="     Mute     ", 
                                     row=5, column=1, command=self.changeMute, state = DISABLED)

        # Button close next
        self.btCloseNext = self.addButton(text="Close Next", 
                                          row=6, column=0, command=self.closeNext, state = DISABLED)

        # Button throw next
        self.btThrowNext = self.addButton(text="Throw Next", 
                                          row=6, column=1, command=self.throwNext, state = DISABLED)

        # Button enter command
        self.btEnterCommand = self.addButton(text="Enter Command",
                                          row=7, column=0, command=self.enterCommand, state = DISABLED)

        # Button edit commands
        self.btDisplayCommands = self.addButton(text="Edit Commands",
                                          row=7, column=1, command=self.editCommands, state = DISABLED)

        # Slider speed
        self.slSpeed = self.addScale(label = "Speed",
                                     row = 8, column = 0, columnspan = 2,
                                     from_ = 0, to = 127,
                                     resolution = 1,
                                     length = 250,
                                     tickinterval = 0,
                                     command = None)
        self.slSpeed.set(0)

        # Halt button
        self.btHalt = self.addButton(text="     Halt     ", 
                                     row=9, column=0, command=self.haltTrain,
                                     columnspan = 2, state = DISABLED)

    def initTrain(self):
        self.readyToReadFromQueue = False
        self.toggles = {'lights': kOff, 'horn': kOff, 'bell': kOff, 
                        'mute': kOff, 'direction': kForward}
        if self.throttleReady:
            self.throttle.setSpeed(1)
            self.throttle.setDirection(kForward)
            self.throttle.setHorn(kOff)
            self.throttle.setBell(kOff)
            self.throttle.setMute(kOff)
            self.throttle.setLights(kOff)
            self.throttle.setVirtSlot(None)
            self.virtSlot = None
            self.throttleReady = False
       
        self.btDirection.config(state = DISABLED)
        self.btBell.config(state = DISABLED)
        self.btCloseNext.config(state = DISABLED)
        self.btThrowNext.config(state = DISABLED)
        self.btLight.config(state = DISABLED)
        self.btHorn.config(state = DISABLED)
        self.btMute.config(state = DISABLED)
        self.btEnterCommand.config(state = DISABLED)
        self.btDisplayCommands.config(state = DISABLED)
        self.btHalt.config(state = DISABLED)
        self.slSpeed.config(command = None)
        self.slSpeed.set(0)
        
        try:
            address = self.addressField.getNumber()
            if not (0 <= address <= 9999):
                raise "EXCEPTION want 0 <= address <= 9999"
        except:
            self.messageBox("ERROR", "For address enter an integer in range 0..9999")
            return

        txtPosition = self.positionField.getText()
        try:
            if txtPosition.strip() == "":
                position = []
            else:
                position = [int(x) for x in txtPosition.split(",")]
                if len(position) == 1:
                    raise "EXCEPTION position must reference at least 2 sensors"
        except:
            self.messageBox("ERROR", "For position enter comma delimited integers or blank.\n\n"
                            + "List sensors from front to back of train or leave blank to unregister a train.")
            return
        
        msg = self.throttle.initTrain(address = address, position = position)
        gLog.print("GuiThrottle: physAdd = {0}, physSlot = {1}, virtAdd = {2}, virtSlot = {3}".
                   format(msg.physAdd, msg.physSlot, msg.virtAdd, msg.virtSlot))

        if msg.physSlot > 120:
            self.messageBox(title = "ERROR", message = "Invalid position: error code {0}".format(msg.physSlot))
            return

        # Enable buttons and make throttle ready
        self.btDirection.config(image=self.gifTrainRight, width="60", height="20")
        self.throttle.setVirtSlot(msg.virtSlot)
        self.virtSlot = msg.virtSlot
        self.throttleReady = True
        self.btDirection.config(state = NORMAL)
        self.btBell.config(state = NORMAL)
        self.btCloseNext.config(state = NORMAL)
        self.btThrowNext.config(state = NORMAL)
        self.btLight.config(state = NORMAL)
        self.btHorn.config(state = NORMAL)
        self.btMute.config(state = NORMAL)
        self.btEnterCommand.config(state = NORMAL)
        self.btDisplayCommands.config(state = NORMAL)
        self.btHalt.config(state = NORMAL)
        self.slSpeed.config(command = self.changeSpeed)

        self.readyToReadFromQueue = True
        self.readQueue()

    def readQueue(self):
        if not self.readyToReadFromQueue:
            return
        try:
            while True:
                msg = self.inQu.get(False) # non-blocking
                self.processMsg(msg)
        except multiprocessing.queues.Empty:
            pass
        finally:
            self.after(100, self.readQueue)

    def processMsg(self, msg):
        if isinstance(msg, PutTrainStateMsg):

            if msg.slot != self.virtSlot:
                return
            self.stateField.setText(kTrainStateList[msg.state])

        elif isinstance(msg, PutTrainPositionMsg):

            if msg.slot != self.virtSlot:
                return
            self.positionField.setText(str(msg.sensors)[1:-1])

            # Do all initial atSensorCommands that have sensor number corresponding to train's new position

            for item in self.atSensorCommands:
                # If the command is empty then remove it and continue to next command
                if item == []:
                    self.atSensorCommands.remove(item)
                    continue

                # Split the command into its parts and retrieve the sensor number
                s = item.split()
                sensor = int(s[0])

                # Compare the command's sensor number to the train's new position
                if sensor != msg.sensors[1]:
                    # Doesn't match so return
                    return

                # Command's sensor number matches train's new position
                # Remove the command and process it.
                if   s[1] == "lightson":
                    self.throttle.doCommand([setLights, kOn])
                elif s[1] == "lightsoff":
                    self.throttle.doCommand([setLights, kOff])
                elif s[1] == "bellon":
                    self.throttle.doCommand([setBell, kOn])
                elif s[1] == "bellof":
                    self.throttle.doCommand([setBell, kOff])
                elif s[1] == "speed":
                    speed = int(s[2])
                    self.throttle.doCommand([setSpeed, speed])
                    self.slSpeed.set(speed)            #    reset speed slider
                elif s[1] == "reverse":
                    self.throttle.doCommand([setDirection, kBackward])
                elif s[1] == "forward":
                    self.throttle.doCommand([setDirection, kForward])
                elif s[1] == "throw":
                    switchId = int(s[2])
                    self.throttle.doCommand([moveSwitch, switchId, kThrown])
                elif s[1] == "close":
                    switchId = int(s[2])
                    self.throttle.doCommand([moveSwitch, switchId, kClosed])
                else:
                    print("Command not understood: ".format(s))
                self.atSensorCommands.remove(item)

            # for item in self.atSensorCommands:
            #     if item[0] != msg.sensors[1]:
            #         return
            #     self.throttle.doCommand(item[1])
            #     if (item[1])[0] == setSpeed:           # if setSpeed command:
            #         self.slSpeed.set((item[1])[1])     #    reset speed slider
            #     self.atSensorCommands.remove(item)

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
            self.btDirection.config(image=self.gifTrainRight, width="60", height="20")
        else:
            self.btDirection.config(image=self.gifTrainLeft, width="60", height="20")
        self.throttle.setDirection(self.toggles['direction'])

    def changeLights(self):
        self.flipToggle("lights")
        self.throttle.setLights(self.toggles['lights'])

    def changeBell(self):
        self.flipToggle("bell")
        self.throttle.setBell(self.toggles['bell'])

    def startHorn(self, dummy):
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

    def enterCommand(self):
        dialog = EnterCommandDialog(self, self.atSensorCommands)
        print("Commands: {0}".format(self.atSensorCommands))

    def editCommands(self):
        """Pops up a dialog to edit the model.
        Updates the app window if the song was modified."""
        dialog = EditCommandsDialog(self, self.atSensorCommands)
        for command in self.atSensorCommands:
            s = command.split()
            print("Command: {0}".format(s))

