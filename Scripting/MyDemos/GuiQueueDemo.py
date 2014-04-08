from breezypythongui import EasyFrame
from multiprocessing import Process, Queue
from threading import Thread
import sys
from Log import gLog


class GuiProcess(Process):
    def __init__(self, number, quSend, quRead):
        print("Initializing process {0}".format(number)); sys.stdout.flush()
        gLog.print("Initializing process {0}".format(number))
        self.number = number
        self.quSend = quSend
        self.quRead = quRead
        Process.__init__(self)
        
    def run(self):
        gLog.open(str(self.number))
        print("Running process {0}".format(self.number)); sys.stdout.flush()
        gLog.print("Running process {0}".format(self.number))
        TheGui(self.number, self.quSend, self.quRead).mainloop()
        

class TheGui(EasyFrame):
    def __init__(self, number, quSend, quRead):
        print("Initializing gui {0}".format(number)); sys.stdout.flush()
        gLog.print("Initializing gui {0}".format(number))
        self.number = number
        self.quSend = quSend
        self.quRead = quRead

        EasyFrame.__init__(self, title = "GUI " + str(number))

        # Label and field for the input
        self.addLabel(text = "Input", row = 0, column = 0)
        self.inputField = self.addTextField(text = "", row = 0, column = 1)

        # Text area for the output
        self.outputArea = self.addTextArea(text = "", row = 1, column = 0, columnspan = 2, width = 50, height = 15)

        # The command button
        self.button = self.addButton(text = "Send", row = 2, column = 0, columnspan = 2, command = self.sendMessage)

        # Set up the queue reader for this window
        quReaderThread = QuReaderThread(self, self.quRead)
        quReaderThread.start()

    # The event handling method for the button
    def sendMessage(self):
        message = self.inputField.getText()
        self.quSend.put(message)                  # send

    def printMessage(self, message):
        self.outputArea.appendText(message + "\n")
       

class QuReaderThread(Thread):
    
    def __init__(self, myGui, myQu):
        print("Initializing a queue reader"); sys.stdout.flush()
        gLog.print("Initializing a queue reader")
        Thread.__init__(self)
        self.myGui = myGui
        self.myQu = myQu

    def run(self):
        while True:
            message = self.myQu.get()
            self.myGui.printMessage(message)

if __name__ == "__main__":
    gLog.open("main")
    qu1 = Queue()
    qu2 = Queue()
    GuiProcess(1, qu1, qu2).start()
    GuiProcess(2, qu2, qu1).start()
    


