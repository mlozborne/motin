from breezypythongui import EasyFrame
from threading import Thread
from queue import Queue


class GuiThread(Thread):
    def __init__(self, number, quSend, quRead):
        print("Starting thread {0}".format(number))
        Thread.__init__(self)
        TheGui(number, quSend, quRead).mainloop()

class TheGui(EasyFrame):
    def __init__(self, number, quSend, quRead):
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

        # Set up and start the message server for this window
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
        Thread.__init__(self)
        self.myGui = myGui
        self.myQu = myQu

    def run(self):
        message = self.myQu.get()           # receive
        self.myGui.printMessage(message)

if __name__ == "__main__":
    qu1 = Queue()
    qu2 = Queue()
    GuiThread(1, qu1, qu2).start()
    GuiThread(2, qu2, qu1).start()
    print("qu1 size {0}".format(qu1.qsize()))
    print("qu2 size {0}".format(qu2.qsize()))


