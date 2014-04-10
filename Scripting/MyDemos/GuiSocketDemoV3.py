"""
Similar in function to the other versions but is easier to start because it creates two
processes which in turn create the GUI threads. So there isn't a separate starter program.
"""

from socket import *
from codecs import decode
from breezypythongui import EasyFrame
from threading import Thread
from multiprocessing import Process


class GuiProcess(Process):
    def __init__(self, number, myPort, otherPort):
        self.number = number
        self.myPort = myPort
        self.otherPort = otherPort
        Process.__init__(self)

    def run(self):
        TheGui(self.number, self.myPort, self.otherPort).mainloop()


class TheGui(EasyFrame):

    def __init__(self, number, myPort, otherPort):
        """Sets up the window and widgets."""
        EasyFrame.__init__(self, title = "Message App " + str(number))

        # Label and field for the input
        self.addLabel(text = "Input",
                      row = 0, column = 0)
        self.inputField = self.addTextField(text = "",
                                            row = 0,
                                            column = 1)

        # Text area for the output
        self.outputArea = self.addTextArea(text = "",
                                           row = 1,
                                           column = 0,
                                           columnspan = 2,
                                           width = 50, height = 15)

        # The command button
        self.button = self.addButton(text = "Send",
                                     row = 2, column = 0,
                                     columnspan = 2,
                                     command = self.sendMessage)

        # Set up and start the message server for this window
        myAddress = ('localhost', myPort)
        self.otherAddress = ('localhost', otherPort)
        myServer = MessageServer(self, myAddress)
        myServer.start()

    # The event handling method for the button

    def sendMessage(self):
        """Inputs the user's text and sends it to the other window's
        message server."""
        message = self.inputField.getText()
        otherServer = socket(AF_INET, SOCK_STREAM)
        otherServer.connect(self.otherAddress)
        otherServer.send(bytes(message, 'ascii'))
        otherServer.close()

    def printMessage(self, message):
        """Appends message to the output area."""
        self.outputArea.appendText(message + "\n")
       

class MessageServer(Thread):
    """Server class for the windows."""
    
    def __init__(self, theGUI, address):
        Thread.__init__(self)
        self.theGUI = theGUI
        self.server = socket(AF_INET, SOCK_STREAM)
        self.server.bind(address)
        self.server.listen(5)
        
    def run(self):
        while True:
            client, address = self.server.accept()
            message = decode(client.recv(1024), "ascii")
            if not message:
                self.theGUI.printMessage('Message empty')
            else:
                self.theGUI.printMessage(message)
            client.close()


if __name__ == "__main__":
    GuiProcess(1, 5000, 5001).start()
    GuiProcess(2, 5001, 5000).start()



