class MsgSocket(object):
    def __init__(self):
        pass

    def createMsgServerThread(self,address, clientHandlerRunFunction):
        MessageServerThread(self, address, clientHandlerRunFunction).start


class MsgServerThread(Thread):
    def __init__(self, address, clientHandlerRunFunction):
        Thread.__init__(self)
        self.clientHandlerRunFunction = clientHandlerRunFunction
        self.serverSocket = socket()
        self.serverSocket.bind(address)                          # bind
        self.serverSocket.listen(5)                              # listen

    def run(self):
        while True:
            socketToClient, address = self.serverSocket.accept() # accept
            ClientHandlerThread(socketToClient, self.clientHandlerRunFunction).start()

class ClientHandlerThread(thread):
    def __init__(self, socketToClient, clientHandlerRunFunction):
        Thread.__init__(self)
        self.socketToClient = socketToClient
        self.clientHandlerRunFunction = clientHandlerRunFunction

    def run(self):
        msgSocket = MsgSocket()
        msgSocket.
        clientHandlerRunFunction(self.socketToClient)
        message = decode(self.client.recv(1024), "ascii")        # receive
        if not message:
            self.theGUI.printMessage('Client disconnected')
        else:
            self.theGUI.printMessage(message)
        self.client.close()

