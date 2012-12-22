class MsgSocket(object):
    def __init__(self):
        pass

    def createMsgServerThread(self,address, clientHandlerRunFunction):
        MessageServerThread(self, address, clientHandlerRunFunction).start

    def connect(self):
        self.inBuffer = []
        self.sock = socket()
        while True:
            if not self.sock.connect_ex((host, port)): break
            sleep(1)

    def setup(self, sock):
        self.sock = sock
        self.inBuffer = []

    def send(self, msg):
        st = makeMsgStr(msg)
        ba = bytes(st)
        self.sock.sendall(ba)
        printLog("<<< Sent message = {0}".format(msg))

    def close(self):
        self.sock.close()
        printLog("Closed RailSocket                  ...in TCP")

    def receive(self):
        if len(self.inBuffer) < 2:
            buf = self.sock.recv(1024)
            self.inBuffer += buf
        strSize = self.inBuffer[0] + 128 * self.inBuffer[1]
        while strSize + 2 > len(self.inBuffer):
            buf = self.sock.recv(1024)
            self.inBuffer += buf
        strMsg = self.inBuffer[2:2 + strSize]
        self.inBuffer = self.inBuffer[2 + strSize:]
        msg = splitMsgStr(strMsg)
        printLog("    >>> Received {0}".format(msg))
        return msg


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
        msgSocketToClient = MsgSocket()
        msgSocketToClient.setup(self.socketToClient)
        clientHandlerRunFunction(msgSocketToClient)
        self.msgSocketToClient.close()

