class EventHook(object):

    def __init__(self):
        self.__handlers = []
        self.onceHandlers = []
        self.alwaysHandlers = []

    def __iadd__(self, handler):
        self.__handlers.append(handler)
        return self

    def addOnce(self, handler):
        self.onceHandlers.append(handler)

    def addAlways(self, handler):
        self.alwaysHandlers.append(handler)

    def __isub__(self, handler):
        self.__handlers.remove(handler)
        return self

    def fire(self, * args, ** keywargs):
        for handler in self.__handlers:
            handler(*args, ** keywargs)
        for handler in self.alwaysHandlers:
            handler(*args, ** keywargs)
        for handler in self.onceHandlers:
            handler(*args, ** keywargs)
            self.onceHandlers = []

    def clearObjectHandlers(self, inObject):
        for theHandler in self.__handlers:
            if theHandler.im_self == inObject:
                self -= theHandler

    def __str__(self):
        return str(self.__handlers)

class Switch(object):
    def __init__(self):
        self.value = None
        self.myListeners = EventHook()

    def setValue(self, swDir):
        oldValue = self.value
        self.value = swDir
        if oldValue != swDir:
            self.myListeners.fire(swDir)

class Train(object):
    def __init__(self, id):
        self.id = id

    def switchChange(self, swDir):
        if swDir == "closed":
            print("start train {0}".format(self.id))
        else:
            print("stop train {0}".format(self.id))

def dummy(swDir):
    print("switch direction = {0}".format(swDir))

def main():
    switches = {}  
    for i in range(5):
        switches[i] = Switch()

    train1 = Train(1)
    train2 = Train(2)
    train3 = Train(3)

    switches[1].myListeners.addAlways(dummy)
    switches[1].myListeners.addOnce(train1.switchChange)
    switches[1].myListeners.addAlways(train2.switchChange)
    switches[1].myListeners.addAlways(train3.switchChange)

    switches[1].setValue("thrown")
    print("")
    switches[1].setValue("closed")

main()
