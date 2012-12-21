From Throttle.py

    def doLocoInit(self, locoAddress, sensors):
        """
        Won't work because throttle should not receive directly           <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        """
        self.quToCon.send(DoLocoInitMsg(address=locoAddress, sensors=sensors))
        msg = self.quToCon.get()
        while not isinstance(msg, PutInitOutcomeMsg):
            msg = self.quToCon.receive()
        self.virtSlot = msg.virtSlot
        return msg.physAdd, msg.physSlot, msg.virtAdd, msg.virtSlot

###########################################################################