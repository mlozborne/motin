#########################################################################################
########################### Unit Testing ################################################
#########################################################################################

import unittest
from MessageTranslationTypes import *
from MessageTranslationLibrary import *


def utEx(st):                        # expand the sting with length and checkSum
    n = len(st) + 1
    st += utMakeCheckSumByte(st)
    lowByte, highByte = utConvertNaturalToBytes(n)
    return lowByte + highByte + st

def utEx2(st):                       # expand the sting WITHOUT checksum
    n = len(st)
    lowByte, highByte = utConvertNaturalToBytes(n)
    return lowByte + highByte + st

class TestMessageTranslationLibrary(unittest.TestCase):
    """Test the functions in the message translation library"""

    def testMakeCheckSumByte(self):
        self.assertEquals("\xFF", utMakeCheckSumByte("\x00"))
        self.assertEquals("\x00", utMakeCheckSumByte("\xF0\x0F"))
        self.assertEquals("\xFF", utMakeCheckSumByte("\xF0\x0F\xFF"))
        self.assertEquals("\x5C", utMakeCheckSumByte("\xA0\x01\x02"))

    def testMakeMsgStr(self):
        msg = PowerMsg(setting=kOn)
        self.assertEquals(utEx("\x83"), makeMsgStr(msg))
        msg = PowerMsg(setting=kOff)
        self.assertEquals(utEx("\x82"), makeMsgStr(msg))

        msg = LocoSpdMsg(slot=1, speed=2)
        self.assertEquals(utEx("\xA0\x01\x02"), makeMsgStr(msg))
        msg = LocoSpdMsg(slot=1, speed=300)
        self.assertEquals(utEx("\xA0\x01\x7F"), makeMsgStr(msg))

        msg = LocoDirfMsg(slot=1, direction=kBackward, lights=kOn, horn=kOn, bell=kOn)
        self.assertEquals(utEx("\xA1\x01\x33"), makeMsgStr(msg))
        msg = LocoDirfMsg(slot=1, direction=kForward, lights=kOff, horn=kOff, bell=kOff)
        self.assertEquals(utEx("\xA1\x01\x00"), makeMsgStr(msg))

        msg = LocoSndMsg(slot=1, mute=kOn, F5=kOn, F6=kOn)
        self.assertEquals(utEx("\xA2\x01\x0B"), makeMsgStr(msg))
        msg = LocoSndMsg(slot=1, mute=kOff, F5=kOff, F6=kOff)
        self.assertEquals(utEx("\xA2\x01\x00"), makeMsgStr(msg))

        msg = SwReqMsg(switch=1, direction=kClosed)
        self.assertEquals(utEx("\xB0\x00\x30"), makeMsgStr(msg))
        msg = SwReqMsg(switch=1, direction=kThrown)
        self.assertEquals(utEx("\xB0\x00\x10"), makeMsgStr(msg))

        msg = MoveSlotsMsg(slot1=1, slot2=2)
        self.assertEquals(utEx("\xBA\x01\x02"), makeMsgStr(msg))

        msg = LocoAdrMsg(address=1111)
        st = "\xBF" + chr(1111 // 128) + chr(1111 % 128)
        self.assertEquals(utEx(st), makeMsgStr(msg))

        msg = WriteSlotDataToClearMsg(slot=1)
        st = "\xEF\x0E\x01\x0B\x00\x00\x00\x00\x00\x00\x00\x00\x00"
        self.assertEquals(utEx(st), makeMsgStr(msg))

        msg = DoLocoInitMsg(address=1111, sensors=[1, 2, 3])
        st = chr(0) + chr(8) + chr(1111 % 128) + chr(1111 // 128) + \
            chr(3) + \
            chr(1) + chr(0) + \
            chr(2) + chr(0) + \
            chr(3) + chr(0)
        self.assertEquals(utEx2(st), makeMsgStr(msg))

        msg = DoReadLayoutMsg(fileName="abc")
        st = chr(0) + chr(10) + chr(3) + "abc"
        self.assertEquals(utEx2(st), makeMsgStr(msg))

    def testSplitMsgStr(self):
        st = "\xB2" + chr(0) + chr(0)
        st += utMakeCheckSumByte(st)
        self.assertEquals(InputRepMsg(sensor=1, isHi=False), splitMsgStr(st))
        st = "\xB2" + chr(4) + chr(0)
        st += utMakeCheckSumByte(st)
        self.assertEquals(InputRepMsg(sensor=9, isHi=False), splitMsgStr(st))
        st = "\xB2" + chr(4) + "\x10"
        st += utMakeCheckSumByte(st)
        self.assertEquals(InputRepMsg(sensor=9, isHi=True), splitMsgStr(st))
        st = "\xB2" + chr(121) + "\x31"
        st += utMakeCheckSumByte(st)
        self.assertEquals(InputRepMsg(sensor=500, isHi=True), splitMsgStr(st))
        st = "\xB2" + chr(121) + "\x01"
        st += utMakeCheckSumByte(st)
        self.assertEquals(InputRepMsg(sensor=499, isHi=False), splitMsgStr(st))

        st = "\xB1" + chr(115) + "\x33"
        st += utMakeCheckSumByte(st)
        self.assertEquals(SwRepMsg(switch=500, direction=kClosed), splitMsgStr(st))

        st = "\xB4" + "\x3F" + "\x00"
        st += utMakeCheckSumByte(st)
        self.assertEquals(LongAckMsg(responseToOpcode=OPC_LOCO_ADR, switchState=0), \
                          splitMsgStr(st))

        st = "\xE7\x0E\x01\x30" + \
            chr(1111 % 128) + "\x00\x00\x00\x00" + chr(1111 // 128) + \
            "\x00\x00\x00"
        st += utMakeCheckSumByte(st)
        self.assertEquals(SlRdDataMsg(address=1111, isAddressAlreadyInUse=True, slot=1), \
                          splitMsgStr(st))

        st = chr(0) + chr(1) + chr(1) + chr(kMoving)
        self.assertEquals(PutTrainStateMsg(slot=1, state=kMoving), splitMsgStr(st))

        st = chr(0) + chr(2) + chr(1) + chr(3) + \
            chr(73) + chr(1) + \
            chr(74) + chr(1) + \
            chr(75) + chr(1)
        self.assertEquals(PutTrainPositionMsg(slot=1, sensors=[201, 202, 203]), \
                          splitMsgStr(st))

        st = chr(0) + chr(3) + chr(72) + chr(1) + chr(kFree)
        self.assertEquals(PutSectionStateMsg(id=200, state=kFree), splitMsgStr(st))

        st = chr(0) + chr(4) + chr(99) + chr(kBeginClosed)
        self.assertEquals(PutSwitchStateMsg(id=99, state=kBeginClosed), splitMsgStr(st))

        st = chr(0) + chr(5) + chr(116) + chr(3) + chr(kSensorOpen)
        self.assertEquals(PutSensorStateMsg(id=500, state=kSensorOpen), splitMsgStr(st))

        st = chr(0) + chr(9) + \
            chr(1111 % 128) + chr(1111 // 128) + chr(1) + \
            chr(11) + chr(0) + chr(2)
        self.assertEquals(PutInitOutcomeMsg(physAdd=1111, physSlot=1, virtAdd=11, virtSlot=2), \
                          splitMsgStr(st))

        st = chr(0) + chr(11) + chr(100) + chr(72) + chr(1)
        self.assertEquals(PutReadLayoutResponseMsg(responseFlag=100, code=200), \
                          splitMsgStr(st))

        st = chr(0) + chr(21) + chr(1) + chr(100) + chr(kForward) + \
            chr(kOn) + chr(kOff) + chr(kOn) + chr(kOn)
        self.assertEquals(PutTrainInformationMsg(slot=1, speed=100, direction=kForward,
                          lights=kOn, bell=kOff, horn=kOn, mute=kOn), \
                          splitMsgStr(st))

        st = chr(0) + chr(26)
        self.assertEquals(PutPowerChangeCompleteMsg(dummy=0), splitMsgStr(st))

if __name__ == "__main__":
    suite = unittest.makeSuite(TestMessageTranslationLibrary)
    unittest.TextTestRunner().run(suite)
    #unittest.main()
    input("press enter")