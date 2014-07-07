#########################################################################################
########################### Unit Testing ################################################
#########################################################################################

import unittest

from MessageTranslationLibrary import *


def utEx(st):  # expand the sting with length and checkSum
    n = len(st) + 1
    st.append(utMakeCheckSumByte(st))
    lowByte, highByte = utConvertNaturalToBytes(n)
    return [lowByte, highByte] + st


def utEx2(st):  # expand the sting WITHOUT checksum
    n = len(st)
    lowByte, highByte = utConvertNaturalToBytes(n)
    return [lowByte, highByte] + st


# def bytes(intList):
#     result = ""
#     for x in intList:
#         result += chr(x)
#     return result


class TestMessageTranslationLibrary(unittest.TestCase):
    """Test the functions in the message translation library"""

    def testMakeCheckSumByte(self):
        self.assertEquals(0xFF, utMakeCheckSumByte([0x00]))
        self.assertEquals(0x00, utMakeCheckSumByte([0xF0, 0x0F]))
        self.assertEquals(0xFF, utMakeCheckSumByte([0xF0, 0x0F, 0xFF]))
        self.assertEquals(0x5C, utMakeCheckSumByte([0xA0, 0x01, 0x02]))

    def testMakeMsgStr(self):
        msg = PowerMsg(setting=kOn)
        self.assertEquals(bytearray(utEx([0x83])), makeMsgStr(msg))
        msg = PowerMsg(setting=kOff)
        self.assertEquals(bytearray(utEx([0x82])), makeMsgStr(msg))

        msg = LocoSpdMsg(slot=1, speed=2)
        self.assertEquals(bytearray(utEx([0xA0, 0x01, 0x02])), makeMsgStr(msg))
        msg = LocoSpdMsg(slot=1, speed=300)
        self.assertEquals(bytearray(utEx([0xA0, 0x01, 0x7F])), makeMsgStr(msg))

        msg = LocoDirfMsg(slot=1, direction=kBackward, lights=kOn, horn=kOn, bell=kOn)
        self.assertEquals(bytearray(utEx([0xA1, 0x01, 0x33])), makeMsgStr(msg))
        msg = LocoDirfMsg(slot=1, direction=kForward, lights=kOff, horn=kOff, bell=kOff)
        self.assertEquals(bytearray(utEx([0xA1, 0x01, 0x00])), makeMsgStr(msg))

        msg = LocoSndMsg(slot=1, mute=kOn, F5=kOn, F6=kOn)
        self.assertEquals(bytearray(utEx([0xA2, 0x01, 0x0B])), makeMsgStr(msg))
        msg = LocoSndMsg(slot=1, mute=kOff, F5=kOff, F6=kOff)
        self.assertEquals(bytearray(utEx([0xA2, 0x01, 0x00])), makeMsgStr(msg))

        msg = SwReqMsg(switch=1, direction=kClosed)
        self.assertEquals(bytearray(utEx([0xB0, 0x00, 0x30])), makeMsgStr(msg))
        msg = SwReqMsg(switch=1, direction=kThrown)
        self.assertEquals(bytearray(utEx([0xB0, 0x00, 0x10])), makeMsgStr(msg))

        msg = MoveSlotsMsg(slot1=1, slot2=2)
        self.assertEquals(bytearray(utEx([0xBA, 0x01, 0x02])), makeMsgStr(msg))

        msg = LocoAdrMsg(address=1111)
        st = [0xBF, (1111 // 128), (1111 % 128)]
        self.assertEquals(bytearray(utEx(st)), makeMsgStr(msg))

        msg = WriteSlotDataToClearMsg(slot=1)
        st = [0xEF, 0x0E, 0x01, 0x0B, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
        self.assertEquals(bytearray(utEx(st)), makeMsgStr(msg))

        msg = DoLocoInitMsg(address=1111, sensors=[1, 2, 3])
        st = [0, 8, (1111 % 128), (1111 // 128), 3, 1, 0, 2, 0, 3, 0]
        self.assertEquals(bytearray(utEx2(st)), makeMsgStr(msg))

        msg = DoReadLayoutMsg(fileName='abc')
        st = [0, 10, 3, ord('a'), ord('b'), ord('c')]
        self.assertEquals(bytes(utEx2(st)), makeMsgStr(msg))

        msg = DoMakeSectionUsableMsg(sensor1=200, sensor2=300,)
        st = [0, 33, (200 % 128), (200 // 128), (300 % 128), (300 // 128)]
        self.assertEquals(bytearray(utEx2(st)), makeMsgStr(msg))

        msg = GetPathMsg(slot=1, pathKind=kBreadthFirst, preSensor=200, fromSensor=300, \
                         toSensor=400, sensorsToExclude = [1001, 1002, 1003, 1004])
        st = [0, 35, 1, kBreadthFirst, (200 % 128), (200 // 128), (300 % 128), (300 // 128) , (400 % 128), (400 // 128),
              4,
              (1001 % 128), (1001 // 128),
              (1002 % 128), (1002 // 128),
              (1003 % 128), (1003 // 128),
              (1004 % 128), (1004 // 128)]
        self.assertEquals(bytearray(utEx2(st)), makeMsgStr(msg))

        msg = GetTrainPositionMsg(slot=119)
        st = [0, 31, 119]
        self.assertEquals(bytearray(utEx2(st)), makeMsgStr(msg))

    # noinspection PyListCreation
    def testSplitMsgStr(self):
        pass
        st = [0xB2, 0, 0]
        st.append(utMakeCheckSumByte(st))
        self.assertEquals(InputRepMsg(sensor=1, isHi=False), splitMsgStr(st))

        st = [0xB2, 4, 0]
        st.append(utMakeCheckSumByte(st))
        self.assertEquals(InputRepMsg(sensor=9, isHi=False), splitMsgStr(st))

        st = [0xB2, 4, 0x10]
        st.append(utMakeCheckSumByte(st))
        self.assertEquals(InputRepMsg(sensor=9, isHi=True), splitMsgStr(st))

        st = [0xB2, 121, 0x31]
        st.append(utMakeCheckSumByte(st))
        self.assertEquals(InputRepMsg(sensor=500, isHi=True), splitMsgStr(st))

        st = [0xB2, 121, 0x01]
        st.append(utMakeCheckSumByte(st))
        self.assertEquals(InputRepMsg(sensor=499, isHi=False), splitMsgStr(st))

        st = [0xB1, 115, 0x33]
        st.append(utMakeCheckSumByte(st))
        self.assertEquals(SwRepMsg(switch=500, direction=kClosed), splitMsgStr(st))

        st = [0xB4, 0x3F, 0x00]
        st.append(utMakeCheckSumByte(st))
        self.assertEquals(LongAckMsg(responseToOpcode=OPC_LOCO_ADR, switchState=0),
                          splitMsgStr(st))

        st = [0xE7, 0x0E, 0x01, 0x30,
              1111 % 128, 0x00, 0x00, 0x00, 0x00, 1111 // 128,
              0x00, 0x00, 0x00]
        st.append(utMakeCheckSumByte(st))
        self.assertEquals(SlRdDataMsg(address=1111, isAddressAlreadyInUse=True, slot=1),
                          splitMsgStr(st))

        st = [0, 1, 1, kMoving]
        self.assertEquals(PutTrainStateMsg(slot=1, state=kMoving), splitMsgStr(st))

        st = [0, 2, 1, 3, 73, 1, 74, 1, 75, 1]
        self.assertEquals(PutTrainPositionMsg(slot=1, sensors=[201, 202, 203]),
                          splitMsgStr(st))

        st = [0, 36, (6%128), (6//128), 73,1, 74,1, 75,1, 76,1, 77,1, 78,1]
        self.assertEquals(PutPathMsg(sensors=[201, 202, 203, 204, 205, 206]),
                          splitMsgStr(st))

        st = [0, 3, 72, 1, kFree]
        self.assertEquals(PutSectionStateMsg(id=200, state=kFree), splitMsgStr(st))

        st = [0, 4, 99, kBeginClosed]
        self.assertEquals(PutSwitchStateMsg(id=99, state=kBeginClosed), splitMsgStr(st))

        st = [0, 5, 116, 3, kSensorOpen]
        self.assertEquals(PutSensorStateMsg(id=500, state=kSensorOpen), splitMsgStr(st))

        st = [0, 9, 1111 % 128, 1111 // 128, 1, 11, 0, 2]
        self.assertEquals(PutInitOutcomeMsg(physAdd=1111, physSlot=1, virtAdd=11, virtSlot=2),
                          splitMsgStr(st))

        st = [0, 11, 100, 72, 1]
        self.assertEquals(PutReadLayoutResponseMsg(responseFlag=100, code=200),
                          splitMsgStr(st))

        st = [0, 21, 1, 100, kForward, kOn, kOff, kOn, kOn]
        self.assertEquals(PutTrainInformationMsg(slot=1, speed=100, direction=kForward,
                                                 lights=kOn, bell=kOff, horn=kOn, mute=kOn),
                          splitMsgStr(st))

        st = [0, 26]
        self.assertEquals(PutPowerChangeCompleteMsg(dummy=0), splitMsgStr(st))

        st = [0, 34, 200 % 128, 200 // 128, 300 % 128, 300 // 128, 1]
        self.assertEquals(PutMakeSectionUsableResponseMsg(sensor1=200, sensor2=300,flag=1),
                          splitMsgStr(st))

if __name__ == "__main__":
    suite = unittest.makeSuite(TestMessageTranslationLibrary)
    unittest.TextTestRunner().run(suite)
    #unittest.main()
    #input("press enter")


