from collections import namedtuple

#######################################################################
# Bit settings for messages
bitBackward          = 0x20 #  0010 0000
bitLightsOn          = 0x10 #  0001 0000
bitHornOn            = 0x02 #  0000 0010
bitBellOn            = 0x01 #  0000 0001
bitMuteOn            = 0x08 #  0000 1000 F8 on UT4
bitF5                = 0x01 #  0000 0001 F5 on UT4, change in value tells controller to close next switch
bitF6                = 0x02 #  0000 0010 F6 on UT4, change in value tells controller to throw next switch
bitRequestClose      = 0x30
bitRequestThrow      = 0x10
bitReportClosed      = 0x30
bitReportThrown      = 0x20

#######################################################################
# Some message constants
kOn                   = 1        # For lights, horn, bell, mute, power
kOff                  = 0

kForward              = 0        # For train direction
kBackward             = 1

kMoving               = 0        # For train states
kWaiting              = 1
kHalted               = 2
kError                = 3
kBeginChangeDirection = 4
kBeginWaiting         = 5
kBeginHalted          = 6

kFree                 = 0        # For section states
kReserved             = 1
kOccupied             = 2
kBlocked              = 3

kClosed               = 0        # For switch states
kThrown               = 1
kBeginClosed          = 2
kBeginThrown          = 3
kRead                 = 4
kUnknown              = 5

kSensorOpen           = 0        # For sensor states
kSensorClosed         = 1

#######################################################################
# Opcodes sent to controller/railroad
OPC_GPOFF      = '\x82' # power off
OPC_GPON       = '\x83' # power on
OPC_LOCO_SPD   = '\xA0' # set speed
OPC_LOCO_DIRF  = '\xA1' # set direction, horn, bell, lights
OPC_LOCO_SND   = '\xA2' # set mute and unmute sound, close/throw next switch
OPC_SW_REQ     = '\xB0' # move a turnout
OPC_MOVE_SLOTS = '\xBA' # set a slot to in-use
OPC_LOCO_ADR   = '\xBF' # request for slot data
OPC_WR_SL_DATA = '\xEF' # write data into a slot
OPC_SW_STATE   = '\xBC' # request state of a turnout
doLocoInit             = chr(8)
doReadLayout           = chr(10)
getSwitchStates        = chr(22)

#######################################################################
# Opcodes received from controller/railroad
OPC_INPUT_REP          = '\xB2' # report sensor fired
OPC_SW_REP             = '\xB1' # report turnout now closed/thrown
OPC_LONG_ACK           = '\xB4' # if 2nd byte = 3F, then insufficient slots
OPC_SL_RD_DATA         = '\xE7' # slot data response
putTrainState          = chr(1)
putTrainPosition       = chr(2)
putSectionState        = chr(3)
putSwitchState         = chr(4)
putSensorState         = chr(5)
putInitOutcome         = chr(9)
putReadLayoutResponse  = chr(11)
putTrainInformation    = chr(21)
putPowerChangeComplete = chr(26)

#######################################################################
# Sent message formats
PowerMsg                   = namedtuple('PowerMsg', 'setting')
LocoSpdMsg                 = namedtuple('LocoSpdMsg', 'slot, speed')
LocoDirfMsg                = namedtuple('LocoDirfMsg', 'slot, direction, lights, horn, bell')
LocoSndMsg                 = namedtuple('LocoSndMsg', 'slot, mute, F5, F6')
SwReqMsg                   = namedtuple('SwReqMsg', 'switch, direction')
MoveSlotsMsg               = namedtuple('MoveSlotsMsg', 'slot1, slot2')
LocoAdrMsg                 = namedtuple('LocoAdrMsg', 'address')
WriteSlotDataToClearMsg    = namedtuple('WriteSlotDataToClearMsg', 'slot')
DoLocoInitMsg              = namedtuple('DoLocoInitMsg', 'address, sensors')
DoReadLayoutMsg            = namedtuple('DoReadLayoutMsg', 'fileName')

#######################################################################
# Received message formats
InputRepMsg                 = namedtuple('InputRepMsg', 'sensor, isHi')
SwRepMsg                    = namedtuple('SwRepMsg', 'switch, direction')
LongAckMsg                  = namedtuple('LongAckMsg', 'responseToOpcode, switchState')
SlRdDataMsg                 = namedtuple('SlRdDataMsg', 'address, isAddressAlreadyInUse, slot')
PutTrainStateMsg            = namedtuple('PutTrainStateMsg', 'slot, state')
PutTrainPositionMsg         = namedtuple('PutTrainPositionMsg', 'slot, sensors')
PutSectionStateMsg          = namedtuple('PutSectionStateMsg', 'id, state')
PutSwitchStateMsg           = namedtuple('PutSwitchStateMsg', 'id, state')
PutSensorStateMsg           = namedtuple('PutSensorStateMsg', 'id, state')
PutInitOutcomeMsg           = namedtuple('PutInitOutcomeMsg', 'physAdd, physSlot, virtAdd, virtSlot')
PutReadLayoutResponseMsg    = namedtuple('PutReadLayoutResponseMsg', 'responseFlag, code')
PutTrainInformationMsg      = namedtuple('PutTrainInformationMsg', 'slot, speed, direction, lights, bell, horn, mute')
PutPowerChangeCompleteMsg   = namedtuple('PutPowerChangeCompleteMsg', 'dummy')
