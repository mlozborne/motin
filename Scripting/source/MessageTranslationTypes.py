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
kTrainStateList = ["Moving", "Waiting", "Halted", "Error", "BeginChangeDirection", "BeginWaiting", "BeginHalted"]

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
OPC_GPOFF              = 0x82 # power off
OPC_GPON               = 0x83 # power on
OPC_LOCO_SPD           = 0xA0 # set speed
OPC_LOCO_DIRF          = 0xA1 # set direction, horn, bell, lights
OPC_LOCO_SND           = 0xA2 # set mute and unmute sound, close/throw next switch
OPC_SW_REQ             = 0xB0 # move a turnout
OPC_MOVE_SLOTS         = 0xBA # set a slot to in-use
OPC_LOCO_ADR           = 0xBF # request for slot data
OPC_WR_SL_DATA         = 0xEF # write data into a slot
OPC_SW_STATE           = 0xBC # request state of a turnout
doLocoInit             = 8
doReadLayout           = 10
getSwitchStates        = 22
doMakeSectionUsable    = 33
getPath                = 35

#######################################################################
# Opcodes received from controller/railroad
OPC_INPUT_REP                  = 0xB2 # report sensor fired
OPC_SW_REP                     = 0xB1 # report turnout now closed/thrown
OPC_LONG_ACK                   = 0xB4 # if 2nd byte = 3F, then insufficient slots
OPC_SL_RD_DATA                 = 0xE7 # slot data response
putTrainState                  = 1
putTrainPosition               = 2
putSectionState                = 3
putSwitchState                 = 4
putSensorState                 = 5
putInitOutcome                 = 9
putReadLayoutResponse          = 11
putTrainInformation            = 21
putPowerChangeComplete         = 26
putMakeSectionUsableResponse   = 34
putPath                        = 36

#######################################################################
# Constants used in messages
kDirectionValues = (kForward, kBackward)
kSetRequestValues = (kThrown, kClosed)
kPowerValues = (kOn, kOff)
kLightsValues = (kOn, kOff)
kHornValues = (kOn, kOff)
kBellValues = (kOn, kOff)
kMuteValues = (kOn, kOff)
kF5Values = (kOn, kOff)
kF6Values = (kOn, kOff)
kSlotMin = 1; kSlotMax = 127
kSpeedMin = 0; kSpeedMax = 127
kSwitchMin = 1; kSwitchMax = 128
kLocoAddressMin = 1; kLocoAddressMax = 9999
kSensorMin = 1; kSensorMax = 16383

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
DoMakeSectionUsableMsg     = namedtuple('DoMakeSectionUsableMsg', 'sensor1, sensor2')
GetPathMsg                 = namedtuple('GetPathMsg', 'preSensor, fromSensor, toSensor')

#######################################################################
# Received message formats
InputRepMsg                     = namedtuple('InputRepMsg', 'sensor, isHi')
SwRepMsg                        = namedtuple('SwRepMsg', 'switch, direction')
LongAckMsg                      = namedtuple('LongAckMsg', 'responseToOpcode, switchState')
SlRdDataMsg                     = namedtuple('SlRdDataMsg', 'address, isAddressAlreadyInUse, slot')
PutTrainStateMsg                = namedtuple('PutTrainStateMsg', 'slot, state')
PutTrainPositionMsg             = namedtuple('PutTrainPositionMsg', 'slot, sensors')
PutSectionStateMsg              = namedtuple('PutSectionStateMsg', 'id, state')
PutSwitchStateMsg               = namedtuple('PutSwitchStateMsg', 'id, state')
PutSensorStateMsg               = namedtuple('PutSensorStateMsg', 'id, state')
PutInitOutcomeMsg               = namedtuple('PutInitOutcomeMsg', 'physAdd, physSlot, virtAdd, virtSlot')
PutReadLayoutResponseMsg        = namedtuple('PutReadLayoutResponseMsg', 'responseFlag, code')
PutTrainInformationMsg          = namedtuple('PutTrainInformationMsg', 'slot, speed, direction, lights, bell, horn, mute')
PutPowerChangeCompleteMsg       = namedtuple('PutPowerChangeCompleteMsg', 'dummy')
PutMakeSectionUsableResponseMsg = namedtuple('PutMakeSectionUsableResponseMsg', 'sensor1, sensor2, flag')
PutPathMsg                      = namedtuple('PutPathMsg', 'sensors')

ControllerOutMsgs = (PowerMsg, LocoSpdMsg, LocoDirfMsg, LocoSndMsg, SwReqMsg,
                     MoveSlotsMsg, LocoAdrMsg, WriteSlotDataToClearMsg,
                     DoLocoInitMsg, DoReadLayoutMsg, DoMakeSectionUsableMsg, GetPathMsg)

ControllerInMsgs = (PutTrainStateMsg, PutTrainPositionMsg, PutSectionStateMsg,
                    PutSwitchStateMsg, PutSensorStateMsg, PutInitOutcomeMsg,
                    PutReadLayoutResponseMsg, PutTrainInformationMsg,
                    PutPowerChangeCompleteMsg, SwRepMsg, InputRepMsg, LongAckMsg, SlRdDataMsg,
                    PutMakeSectionUsableResponseMsg)


