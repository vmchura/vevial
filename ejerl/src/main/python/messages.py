import json


class ActionsForState(object):
    def __init__(self, actions):
        self.actions = actions

    @staticmethod
    def readfromstr(dictParam):
        return ActionsForState(dictParam["actions"])

    def asJson(self):
        return {"actions": self.actions}


class Regard(object):
    def __init__(self, value):
        self.value = value

    @staticmethod
    def readfromstr(dictParam):
        return Regard(dictParam["value"])

    def asJson(self):
        return {"value": self.value}


class Message(object):
    def __init__(self):
        pass

    @staticmethod
    def parseMessage(val):
        j = json.loads(val)
        typeClass = j["$type"]

        if typeClass == Welcome.typeClass:
            return Welcome(j["message"])

        if typeClass == InvalidRequest.typeClass:
            return InvalidRequest(j["error"])

        if typeClass == ExperimentResp.typeClass:
            return ExperimentResp(j["experimentID"], j["stateDim"])

        if typeClass == Farewell.typeClass:
            return Farewell(j["message"])

        if typeClass == NewExperiment.typeClass:
            return NewExperiment(j["experiment"])

        if typeClass == Action.typeClass:
            return Action(j["experimentID"], j["action"])

        if typeClass == NewState.typeClass:
            return NewState(j["state"],j["actionsForNewState"], j["regard"])

        raise NotImplementedError("can not parse " + str(j))


class Request(Message):
    def __init__(self):
        super().__init__()


class Response(Message):
    def __init__(self):
        super().__init__()


class Welcome(Message):
    typeClass = "CommunicationLayerPython.Welcome"

    def __init__(self, message):
        self.message = message

    def __str__(self):
        var = json.dumps({"$type": Welcome.typeClass,
                          "message": self.message})
        return str(var)


class Farewell(Message):
    typeClass = "CommunicationLayerPython.Farewell"

    def __init__(self, message):
        self.message = message

    def __str__(self):
        var = json.dumps({"$type": Farewell.typeClass,
                          "message": self.message})
        return str(var)


# Requests


class NewExperiment(Request):
    typeClass = "CommunicationLayerPython.NewExperiment"

    def __init__(self, experiment):
        self.experiment = experiment

    def __str__(self):
        var = json.dumps({"$type": NewExperiment.typeClass,
                          "experiment": self.experiment})
        return str(var)




class Action(Request):
    typeClass = "CommunicationLayerPython.Action"

    def __init__(self, experimentID, action):
        self.experimentID = experimentID
        self.action = action

    def __str__(self):
        var = json.dumps({"$type": Action.typeClass,
                          "experimentID": self.experimentID,
                          "action": self.action})
        return str(var)


class EndExperiment(Request):
    typeClass = "CommunicationLayerPython.EndExperiment"

    def __init__(self, experimentID):
        self.experimentID = experimentID

    def __str__(self):
        var = json.dumps({"$type": EndExperiment.typeClass,
                          "experimentID": self.experimentID})
        return str(var)


class ResetExperiment(Request):
    typeClass = "CommunicationLayerPython.ResetExperiment"

    def __init__(self, experimentID):
        self.experimentID = experimentID

    def __str__(self):
        var = json.dumps({"$type": ResetExperiment.typeClass,
                          "experimentID": self.experimentID})
        return str(var)


# Response


class InvalidRequest(Response):
    typeClass = "CommunicationLayerPython.InvalidRequest"

    def __init__(self, error):
        self.error = error

    def __str__(self):
        var = json.dumps({"$type": InvalidRequest.typeClass,
                          "error": self.error})
        return str(var)


class ExperimentResp(Response):
    typeClass = "CommunicationLayerPython.ExperimentResp"

    def __init__(self, experimentID, stateDim):
        self.experimentID = experimentID
        self.stateDim = stateDim

    def __str__(self):
        var = json.dumps({"$type": ExperimentResp.typeClass,
                          "experimentID": self.experimentID,
                          "stateDim": self.stateDim})
        return str(var)


class NewState(Response):
    typeClass = "CommunicationLayerPython.NewState"

    def __init__(self,state, actionsForNewState, regard):
        self.state = state
        self.actionsForNewState = ActionsForState.readfromstr(actionsForNewState)
        self.regard = Regard.readfromstr(regard)

    def __str__(self):
        var = json.dumps({"$type": NewState.typeClass,
                          "state": self.state,
                          "actionsForNewState": self.actionsForNewState.asJson(),
                          "regard": self.regard.asJson()})
        return str(var)
