import gym
import sys

if "../" not in sys.path:
    sys.path.append("../")
from messages import Message
from messages import Farewell
from messages import Welcome
from messages import ExperimentResp
from messages import Action
from messages import NewState
from messages import NewExperiment
from messages import EndExperiment
from messages import ResetExperiment

import os
import random
import socket


class MyEnv(object):
    def __init__(self):
        self.env = gym.make('CartPole-v0')

    def getStateDim(self):
        return self.env.observation_space.shape[0]

    def getActionCount(self):
        return self.env.action_space.n

    def reset(self):
        return self.env.reset()

    def step(self, action):
        # observation, reward, done, _
        return self.env.step(action)


class LanEnv(object):
    def __init__(self):
        host = "192.168.0.6"
        port = 8082
        print('# Creating socket')
        try:
            s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            s.connect((host, port))
            msg, _, _, _ = s.recvmsg(1024)

            m = Message.parseMessage(msg.decode("utf-8"))

            if isinstance(m, Welcome):
                print("Welcome received")
                s.send((str(NewExperiment("SimpleExperiment")) + os.linesep).encode())

                msg, _, _, _ = s.recvmsg(1024)
                newExperiment = Message.parseMessage(msg.decode("utf-8"))
                if isinstance(newExperiment, ExperimentResp):
                    print("new Experiment")
                    self.s = s
                    self.newExperiment = newExperiment
                else:
                    print("No new experiment")
            else:
                print("No welcome")

        except socket.error:
            print("Error on socket")

    def getStateDim(self):
        return self.newExperiment.stateDim

    def getActionCount(self):
        return 3

    def reset(self):
        resetCmd = ResetExperiment(self.newExperiment.experimentID)
        self.s.sendall((str(resetCmd)+os.linesep).encode())
        msg, _, _, _ = self.s.recvmsg(1024)
        m = Message.parseMessage(msg.decode("utf-8"))
        if isinstance(m,NewState):
            return m.state
        else:
            raise AssertionError("Not a NewState "+str(m))

    def step(self,action):
        actionCommand = Action(self.newExperiment.experimentID,action)
        self.s.sendall((str(actionCommand)+os.linesep).encode())
        msg, _, _, _ = self.s.recvmsg(1024)
        m = Message.parseMessage(msg.decode("utf-8"))
        if isinstance(m,NewState):
            return m.state, m.regard.value, m.actionsForNewState.actions == 0
        else:
            raise AssertionError("Not a NewState "+str(m))
