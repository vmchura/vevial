import socket
import sys
import time

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

import os
import random


def main():
    host = "localhost"
    port = 8080
    print('# Creating socket')
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.connect((host, port))

        msg, _, _, _ = s.recvmsg(1024)

        m = Message.parseMessage(msg.decode("utf-8"))

        if isinstance(m, Welcome):
            print("Welcome received")
            print("Sending request for a new experiment")

            s.send((str(NewExperiment("SimpleExperiment")) + os.linesep).encode())

            msg, _, _, _ = s.recvmsg(1024)
            newExperiment = Message.parseMessage(msg.decode("utf-8"))

            if isinstance(newExperiment, ExperimentResp):
                actions = newExperiment.actionsForState.actions
                while actions > 0:
                    simpleAction = random.randint(1, actions)
                    action = Action(newExperiment.experimentID,simpleAction)
                    s.send((str(action) + os.linesep).encode())
                    msg, _, _, _ = s.recvmsg(1024)

                    m = Message.parseMessage(msg.decode("utf-8"))
                    if isinstance(m, Farewell):
                        print("Got an Farewell\n")
                        break
                    else:
                        if isinstance(m, NewState):
                            actions = m.actionsForNewState.actions
                        else:
                            raise AssertionError("is not a NewState")
                if actions <= 0:
                    s.send((str(EndExperiment(newExperiment.experimentID))+os.linesep).encode())

            else:
                raise AssertionError("is not a ExperimentResp "+msg)

        s.close()

    except socket.error:
        print('Failed to create socket')
        sys.exit()

    print('#Getting remote IP adders')


if __name__ == '__main__':
    main()
