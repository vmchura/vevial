import socket
import sys
import time
if "../" not in sys.path:
    sys.path.append("../")
from messages import Message
from messages import Farewell
from messages import Welcome
import os

def main():

    host = "localhost"
    port = 8080
    print('# Creating socket')
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.connect((host, port))

        msg, _, _, _ = s.recvmsg(1024)

        m = Message.parseMessage(msg.decode("utf-8"))

        if isinstance(m,Welcome):
            print("Welcome")
            while True:
                msg, _, _, _ = s.recvmsg(1024)

                m = Message.parseMessage(msg.decode("utf-8"))
                if isinstance(m, Farewell):
                    print("Got an Farewell\n")
                    break
                else:
                    #print("Got an "+str(m)+"\n")
                    #print("Sending it back")
                    time.sleep(2)
                    s.send((str(m)+os.linesep).encode())


        s.close()

    except socket.error:
        print('Failed to create socket')
        sys.exit()

    print('#Getting remote IP adders')


if __name__ == '__main__':
    main()
