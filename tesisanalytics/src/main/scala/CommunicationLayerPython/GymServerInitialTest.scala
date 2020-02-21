package CommunicationLayerPython

import java.io.{BufferedReader, InputStreamReader, PrintWriter}
import java.net._
import java.util.UUID

import com.typesafe.scalalogging.Logger
import upickle.default._

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.Success
import scala.language.postfixOps
class GymServerInitialTest(portNumber: Int) {
  object WatchDog{
    val alertTime = 5 seconds
  }
  def runWithTimeout[T](f: => T, duration: Duration = WatchDog.alertTime) : Option[T] = {

    try {
      Await.result(Future(Some(f)), duration).asInstanceOf[Option[T]]
    }catch{
      case e: Throwable => None
    }
  }

  val gymVersion = "0.1"


  val logger = Logger[GymServerInitialTest]
  def start(): Unit = {



        val messagesToSend = mutable.Queue (
          NewExperiment("experiment"),
          Action(UUID.randomUUID(), 1),
          InvalidRequest("newError"),
          ExperimentResp(UUID.randomUUID(),1),
          NewState(1,ActionsForState(5), Regard(1.2))

        )



        val serverSocket: ServerSocket = new ServerSocket(portNumber)

        try {

          logger.info(s"Server initiated on port: $portNumber")
          val clientSocketOpt = runWithTimeout(serverSocket.accept())
          if (clientSocketOpt.isEmpty)
            throw new IllegalStateException("No client")
          val clientSocket = clientSocketOpt.get
          logger.info(s"Client accepted")
          val out = new PrintWriter(clientSocket.getOutputStream, true)
          val in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream))

          logger.info("Sending Welcome")
          out.println(write(Welcome(gymVersion)))

          var watchDog = WatchDog.alertTime.fromNow

          def touchDog(): Unit = watchDog = watchDog + WatchDog.alertTime

          while(messagesToSend.nonEmpty) {

            val messageToSend = messagesToSend.dequeue()
            val charToSend = messageToSend match {
              case m: NewExperiment => write(m)
              case m: Action => write(m)
              case m: InvalidRequest => write(m)
              case m: ExperimentResp => write(m)
              case m: NewState => write(m)

            }

            //logger.info("Sending message to python: "+charToSend)
            out.println(charToSend)
            var lineRead: Boolean = false
            while (watchDog.hasTimeLeft() && !lineRead) {

              val futLine = Future.successful(in.readLine())

              while(watchDog.hasTimeLeft() && !futLine.isCompleted){
                Thread.sleep(10)
              }

              futLine.value match {
                case Some(Success(line)) =>
                  if (line != null) {
                    touchDog()
                    /*
                    logger.info("tyring to parse")
                    logger.info(line)

                     */
                    val elementRead: Message = messageToSend match {
                      case _: NewExperiment => read[NewExperiment](line)
                      case _: Action => read[Action](line)
                      case _: InvalidRequest => read[InvalidRequest](line)
                      case _: ExperimentResp => read[ExperimentResp](line)
                      case _: NewState => read[NewState](line)
                      case _ => throw new IllegalArgumentException()
                    }
                    if(elementRead == messageToSend){
                      logger.info("Messages are the same")
                    }else{
                      logger.info("Messages are not the same")
                      logger.info("Sent: "+messageToSend)
                      logger.info("Got: "+elementRead)
                    }

                    lineRead = true

                  }
                case _ => logger.info("error")

              }


              //Thread.sleep((1 second).toMillis)
            }
            if(!lineRead)
              throw  new TimeoutException("cant get message of python")
          }

          out.println( write(Farewell("bye")))


        } catch {
          case e: Exception => logger.info("Ended by error: " + e.toString)
        }
        finally {

          serverSocket.close()
        }
  }



}
