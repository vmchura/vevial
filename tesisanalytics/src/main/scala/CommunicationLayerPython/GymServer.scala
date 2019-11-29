package CommunicationLayerPython
import java.io.{BufferedReader, InputStreamReader, PrintWriter}
import java.net._
import java.util.UUID

import upickle.default._
import com.typesafe.scalalogging.Logger
import gym.SimpleExperiment

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.Success
class GymServer(portNumber: Int) {
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


  val logger = Logger[GymServer]
  val experimentsCreated = mutable.Map.empty[UUID,Experiment]
  def start(): Unit = {

        logger.info("Starting server")




        val serverSocket: ServerSocket = new ServerSocket(portNumber)

        try {

          logger.info(s"Server started on port: $portNumber")
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



          while (watchDog.hasTimeLeft()) {

            val futLine = Future.successful(in.readLine())

            while(watchDog.hasTimeLeft() && !futLine.isCompleted){
              Thread.sleep(10)
            }

            futLine.value match {
              case Some(Success(line)) =>
                if (line != null) {
                  touchDog()
                  val u = line.indexOf("CommunicationLayerPython")
                  if(u>0){
                    val endTag = line.indexOf('\"',u)
                    val iniTag = line.indexOf('.',u)
                    val tag = line.substring(iniTag+1,endTag)

                    val elementReadOpt: Option[Request] =
                      try {
                        tag match {
                          case "NewExperiment" => Some(read[NewExperiment](line))
                          case "Action" => Some(read[Action](line))
                          case "EndExperiment" => Some(read[EndExperiment](line))
                          case "ResetExperiment" => Some(read[ResetExperiment](line))
                          case _ => None
                        }
                      } catch {
                        case e: Throwable => {
                          logger.error("cant parse "+line)
                          None
                        }
                      }

                    val responseOpt: Option[Response] = elementReadOpt match {
                      case Some(NewExperiment("SimpleExperiment")) => {
                        val id = UUID.randomUUID()
                        val exp = SimpleExperiment(id)
                        experimentsCreated += id -> exp
                        Some(ExperimentResp(id,exp.dimState))
                      }
                      case Some(Action(id,action)) => {
                        experimentsCreated.get(id).map{ exp =>
                          if(action <= exp.actions && action >= 1){
                            val expUpdated = exp.update(action)
                            experimentsCreated(id) = expUpdated
                            Some(NewState(expUpdated.state(0),ActionsForState(expUpdated.actions),Regard(exp.regard(action))))

                          }else{
                            Some(InvalidRequest("Invalid action"))
                          }
                        }.getOrElse(Some(InvalidRequest("Invalid ID action")))
                      }
                      case Some(EndExperiment(experimentID)) => {
                        experimentsCreated -= experimentID
                        logger.info("Experiment ended")
                        None
                      }
                      case Some(ResetExperiment(experimentID)) => {
                        experimentsCreated.get(experimentID).map { exp =>
                          val newExp = exp.reset
                          experimentsCreated(experimentID) = newExp
                          Some(NewState(newExp.state(0),ActionsForState(newExp.actions),Regard(0)))


                        }.getOrElse(Some(InvalidRequest("Invalid ID reset")))
                      }
                      case None => Some(InvalidRequest("Invalid action"))
                    }

                    responseOpt.map{ response =>

                      val toSend = response match {
                        case m: ExperimentResp => write(m)
                        case m: NewState => write(m)
                        case m: InvalidRequest => write(m)
                        case _ => throw new IllegalStateException("is not a valid resposne")
                      }

                      out.println(toSend)
                    }




                  }else{
                    throw new IllegalArgumentException("not containing: CommunicationLayerPython")
                  }



                }
              case _ => logger.info("error")

            }


            //Thread.sleep((1 second).toMillis)
          }



          //out.println( write(Farewell("bye")))


        } catch {
          case e: Exception => logger.info("Ended by error: " + e.toString)
        }
        finally {

          serverSocket.close()
        }
  }



}
