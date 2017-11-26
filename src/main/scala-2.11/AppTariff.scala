

import akka.actor.{ActorSystem, Props}
import akka.io.IO
import akka.pattern.ask
import akka.util.Timeout
import spray.can.Http

import scala.concurrent.duration._
import actor.CostActor
import actor.Message._
import utils.Logger

import scala.concurrent.ExecutionContext.Implicits.global
import scala.annotation.tailrec
import scala.util.Try
object AppTariff extends App with Logger{

  implicit val system = ActorSystem("command-service")
  implicit val timeout = Timeout(5.seconds)
  val actorCommand = system.actorOf(Props(new CostActor))

  def printInstruccions = {
    println("###################################################################################")
    println("###TARIFF COMMAND : Use of console                                 ###############")
    println("###---> 1 : cost <POWER_USAGE> <GAS_USAGE>                         ###############")
    println("###---> 2 : usage <TARIFF_NAME> <FUEL_TYPE> <TARGET_MONTHLY_SPEND> ###############")
    println("###---> 3 : exit                                                   ###############")
    println("###################################################################################")

  }
  def commandLoop():Unit = {
    logger.info("###---> Introduce  a command:  ")
    val command = scala.io.StdIn.readLine()
    (actorCommand ? Command(command)).map{
      case response:CostResponse =>
        response.costs.foreach(
          costRes => {
            logger.info(s"###---> Tariff ${costRes.name_tariff} ,GAS : ${costRes.cost_gas.getOrElse(0)} ###############")
            logger.info(s"###---> POWER ${costRes.cost_fuel.getOrElse(0)} ###############")
          })
        commandLoop

      case EmptyCost =>
        logger.info("###---> No costs specified                                    ###############")
      case NotValidCommand =>
        logger.info("###---> Not valid command!!                                     ###############")
        commandLoop()
      case NotValidFormat =>
        logger.info("###---> Not valid format!!                                     ###############")
        commandLoop()
      case response:UsageResponse =>
        logger.info(s"###---> Annual consuptiom : ${response.annualComsuption}  £   ###############")
        commandLoop
      case FuelTypeNotValid =>
        logger.info("###--->Fuel type is not valid                                  ###############")
        commandLoop
      case TariffNotFound =>
        logger.info("###--->Tariff specified is not found                            ###############")
        commandLoop
      case FuelNotDefinedInTariff =>
        logger.info("###--->Fuel type  not defined in tariff specified                ###############")
        commandLoop
      case Exit =>
        logger.info("###---> Closing console...                                     ###############")
    }
  }
  printInstruccions
  commandLoop


}
