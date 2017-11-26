package actor


import actor.Message._
import akka.actor._
import domain.{Cost, Tariff, Usage}
import utils.Logger

/**
  * A [[CostActor]] centralizes the different
  * events of the system and returns the aswer to the command.
  */
class CostActor extends Actor with Logger {
  val patternCost = "cost .*".r
  val patternUsage = "usage .*".r
  val patternExit = "exit".r
  val tariffs = Tariff.parseTariffFromFile()
  val VAT = 0.05

  override def receive: Receive = {
    case Command(message) =>
      message match {
        case patternCost() =>
          logger.info(s"[COST-ACTOR] Cost message $message")
          Cost.parseFromString(message).toOption.fold {
            sender() ! NotValidFormat
          } { cost =>
            val response: List[CostTariffResponse] =
              (cost.gasUsage, cost.powerUsage) match {
                case (0.0, 0.0) => {
                  println("Error, cost empty can be processed")
                  List(CostTariffResponse("Empty response", None, None))
                }
                case (0.0, powerUsage) =>
                  tariffs.map(tariff => {
                    val powerCost =
                      tariff.rates.power.map {
                        powerC =>
                          val response =
                            ((powerC * powerUsage) + tariff.standing_charge) * (1 + VAT)
                          BigDecimal(response)
                            .setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
                      }
                    CostTariffResponse(tariff.tariff, powerCost, None)
                  })
                case (gasUsage, 0.0) =>
                  tariffs.map(tariff => {
                    val gasCost =
                      tariff.rates.gas.map {
                        gasC =>
                          val response =
                            ((gasC * gasUsage) + tariff.standing_charge) * (1 + VAT)
                          BigDecimal(response)
                            .setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
                      }
                    CostTariffResponse(tariff.tariff, None, gasCost)
                  })
                case (gasUsage, powerUsage) =>
                  tariffs.map(tariff => {
                    val powerCost =
                      tariff.rates.power.map {
                        powerC =>
                          val response =
                            ((powerC * powerUsage) + tariff.standing_charge) * (1 + VAT)
                          BigDecimal(response)
                            .setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
                      }
                    val gasCost =
                      tariff.rates.gas.map {
                        gasC =>
                          val response =
                            ((gasC * gasUsage) + tariff.standing_charge) * (1 + VAT)
                          BigDecimal(response)
                            .setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
                      }
                    CostTariffResponse(tariff.tariff, powerCost, gasCost)

                  })
              }
            sender() ! CostResponse(response)
          }
        case patternUsage() =>
          logger.info(s"[COST-ACTOR] Usage message $message")

          Usage.parseFromString(message).toOption.fold({
            sender() ! NotValidFormat
          }) { usage =>
            tariffs.find(_.tariff == usage.tariff_name).fold {
              sender() ! TariffNotFound
            } {
              tariff =>
                usage.fuel_type match {
                  case v if (v == "GAS") =>
                      tariff.rates.gas.fold {
                        sender() ! FuelNotDefinedInTariff
                      } { gasC =>
                       val response =
                         ((gasC * usage.target_monthly_spent * 12) * (1 + VAT))
                        val gastCons = BigDecimal(response)
                          .setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
                        sender() ! UsageResponse(gastCons)
                      }
                  case v if (v == "POWER")  =>
                      tariff.rates.power.fold {
                        sender() ! FuelNotDefinedInTariff
                      } { powerC =>
                        val response =
                          ((powerC * usage.target_monthly_spent * 12) * (1 + VAT))
                        val powerCons = BigDecimal(response)
                          .setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
                        sender() ! UsageResponse(powerCons)
                      }
                  case _ => sender() ! FuelTypeNotValid
                }
            }
          }
        case patternExit() =>
          logger.info("[COST-ACTOR] Exit message")
          sender() ! Exit
        case _ => sender() ! NotValidCommand
      }
  }
}

object Message {

  case class CostResponse(costs: List[CostTariffResponse])

  case object FuelNotDefinedInTariff

  case object EmptyCost

  case class CostTariffResponse(
    name_tariff: String,
    cost_gas: Option[Double],
    cost_fuel: Option[Double])

  case class UsageResponse(
    annualComsuption: Double)

  case object TariffNotFound

  case object FuelTypeNotValid

  case object NotValidCommand

  case object NotValidFormat

  case object Exit

  case class Command(msg: String)

}
