package domain

import utils.{Logger, OvoException}
import utils.ErrorCodes._

import scala.util.Try

/**
  * A [[Cost()]] represents costs of tariff
  *
  * @param powerUsage       :  power usage
  * @param gasUsage  :  gas usage

  */
case class Cost(
  powerUsage: Double,
  gasUsage: Double)

/**
  * Cost Singleton object encapsulate methods for charging cost from external source
  */
object Cost extends Logger{

  def parseFromString(stringCost: String):Try[Cost] ={
    Try{
      val tokens = stringCost.trim.replaceAll(" +", " ").split(" ")
      Cost(
        tokens.tail.head.toDouble,
        tokens.tail.tail.head.toDouble
      )
    }recover{
      case e:Exception =>
        logger.error(s"Error when parsing string of cost $stringCost")
        throw OvoException(
          costParseError,
          "Error when parsing string of cost $stringCost",
          Some(e))
    }
  }
}

