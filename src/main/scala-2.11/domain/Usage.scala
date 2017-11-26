package domain

import utils.ErrorCodes.usageParseError
import utils.OvoException

import scala.util.Try
import utils.Logger
case class Usage(
  tariff_name: String,
  fuel_type: String,
  target_monthly_spent: Double)

/**
  * Usage Singleton object encapsulate methods for charging cost from external source
  */

object Usage extends Logger{

  def parseFromString(stringUsage: String):Try[Usage] ={
    Try{
      val tokens = stringUsage.trim.replaceAll(" +", " ").split(" ")
      Usage(
        tokens.tail.head,
        tokens.tail.tail.head,
        tokens.tail.tail.tail.head.toDouble)

    }recover{
      case e:Exception =>
        logger.error(s"Error when parsing string of usage $stringUsage")
        throw OvoException(
          usageParseError,
          s"Error when parsing string of usage $stringUsage",
          Some(e))
    }
  }

}
