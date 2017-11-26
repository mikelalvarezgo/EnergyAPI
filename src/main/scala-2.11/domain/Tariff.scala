package domain
import scala.io.Source
import spray.json._
import DefaultJsonProtocol._

import scala.util.parsing.json.JSON

/**
  * A [[Rate]] represents costs of tariff
  *
  * @param power       : Possible power cost
  * @param gas  : Possible gas cost

  */
case class Rate(
  power:Option[Double],
  gas: Option[Double])

/**
  * A [[Tariff]] represents a energy tariff
  *
  * @param tariff       : Identifier of tariff
  * @param rates  : Rates of tariff
  * @param standing_charge : Standing charge of the tariff

  */

case class Tariff(
  tariff: String,
  rates: Rate,
  standing_charge:Double)

/**
  * Tariff Singleton object encapsulate methods for charging cost from external source
  */
object Tariff {

  implicit  val jfRate :JsonFormat[Rate] = jsonFormat2(Rate.apply)
  implicit  val jfTariff :JsonFormat[Tariff] = jsonFormat3(Tariff.apply)

  def parseTariffFromFile():List[Tariff]={

    val file = Source.fromURL(getClass.getResource("/prices.json"))
    val json = ""+file.getLines().toList.fold("")((text,line) => text+line)+""
    val jsonArr = json.stripMargin.parseJson.asInstanceOf[JsArray]
    jsonArr.elements.map{
      jfTariff.read(_)
    }.toList
  }
}

