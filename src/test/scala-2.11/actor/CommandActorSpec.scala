package actor

import scala.util.{Random, Try}
import org.scalatest.WordSpecLike
import akka.actor.{ActorSystem, Props}
import akka.testkit.{DefaultTimeout, ImplicitSender, TestActors, TestKit}
import akka.pattern.ask
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mock.MockitoSugar
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import akka.util.Timeout
import org.joda.time.DateTime
import org.scalatest.time.{Millis, Seconds, Span}
import actor.Message._
import actor.Message.Exit

class CommandActorSpec  extends TestKit(ActorSystem("ComputingActorSpec"))
  with Matchers
  with MockitoSugar
  with WordSpecLike
  with ScalaFutures
  with BeforeAndAfterAll {

  implicit val timeout: Timeout = 20 seconds

  implicit val defaultPatience =
    PatienceConfig(
      timeout = Span(60, Seconds),
      interval = Span(5000, Millis))

  val commandActor =
    system.actorOf(Props(new CostActor()), "testActor")

  val nowTime = DateTime.now()
  val message1 = "exit "

  "Command actor should answer exit if command --> <$message1>" in {
    val fResult = for {
      result <- (commandActor ? Command("exit")).map{
        case Exit => true
        case _ => false
      }
    } yield {
      result
    }
    whenReady(fResult) {
      result =>
        result shouldBe true
    }
  }
  val message2 = "clkrevhrle "
  s"Command actor should answer not valid command command --> <$message2>" in {
    val fResult = for {
      result <- (commandActor ? Command("clkrevhrle  ")).map{
        case NotValidCommand => true
        case _ => false
      }
    } yield {
     result
    }
    whenReady(fResult) {
      result =>
        result shouldBe true
    }
  }
  val message3 = "cost hello ovo !  "
  s"Command actor should answer not valid format for cost command --> <$message3>" in {
    val fResult = for {
      result <- (commandActor ? Command("cost hello ovo !  ")).map{
        case NotValidFormat => true
        case _ => false
      }
    } yield {
      result
    }
    whenReady(fResult) {
      result =>
        result shouldBe true
    }
  }
  val message4 = "usage hello 111 o"
  s"Command actor should answer not valid format for cost usage command --> <$message4>" in {
    val fResult = for {
      result <- (commandActor ? Command("usage hello 111 o")).map{
        case NotValidFormat => true
        case _ => false
      }
    } yield {
      result
    }
    whenReady(fResult) {
      result =>
        result shouldBe true
    }
  }

  val message5 = "usage better-energyy  111 2"
  s"Command actor should not found tariff for cost usage command --> <$message5>"in {
    val fResult = for {
      result <- (commandActor ? Command(message5)).map{
        case TariffNotFound => true
        case _ => false
      }
    } yield {
      result
    }
    whenReady(fResult) {
      result =>
        result shouldBe true
    }
  }

  val message6 = "usage better-energy  GASSS 10"
  s"Command actor should answer fuel type not valid  for command --> <$message6>"in {
    val fResult = for {
      result <- (commandActor ? Command(message6)).map{
        case FuelTypeNotValid => true
        case _ => false
      }
    } yield {
      result
  }
  whenReady(fResult) {
      result =>
        result shouldBe true
    }
  }

  val message7 = "usage better-energy  GAS 0"
  s"Command actor should answer the cost 0  for command --> <$message7>"in {
    val fResult = for {
      result <- (commandActor ? Command(message7)).map{
        case UsageResponse(annualCons) => annualCons == 0.0
        case _ => false
      }
    } yield {
      result
    }
    whenReady(fResult) {
      result =>
        result shouldBe true
    }
  }

  val message8 = "usage better-energy  GAS 1"
  s"Command actor should answer the cost 0.36 for command --> <$message8>"in {
    val fResult = for {
      result <- (commandActor ? Command(message8)).map{
        case UsageResponse(annualCons) => annualCons == 0.36
        case _ => false
      }
    } yield {
      result
    }
    whenReady(fResult) {
      result =>
        result shouldBe true
    }
  }

  val message9 = "usage greener-energy  GAS 10"
  s"Command actor answer Not fuel defined for tariff for command --> <$message9>"in {
    val fResult = for {
      result <- (commandActor ? Command(message9)).map{
        case FuelNotDefinedInTariff => true
        case _ => false
      }
    } yield {
      result
    }
    whenReady(fResult) {
      result =>
        result shouldBe true
    }
  }

  val message10 = "cost 20 10 "
  s"Command actor should answer a costResponse  for command --> <$message10>"in {
    val fResult = for {
      result <- (commandActor ? Command(message10)).map{
        case CostResponse(cost) => true
        case _ => false
      }
    } yield {
      result
    }
    whenReady(fResult) {
      result =>
        result shouldBe true
    }
  }

}
