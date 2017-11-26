package utils

/**
  * A [[OvoException]] is the exception manage in the service
  *
  * @param code       : Exception code
  * @param message  : Exception message
  * @param cause  : Possible exception cause o

  */
case class OvoException(
  code:Int,
  message:String,
  cause: Option[Throwable]) extends Throwable



/**
  * Cost Singleton object encapsulate possible error codes
  */
object ErrorCodes{

  val costParseError = 100
  val usageParseError = 101
  val tariffParseError = 102

}