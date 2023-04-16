package exercises05.parser

import exercises05.either.EitherCombinators._
import Error._

import scala.util.matching.Regex

object Examples {

  /**
    * если rawUser.firstName или rawUser.secondName == None, то функция должна вернуть None
    * если rawUser.passport == None или rawUser.thirdName == None, то и в результирующем User эти поля == None
    * passport должен быть передан в формате 1234 567890, если не так, то функция должна вернуть None
    * если rawUser.id не парсится в Long то функция должна вернуть None
    * если rawUser.banned, то вернуть None
    * используйте for-comprehension
    */
  private val regex: Regex = ("(\\d{4}) (\\d{6})".r)
  def transformToOption(rawUser: RawUser): Option[User] = {

    for {
      userId <- rawUser.id.toLongOption
      userPassport <- rawUser.passport match {
        case Some(value) =>
          value.trim match {
            case regex(ser, num) => Some(Some(Passport(ser.toLong, num.toLong)))
            case _               => None
          }
        case None => Some(None)
      }
      userFirstName  <- rawUser.firstName
      userSecondName <- rawUser.secondName
      if (!rawUser.banned)

    } yield User(userId, UserName(userFirstName, userSecondName, rawUser.thirdName), userPassport)
  }

  /**
    * если rawUser.firstName или rawUser.secondName == None, то функция должна вернуть Left(InvalidName)
    * если rawUser.passport == None или rawUser.thirdName == None, то и в результирующем User эти поля == None
    * passport должен быть передан в формате 1234 567890, если не так, то функция должна вернуть Left(InvalidPassport)
    * если rawUser.id не парсится в Long то функция должна вернуть Left(InvalidId)
    * если rawUser.banned, то вернуть Left(Banned)
    * у ошибок есть приоритет:
    * 1. Banned
    * 2. InvalidId
    * 3. InvalidName
    * 4. InvalidPassport
    * используйте for-comprehension
    * но для того, чтобы for-comprehension заработал надо реализовать map и flatMap в Either
    */
  def transformToEither(rawUser: RawUser): Either[Error, User] = {
    for {
      _              <- if (rawUser.banned) Left(Banned) else Right(true)
      userId         <- Either.fromOption(rawUser.id.toLongOption)(InvalidId)
      userFirstName  <- Either.fromOption(rawUser.firstName)(InvalidName)
      userSecondName <- Either.fromOption(rawUser.secondName)(InvalidName)
      userPassport <- Either.fromOption(rawUser.passport match {
        case Some(value) =>
          value.trim match {
            case regex(ser, num) => Some(Some(Passport(ser.toLong, num.toLong)))
            case _               => None
          }
        case None => Some(None)
      })(InvalidPassport)

    } yield User(userId, UserName(userFirstName, userSecondName, rawUser.thirdName), userPassport)
  }
}
