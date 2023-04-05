package exercises06.e3_transformer

import exercises06.e3_transformer.Error.{InvalidId, InvalidName}

trait Transformer[A, B] {
  def toOption(a: A): Option[B]

  def toEither(a: A): Either[Error, B]
}

object TransformerInstances {
  implicit val transformer: Transformer[RawUser, User] = new Transformer[RawUser, User] {
    def toOption(a: RawUser): Option[User] = {
      for {
        id         <- a.id.toLongOption
        firstName  <- a.firstName.filter(_.nonEmpty)
        secondName <- a.secondName.filter(_.nonEmpty)
      } yield User(id, UserName(firstName, secondName, a.thirdName))
    }

    def toEither(a: RawUser): Either[Error, User] = {
      for {
        id         <- a.id.toLongOption.toRight(InvalidId)
        firstName  <- a.firstName.filter(_.nonEmpty).toRight(InvalidName)
        secondName <- a.secondName.filter(_.nonEmpty).toRight(InvalidName)
      } yield User(id, UserName(firstName, secondName, a.thirdName))
    }
  }
}

object TransformerSyntax {
  implicit class TransformerOps[A](val a: A) extends AnyVal {
    def transformToOption[B](implicit transformer: Transformer[A, B]): Option[B] =
      transformer.toOption(a)

    def transformToEither[B](implicit transformer: Transformer[A, B]): Either[Error, B] =
      transformer.toEither(a)
  }
}

object Examples {
  import TransformerInstances._
  import TransformerSyntax._

  RawUser("1234", Some(""), Some(""), None).transformToOption[User]
  RawUser("1234", Some(""), Some(""), None).transformToEither[User]
}
