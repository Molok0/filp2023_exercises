package competition

import cats.syntax.all._
import cats.effect.{IO, IOApp}
import competition.domain.ScenarioError.TopAuthorNotFound
import service.TwitterService
import twitter.domain._

/**
  * Конкурс! Кто наберет больше лайков под своим постом - тот победил
  *
  * Каждый пользовать постит твит "${user.id} will win!", и его фолловеры его лайкают
  * юзеры постят твиты параллельно, и так же параллельно их лайкают фолловеры
  *
  * Но случилась беда: пользователь с именем bot нарушил правила конкурса, и все его лайки надо удалить
  *
  * В конце надо вывести победителя
  * Если победителей несколько, то того, у которого твит был раньше
  * Если победителей нет, то вернуть ошибку TopAuthorNotFound
  *
  * используйте методы
  * CompetitionMethods.unlikeAll
  * CompetitionMethods.topAuthor
  */
class IOCompetition(service: TwitterService[IO], methods: CompetitionMethods[IO]) extends Competition[IO] {
  def winner(users: List[User], followers: Map[User, List[User]], botUser: User): IO[User] =
    for {
      tweetIds <- users.parTraverse(user =>
        service
          .tweet(user, s"${user.id} will win!")
          .flatMap(id => followers(user).parTraverse(service.like(_, id)).map(_ => id))
      )
      _           <- methods.unlikeAll(botUser, tweetIds)
      maybeWinner <- methods.topAuthor(tweetIds)
      winner      <- IO.fromOption(maybeWinner)(TopAuthorNotFound)
    } yield winner

}

object IOCompetitionRun extends IOApp {
  import cats.effect.ExitCode
  import scala.util.Random
  import twitter.{LocalTwitterApi, TwitterApi}
  import _root_.service.TwitterServiceIO

  val api: TwitterApi = new LocalTwitterApi(Iterator.continually((Random.nextDouble() * 1000).toInt))

  val service: TwitterService[IO] = new TwitterServiceIO(api)

  val methods: CompetitionMethods[IO] = new CompetitionMethods[IO](service)

  val oleg: User   = User("oleg")
  val ivan: User   = User("ivan")
  val marya: User  = User("marya")
  val gustav: User = User("gustav")
  val bot: User    = User("bot")

  val users: List[User] = List(oleg, ivan, marya, gustav, bot)

  val followers: Map[User, List[User]] = Map(
    oleg   -> List(ivan, bot),
    ivan   -> List(oleg, gustav),
    marya  -> List(oleg, ivan, gustav, bot),
    gustav -> List(oleg, ivan, marya),
    bot    -> List(bot)
  )

  val competition: Competition[IO] = new IOCompetition(service, methods)

  def run(args: List[String]): IO[ExitCode] =
    for {
      winner <- competition.winner(users, followers, bot)
      _      <- IO.delay(println(winner))
    } yield ExitCode.Success
}
