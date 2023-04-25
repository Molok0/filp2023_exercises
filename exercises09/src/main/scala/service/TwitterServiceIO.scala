package service

import cats.effect.IO
import cats.syntax.all._
import service.domain.GetTweetResponse.{Found, NotFound}
import service.domain._
import twitter.domain.TwitterError._
import twitter.TwitterApi
import twitter.domain._

import scala.util.{Failure, Success}

// Воспользуйтесь синтаксисом map, recover, traverse из cats.syntax.all_
class TwitterServiceIO(api: TwitterApi) extends TwitterService[IO] {
  def tweet(user: User, text: String): IO[TweetId] = IO.async_(cb => api.tweet(user, text)(x => cb(x.toEither)))

  def like(user: User, tweetId: TweetId): IO[Unit] =
    IO.async_((cb: Either[Throwable, Unit] => Unit) => api.like(user, tweetId)(x => cb(x.toEither)))
      .recover({ case LikeAlreadyExistError => () })

  def unlike(user: User, tweetId: TweetId): IO[Unit] =
    IO.async_((cb: Either[Throwable, Unit] => Unit) => api.unlike(user, tweetId)(x => cb(x.toEither)))
      .recover({
        case LikeNotExistError => ()
      })

  def getTweet(tweetId: TweetId): IO[GetTweetResponse] = {
    IO.async_((cb: Either[Throwable, GetTweetResponse] => Unit) =>
        api.get(tweetId)(x =>
          cb((x match {
            case Failure(_)     => Success(NotFound(tweetId))
            case Success(value) => Success(Found(value))
          }).toEither)
        )
      )
      .recover(PartialFunction.empty)
  }

  def getTweets(ids: List[TweetId]): IO[GetTweetsResponse] = {
    ids
      .traverse(getTweet)
      .map(list =>
        list.foldLeft(GetTweetsResponse(Set.empty[TweetId], Set.empty[TweetInfo])) { (tweets, response) =>
          response match {
            case Found(v)    => GetTweetsResponse(tweets.notFound, tweets.found + v)
            case NotFound(v) => GetTweetsResponse(tweets.notFound + v, tweets.found)
          }
        }
      )
  }

}
