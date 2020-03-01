package transactions

import cats.effect._
import cats.implicits._

import scala.Function.const


sealed trait Transaction[T] {

  private def toIO[R](nextActions: T => IO[R]): IO[R] = this match {

    case Action(perform, commit, compensate) =>
      perform.flatMap { t =>
        nextActions(t).redeemWith(
          bind = commit(t).attempt >> IO.pure(_),
          recover = compensate(t).attempt >> IO.raiseError(_)
        )
      }

    case ActionChain(first, next) =>
      first.toIO { a =>
        next(a).toIO(nextActions)
      }
  }

  def compile: IO[T] = toIO(IO.pure)
}


final case class Action[T](
  perform:    IO[T],
  commit:     T => IO[Unit] = const(IO.unit)(_: T),
  compensate: T => IO[Unit] = const(IO.unit)(_: T)
) extends Transaction[T]


final case class ActionChain[A, B](
  first:  Transaction[A],
  next:   A => Transaction[B]
) extends Transaction[B]


