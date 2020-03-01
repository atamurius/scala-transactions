package transactions

import cats._
import cats.effect._
import cats.implicits._

import scala.Function.const


sealed trait Transaction[T] {

  private def compile[R](continue: T => IO[R]): IO[R] = this match {

    case Action(perform, commit, compensate) =>
      perform.flatMap { t =>
        continue(t).redeemWith(
          bind = commit(t).attempt >> IO.pure(_),
          recover = compensate(t).attempt >> IO.raiseError(_)
        )
      }

    case ActionChain(first, next) =>
      first.compile { a =>
        next(a).compile(continue)
      }
  }

  def compile: IO[T] = compile(IO.pure)

  def flatMap[R](f: T => Transaction[R]): Transaction[R] = ActionChain(this, f)

  def map[R](f: T => R): Transaction[R] = flatMap { t => Action(IO(f(t))) }
}

object Transaction {

  implicit object MonadInstance extends Monad[Transaction] {
    override def pure[A](x: A): Transaction[A] = Action(IO.pure(x))

    override def flatMap[A, B](fa: Transaction[A])(f: A => Transaction[B]): Transaction[B] = fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: A => Transaction[Either[A, B]]): Transaction[B] = f(a).flatMap {
      case Left(a) => tailRecM(a)(f)
      case Right(b) => pure(b)
    }
  }
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


