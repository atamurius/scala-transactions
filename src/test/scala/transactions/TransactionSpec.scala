package transactions

import cats.effect.IO
import cats.implicits._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TransactionSpec extends AnyWordSpec with Matchers {

  "Transaction sequence should behave like transaction" in {
    var values = Map(
      "x" -> 0,
      "y" -> 0
    )

    def set(name: String, newValue: Int) = Action[Int](
      perform = IO {
        println(s"$name <- $newValue")
        val oldValue = values(name)
        values += s"old_$name" -> oldValue
        values += name -> newValue
        oldValue
      },
      commit = old => IO {
        println("commit")
        values -= s"old_$name"
      },
      compensate = old => IO {
        println("compensate")
        values -= s"old_$name"
        values += name -> old
      }
    )

    val transaction1 = for {
      oldX  <- set("x", 1)
      _     <- set("old_x", 42)
      _     <- set("y", oldX + 5)
    } yield ()

    transaction1.compile.unsafeRunSync()
    values shouldBe Map(
      "x" -> 1,
      "y" -> 5
    )

    val transaction2 = for {
      oldX <- set("x", 1)
      _ <- set("unknown", 42)
      _ <- set("y", oldX + 5)
    } yield ()

    transaction2.compile.attempt.unsafeRunSync() shouldBe a[Left[Throwable, Unit]]
    values shouldBe Map(
      "x" -> 1,
      "y" -> 5
    )

  }
}
