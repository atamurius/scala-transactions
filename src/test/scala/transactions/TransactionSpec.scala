package transactions

import cats.effect.IO
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

    val transaction1 = ActionChain[Int, Int](
      set("x", 1),
      oldX => ActionChain[Int, Int](
        set("old_x", 42),
        _ => set("y", oldX + 5)
      )
    )

    transaction1.compile.unsafeRunSync()
    values shouldBe Map(
      "x" -> 1,
      "y" -> 5
    )

    val transaction2 = ActionChain[Int, Int](
      set("x", 1),
      oldX => ActionChain[Int, Int](
        set("unknown", 42),
        _ => set("y", oldX + 5)
      )
    )

    transaction2.compile.attempt.unsafeRunSync() shouldBe a[Left[Throwable, Int]]
    values shouldBe Map(
      "x" -> 1,
      "y" -> 5
    )

  }
}
