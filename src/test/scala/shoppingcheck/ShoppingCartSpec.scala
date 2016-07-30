package shoppingcheck

import org.scalacheck.Gen._
import org.scalacheck.Shrink

import scala.util.Random._

class ShoppingCartSpec extends BaseSpec {
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  "totalCents" must {
    "add unit prices per item on the list, regardless of order" in {
      forAll(apples, oranges) { (as, os) =>
        val ps = shuffle(as ++ os)
        sc.totalCents(ps) shouldBe as.length * 60 + os.length * 25
      }
    }
  }

  // Test Setup
  val sc = new ShoppingCart
  val apple = "apple"
  val orange = "orange"

  val apples = listOf(delay(apple))
  val oranges = listOf(delay(orange))
}
