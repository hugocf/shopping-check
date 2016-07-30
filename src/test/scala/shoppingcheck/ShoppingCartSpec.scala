package shoppingcheck

import org.scalacheck.Gen._
import org.scalacheck.Shrink

import scala.util.Random._

class ShoppingCartSpec extends BaseSpec {
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  "simpleTotalCents" must {
    "add unit prices per item on the list, regardless of order" in {
      forAll(apples, oranges) { (as, os) =>
        val ps = shuffle(as ++ os)
        sc.simpleTotalCents(ps) shouldBe priceOfApples(as) + priceOfOranges(os)
      }
    }
  }

  "promoTotalCents" must {
    "apply discounts to the total value" in {
      forAll(apples, oranges) { (as, os) =>
        val ps = shuffle(as ++ os)
        sc.promoTotalCents(ps) shouldBe (priceOfApples(as) + priceOfOranges(os)
                                         - sc.discountNforM(as, apple, 2, 1)
                                         - sc.discountNforM(os, orange, 3, 2))
      }
    }
  }

  "discountNforM" must {
    "return half of the prices sum total as discount for even length apples" in {
      val discountApples = (listOf(delay(apple)), "discounted")
      forAll(discountApples) { as =>
        sc.discountNforM(as ++ as, apple, 2, 1) shouldBe priceOfApples(as)
      }
    }

    "not count the extra apple as discount for odd length apples" in {
      val discountApples = (listOf(delay(apple)), "discounted")
      forAll(discountApples) { as =>
        sc.discountNforM(as ++ (as :+ apple), apple, 2, 1) shouldBe priceOfApples(as)
      }
    }

    "not apply discounts to lists without the discounted items" in {
      forAll(oranges) { os =>
        sc.discountNforM(os, apple, 2, 1) shouldBe 0
      }
    }

    "offer 1 orange whenever 3 are bought" in {
      val discountOranges = (listOf(delay(orange)), "discounted")
      forAll(discountOranges) { os =>
        sc.discountNforM(os ++ os ++ os, orange, 3, 2) shouldBe priceOfOranges(os)
      }
    }

    "ignore the extra orange(s) whenever odd numbers are bought" in {
      val discountOranges = (listOf(delay(orange)), "discounted")
      forAll(discountOranges) { os =>
        val oranges = os ++ os ++ os
        sc.discountNforM(oranges :+ orange, orange, 3, 2) shouldBe priceOfOranges(os)
        sc.discountNforM(oranges :+ orange :+ orange, orange, 3, 2) shouldBe priceOfOranges(os)
      }
    }
  }

  // Test Setup
  val sc = new ShoppingCart
  val apple = "apple"
  val orange = "orange"

  val apples = listOf(delay(apple))
  val oranges = listOf(delay(orange))

  def priceOfApples(ps: List[String]) = ps.length * 60
  def priceOfOranges(ps: List[String]) = ps.length * 25
}
