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
        sc.simpleTotalCents(ps) shouldBe costOfApples(as) + costOfOranges(os)
      }
    }
  }

  "promoTotalCents" must {
    "apply discounts to the total value" in {
      val applePromo = sc.discountNforM(apple, 2, 1)
      val orangePromo = sc.discountNforM(orange, 3, 2)
      val promos = Seq(applePromo, orangePromo)

      forAll(apples, oranges) { (as, os) =>
        val ps = shuffle(as ++ os)
        sc.promoTotalCents(ps, promos) shouldBe (costOfApples(as) + costOfOranges(os) - applePromo(as) - orangePromo(os))
      }
    }
  }

  "discountNforM" must {
    "return half of the prices sum total as discount for even length apples" in {
      val discountApples = (listOf(delay(apple)), "discounted")
      forAll(discountApples) { as =>
        sc.discountNforM(apple, 2, 1)(as ++ as) shouldBe costOfApples(as)
      }
    }

    "not count the extra apple as discount for odd length apples" in {
      val discountApples = (listOf(delay(apple)), "discounted")
      forAll(discountApples) { as =>
        sc.discountNforM(apple, 2, 1)(as ++ (as :+ apple)) shouldBe costOfApples(as)
      }
    }

    "not apply discounts to lists without the discounted items" in {
      forAll(oranges) { os =>
        sc.discountNforM(apple, 2, 1)(os) shouldBe 0
      }
    }

    "offer 1 orange whenever 3 are bought" in {
      val discountOranges = (listOf(delay(orange)), "discounted")
      forAll(discountOranges) { os =>
        sc.discountNforM(orange, 3, 2)(os ++ os ++ os) shouldBe costOfOranges(os)
      }
    }

    "ignore the extra orange(s) whenever odd numbers are bought" in {
      val discountOranges = (listOf(delay(orange)), "discounted")
      forAll(discountOranges) { os =>
        val oranges = os ++ os ++ os
        sc.discountNforM(orange, 3, 2)(oranges :+ orange) shouldBe costOfOranges(os)
        sc.discountNforM(orange, 3, 2)(oranges :+ orange :+ orange) shouldBe costOfOranges(os)
      }
    }
  }

  // Test Setup
  val sc = new ShoppingCart
  val apple = "apple"
  val orange = "orange"

  val apples = listOf(delay(apple))
  val oranges = listOf(delay(orange))

  def costOfApples(ps: List[String]) = ps.length * 60
  def costOfOranges(ps: List[String]) = ps.length * 25
}
