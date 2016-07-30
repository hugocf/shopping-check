package shoppingcheck

import org.scalacheck.Gen._
import org.scalacheck.Shrink

import scala.util.Random._

class ShoppingCartSpec extends BaseSpec {
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  "simpleTotalCents" must {
    "add unit prices per item on the list, regardless of order" in {
      forAll(apples, oranges, bananas) { (as, os, bs) =>
        val ps = shuffle(as ++ os ++ bs)
        sc.simpleTotalCents(ps) shouldBe costOfApples(as) + costOfOranges(os) + costOfBananas(bs)
      }
    }
  }

  "promoTotalCents" must {
    "apply the given discounts to the total value" in {
      val applePromo = sc.discountNforM(apple, 2, 1)
      val orangePromo = sc.discountNforM(orange, 3, 2)
      val promos = Seq(applePromo, orangePromo)

      forAll(apples, oranges) { (as, os) =>
        val ps = shuffle(as ++ os)
        sc.promoTotalCents(ps, promos) shouldBe (costOfApples(as) + costOfOranges(os) - applePromo(as) - orangePromo(os))
      }
    }
  }

  "discountNforM" when {

    "discount considers only a single type of product" must {

      "return half of the prices sum total as discount for even length apples" in {
        val discountedApples = (listOf(delay(apple)), "discounted")
        forAll(discountedApples) { as =>
          sc.discountNforM(apple, 2, 1)(as ++ as) shouldBe costOfApples(as)
        }
      }

      "not count the extra apple as discount for odd length apples" in {
        val discountedApples = (listOf(delay(apple)), "discounted")
        forAll(discountedApples) { as =>
          sc.discountNforM(apple, 2, 1)(as ++ (as :+ apple)) shouldBe costOfApples(as)
        }
      }

      "not apply discounts to lists without the discounted items" in {
        forAll(oranges) { os =>
          sc.discountNforM(apple, 2, 1)(os) shouldBe 0
        }
      }

      "offer 1 orange whenever 3 are bought" in {
        val discountedOranges = (listOf(delay(orange)), "discounted")
        forAll(discountedOranges) { os =>
          sc.discountNforM(orange, 3, 2)(os ++ os ++ os) shouldBe costOfOranges(os)
        }
      }

      "ignore the extra orange(s) whenever odd numbers are bought" in {
        val discountedOranges = (listOf(delay(orange)), "discounted")
        forAll(discountedOranges) { os =>
          val oranges = os ++ os ++ os
          sc.discountNforM(orange, 3, 2)(oranges :+ orange) shouldBe costOfOranges(os)
          sc.discountNforM(orange, 3, 2)(oranges :+ orange :+ orange) shouldBe costOfOranges(os)
        }
      }
    }

    "discount considers a mix of different types of product" must {

      "offer all the cheapest products first if they are both of the same number" in {
        val sameLengthLists = for {
          n <- posNum[Int]
          as <- listOfN(n, delay(apple))
          bs <- listOfN(n, delay(banana))
        } yield (as, bs)

        forAll(sameLengthLists) { case (as, bs) =>
          sc.discountNforM(Seq(apple, banana), 2, 1)(as ++ bs) shouldBe costOfBananas(bs)
        }
      }

      "offer all the cheapest products and half of the remaining if the expensive list is bigger" in {
        val lessCheaperProducts = for {
          as <- apples
          len <- choose(0, as.length)
          bs <- listOfN(len, delay(banana))
        } yield (as, bs)

        forAll(lessCheaperProducts) { case (as, bs) =>
          val expectedDiscount = costOfBananas(bs) + (as.length - bs.length) / 2 * 60
          sc.discountNforM(Seq(apple, banana), 2, 1)(as ++ bs) shouldBe expectedDiscount
        }
      }

      "offer all the cheapest products and half of the remaining if the cheapest list is bigger" in {
        val lessCheaperProducts = for {
          as <- apples
          len <- posNum[Int] suchThat (_ > as.length)
          bs <- listOfN(len, delay(banana))
        } yield (as, bs)

        forAll(lessCheaperProducts) { case (as, bs) =>
          val expectedDiscount = (as.length * 20) + (bs.length - as.length) / 2 * 20
          sc.discountNforM(Seq(apple, banana), 2, 1)(as ++ bs) shouldBe expectedDiscount
        }
      }
    }
  }

  // Test Setup
  val sc = new ShoppingCart
  val apple = "apple"
  val orange = "orange"
  val banana = "banana"

  val apples = listOf(delay(apple))
  val oranges = listOf(delay(orange))
  val bananas = listOf(delay(banana))

  def costOfApples(ps: List[String]) = ps.length * 60
  def costOfOranges(ps: List[String]) = ps.length * 25
  def costOfBananas(ps: List[String]) = ps.length * 20
}
