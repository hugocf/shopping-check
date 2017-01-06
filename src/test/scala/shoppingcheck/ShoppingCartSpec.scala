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
        val expectedCost = costOfApples(as) + costOfOranges(os)
        val expectedDiscount = applePromo(as) + orangePromo(os)

        sc.promoTotalCents(ps, promos) shouldBe (expectedCost - expectedDiscount)
      }
    }
  }

  "totalCents" should {
    "apply promotions for (apples + bananas), oranges, and melons" in {
      val appleBananaPromo = sc.discountNforM(Seq(apple, banana), 2, 1)
      val orangePromo = sc.discountNforM(orange, 3, 2)
      val melonPromo = sc.discountNforM(melon, 3, 2)

      forAll(apples, oranges, bananas, melons) { (as, os, bs, ms) =>
        val ps = shuffle(as ++ os ++ bs ++ ms)
        val expectedCost = costOfApples(as) + costOfBananas(bs) + costOfOranges(os) + costOfMelons(ms)
        val expectedDiscount = appleBananaPromo(as ++ bs) + orangePromo(os) + melonPromo(ms)

        sc.totalCents(ps) shouldBe (expectedCost - expectedDiscount)
      }
    }
  }

  "discountNforM" when {

    "discount considers only a single type of product" must {

      "return half of the prices sum total as discount for even length apples" in {
        val discountedApples = (listOf(const(apple)), "discounted")
        forAll(discountedApples) { as =>
          sc.discountNforM(apple, 2, 1)(as ++ as) shouldBe costOfApples(as)
        }
      }

      "not count the extra apple as discount for odd length apples" in {
        val discountedApples = (listOf(const(apple)), "discounted")
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
        val discountedOranges = (listOf(const(orange)), "discounted")
        forAll(discountedOranges) { os =>
          sc.discountNforM(orange, 3, 2)(os ++ os ++ os) shouldBe costOfOranges(os)
        }
      }

      "ignore the extra orange(s) whenever odd numbers are bought" in {
        val discountedOranges = (listOf(const(orange)), "discounted")
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
          as <- listOfN(n, const(apple))
          bs <- listOfN(n, const(banana))
        } yield (as, bs)

        forAll(sameLengthLists) { case (as, bs) =>
          sc.discountNforM(Seq(apple, banana), 2, 1)(as ++ bs) shouldBe costOfBananas(bs)
        }
      }

      "offer all the cheapest products and half of the remaining if the expensive list is bigger" in {
        val lessCheaperProducts = for {
          as <- apples
          len <- choose(0, as.length)
          bs <- listOfN(len, const(banana))
        } yield (as, bs)

        forAll(lessCheaperProducts) { case (as, bs) =>
          val expectedDiscount = costOfBananas(bs) + (as.length - bs.length) / 2 * applePrice
          sc.discountNforM(Seq(apple, banana), 2, 1)(as ++ bs) shouldBe expectedDiscount
        }
      }

      "offer all the cheapest products and half of the remaining if the cheapest list is bigger" in {
        val lessCheaperProducts = for {
          as <- apples
          len <- posNum[Int] suchThat (_ > as.length)
          bs <- listOfN(len, const(banana))
        } yield (as, bs)

        forAll(lessCheaperProducts) { case (as, bs) =>
          val expectedDiscount = (as.length * bananaPrice) + (bs.length - as.length) / 2 * bananaPrice
          sc.discountNforM(Seq(apple, banana), 2, 1)(as ++ bs) shouldBe expectedDiscount
        }
      }
    }
  }

  // Test Setup
  val sc = new ShoppingCart

  val (apple, applePrice) = ("apple", 60)
  val (orange, orangePrice) = ("orange", 25)
  val (banana, bananaPrice) = ("banana", 20)
  val (melon, melonPrice) = ("melon", 100)

  val apples = listOf(const(apple))
  val oranges = listOf(const(orange))
  val bananas = listOf(const(banana))
  val melons = listOf(const(melon))

  def costOfX(x: String, price: Int, ps: List[String]) = {
    require(ps.forall(_ == x), s"costOf${x.toLowerCase.capitalize} must contain only ${x}s")
    ps.length * price
  }
  def costOfApples(ps: List[String]) = costOfX(apple, applePrice, ps)
  def costOfOranges(ps: List[String]) = costOfX(orange, orangePrice, ps)
  def costOfBananas(ps: List[String]) = costOfX(banana, bananaPrice, ps)
  def costOfMelons(ps: List[String]) = costOfX(melon, melonPrice, ps)
}
