package shoppingcheck

class ShoppingCart {
  type DiscountFunction = List[String] => Int
  private val prices = Map("apple" -> 60, "orange" -> 25, "banana" -> 20, "melon" -> 100)
  private def price(p: String) = prices.getOrElse(p, 0)

  def totalCents(ps: List[String]): Int = {
    val appleBananaPromo = discountNforM(Seq("apple", "banana"), 2, 1)
    val orangePromo = discountNforM("orange", 3, 2)
    val melonPromo = discountNforM("melon", 3, 2)
    promoTotalCents(ps, Seq(appleBananaPromo, orangePromo, melonPromo))
  }

  def promoTotalCents(ps: List[String], discounts: Seq[DiscountFunction]): Int =
    simpleTotalCents(ps: List[String]) - discounts.map(f => f(ps)).sum

  def simpleTotalCents(ps: List[String]): Int = ps.map(price).sum

  def discountNforM(d: String, total: Int, pay: Int): DiscountFunction =
    discountNforM(Seq(d), total, pay)

  def discountNforM(ds: Seq[String], total: Int, pay: Int): DiscountFunction =
    (ps) => {
      val discountItems = ps.collect { case p if ds.contains(p) => p }
      val countFree = discountItems.length / total * (total - pay)
      discountItems.map(price).sorted.take(countFree).sum
    }
}
