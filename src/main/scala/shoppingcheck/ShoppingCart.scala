package shoppingcheck

class ShoppingCart {
  type DiscountFunction = List[String] => Int
  val prices = Map("apple" -> 60, "orange" -> 25)

  def simpleTotalCents(ps: List[String]): Int =
    ps.map(price).sum

  def promoTotalCents(ps: List[String], discounts: Seq[DiscountFunction]): Int =
    simpleTotalCents(ps: List[String]) - discounts.map(f => f(ps)).sum

  def discountNforM(p: String, total: Int, pay: Int): DiscountFunction =
    (ps) => ps.count(_ == p) / total * price(p) * (total - pay)

  private def price(p: String) = prices.getOrElse(p, 0)

}
