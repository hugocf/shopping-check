package shoppingcheck

class ShoppingCart {
  val prices = Map("apple" -> 60, "orange" -> 25)

  private def price(p: String) = prices.getOrElse(p, 0)

  def simpleTotalCents(ps: List[String]): Int =
    ps.map(price).sum

  def promoTotalCents(ps: List[String]): Int =
    simpleTotalCents(ps: List[String]) - discountNforM(ps, "apple", 2, 1) - discountNforM(ps, "orange", 3, 2)

  def discountNforM(ps: List[String], p: String, total: Int, pay: Int): Int =
    ps.count(_ == p) / total * price(p) * (total - pay)

}
