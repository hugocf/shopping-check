package shoppingcheck

class ShoppingCart {
  val prices = Map("apple" -> 60, "orange" -> 25)

  def totalCents(ps: List[String]): Int = ps.map(prices.getOrElse(_, 0)).sum

}
