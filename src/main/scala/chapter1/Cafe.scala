package chapter1

/**
 * Created by tak on 15. 5. 24.
 */


class CreditCard {
  def charge(price: Int) = {}
}

class Coffee(val price: Int) {
}

class Payments {
  def charge(card: CreditCard, price: Int)={}
}

class Cafe {
  def buyCoffee(cc: CreditCard) : Coffee = {
    val cup = new Coffee(30)
    cc.charge(cup.price)
    cup
  }
}

class Cafe2 {
  def buyCoffee(cc: CreditCard, p: Payments) : Coffee = {
    val cup = new Coffee(20)
    p.charge(cc, cup.price)
    cup
  }
}

case class Charge(card: CreditCard, amount: Int) {
  def combine(other: Charge): Charge = {
    if (card == other.card)
      Charge(card, amount + other.amount)
    else
      throw new Exception("Can't combine charges to different cards")
  }
}

class Cafe3 {
  def buyCoffee(cc: CreditCard) : (Coffee, Charge) = {
    val cup = new Coffee(30)
    (cup, Charge(cc, cup.price))
  }

  def buyCoffees(cc: CreditCard, n: Int) : (List[Coffee], Charge) = {
    val purchases : List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))
    val (coffees, charges) = purchases.unzip
    (coffees, charges.reduce( (c1, c2) => c1.combine(c2)) )
  }
}

