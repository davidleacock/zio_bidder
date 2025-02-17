package domain

case class TraderId(value: String) extends AnyVal
case class StockSymbol(value: String) extends AnyVal
case class Quantity(value: Int) extends AnyVal
case class Price(value: BigDecimal) extends AnyVal
case class OrderId(value: String) extends AnyVal

sealed trait OrderType
case object Buy extends OrderType
case object Sell extends OrderType

case class Order(
  traderId: TraderId,
  orderId: OrderId,
  symbol: StockSymbol,
  orderType: OrderType,
  quantity: Quantity,
  price: Price)

case class TraderAccount(
  id: TraderId,
  balance: BigDecimal,
  positions: Map[StockSymbol, Quantity])

sealed trait TradingError extends Throwable
case class InsufficientFunds(
  required: BigDecimal,
  available: BigDecimal)
    extends TradingError {
  override def getMessage = s"Insufficient funds: required $required but only have $available"
}

case class InsufficientPosition(
  symbol: StockSymbol,
  required: Quantity,
  available: Quantity)
    extends TradingError {
  override def getMessage =
    s"Insufficient position in ${symbol.value}: required ${required.value} but only have ${available.value}"
}

case class OrderExpired(
  orderId: String, // We should add an orderId to our Order class
  attempts: Int,
  lastError: Option[Throwable])
    extends TradingError {
  override def getMessage =
    s"Order $orderId expired after $attempts attempts. ${lastError.map(e => s"Last error: ${e.getMessage}").getOrElse("")}"
}

case class AccountUpdateError(
  traderId: TraderId,
  amount: BigDecimal)
    extends TradingError {
  override def getMessage =
    s"Unable to add $amount to account for traderId $traderId."
}

case class AccountNotFound(traderId: TraderId) extends TradingError {
  override def getMessage = s"Account not found: ${traderId.value}"
}

case class AccountCreationError(
  traderId: TraderId,
  amount: BigDecimal)
    extends TradingError {
  override def getMessage =
    s"Unable to create new account for traderId $traderId with initial balance of $amount."
}

case class AccountAlreadyExists(
  traderId: TraderId)
    extends TradingError {
  override def getMessage =
    s"TraderId $traderId account already exists."
}
