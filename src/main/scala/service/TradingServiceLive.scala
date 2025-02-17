package service

import domain._
import zio.stm.{STM, TMap}
import zio.{Ref, Schedule, ZIO, durationInt}

class TradingServiceLive(
  orderBook: TMap[StockSymbol, List[Order]],
  accounts: TMap[TraderId, TraderAccount])
    extends TradingService {

  private def attemptPlaceOrder(order: Order): STM[TradingError, Unit] =
    for {
      // Verify account
      accountOpt <- accounts.get(order.traderId)
      account <- STM
        .fromOption(accountOpt)
        .mapError(_ => AccountNotFound(order.traderId))

      // Verify order type
      _ <- order.orderType match {
        case Buy =>
          val requiredFunds = order.price.value * order.quantity.value
          if (account.balance >= requiredFunds) STM.unit
          else STM.fail(InsufficientFunds(requiredFunds, account.balance))
        case Sell =>
          val currentPosition = account.positions.getOrElse(order.symbol, Quantity(0))
          if (currentPosition.value >= order.quantity.value) STM.unit
          else STM.fail(InsufficientPosition(order.symbol, order.quantity, currentPosition))
      }

      // Update order book
      currentOrders <- orderBook.get(order.symbol).map(_.getOrElse(List.empty))
      _ <- orderBook.put(order.symbol, currentOrders :+ order)

      // Update account
      _ <- order.orderType match {
        case Buy =>
          val newBalance = account.balance - (order.price.value * order.quantity.value)
          val newPositions = account.positions.updatedWith(order.symbol) {
            case Some(qty) => Some(Quantity(qty.value + order.quantity.value))
            case None      => Some(Quantity(order.quantity.value))
          }
          accounts.put(account.id, account.copy(balance = newBalance, positions = newPositions))

        case Sell =>
          val newBalance = account.balance + (order.price.value * order.quantity.value)
          val newPositions = account.positions.updatedWith(order.symbol) {
            case Some(qty) => Some(Quantity(qty.value - order.quantity.value))
            case None      => None
          }
          accounts.put(account.id, account.copy(balance = newBalance, positions = newPositions))
      }
    } yield ()

  override def placeOrder(order: Order): ZIO[Any, TradingError, Unit] = {

    def retryWithTracking(attemptCount: Ref[Int], lastError: Ref[Option[Throwable]]): ZIO[Any, TradingError, Unit] =
      attemptPlaceOrder(order)
        .commit
        .tapBoth(
          error =>
            lastError.set(Some(error)) *> attemptCount.update(_ + 1),
          _ => attemptCount.update(_ + 1)
        )

    // TODO does this retry logic happen at the STM or ZIO level?

    for {
      attemptCount <- Ref.make(0)
      lastError <- Ref.make[Option[Throwable]](None)
      result <- retryWithTracking(attemptCount, lastError)
        .timeout(5.seconds)
        .flatMap {
          case None =>
            for {
              attempts <- attemptCount.get
              error <- lastError.get
              _ <- ZIO.fail(OrderExpired(order.orderId.value, attempts, error))
            } yield ()
          case Some(_) => ZIO.unit
        }
        .retry(
          Schedule.recurs(3) &&
            Schedule.exponential(100.milliseconds) &&
            // Stop retrying on business validation errors
            Schedule.recurWhile[TradingError] {
              case _: InsufficientFunds    => false
              case _: InsufficientPosition => false
              case _: AccountNotFound      => false
              case _: OrderExpired         => false
              case _                       => true // Retry on other errors (like concurrent access)
            }
        )
    } yield result
  }

  override def getAccount(traderId: TraderId): ZIO[Any, TradingError, TraderAccount] =
    accounts
      .get(traderId)
      .flatMap(STM.fromOption(_))
      .mapError(_ => AccountNotFound(traderId))
      .commit

  override def updateBalance(traderId: TraderId, amount: BigDecimal): ZIO[Any, TradingError, Unit] = {
    val transaction = for {
      // Verify account
      accountOpt <- accounts.get(traderId)
      account <- STM
        .fromOption(accountOpt)
        .mapError(_ => AccountNotFound(traderId))

      newBalance = account.balance + amount
      _ <- if (newBalance >= 0) STM.unit else STM.fail(AccountUpdateError(traderId, amount))
      _ <- accounts.put(traderId, account.copy(balance = newBalance))

    } yield ()
    transaction.commit
  }

  override def createAccount(traderId: TraderId, initialBalance: BigDecimal): ZIO[Any, TradingError, Unit] = {
    val transaction = for {
      exists <- accounts.contains(traderId)
      _ <- if (!exists) STM.unit else STM.fail(AccountAlreadyExists(traderId))
      _ <- if (initialBalance >= 0) STM.unit else STM.fail(AccountCreationError(traderId, initialBalance))
      _ <- accounts.put(traderId, TraderAccount(traderId, initialBalance, Map.empty))
    } yield ()

    transaction.commit
  }
}
