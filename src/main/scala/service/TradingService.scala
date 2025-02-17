package service

import domain._
import zio.ZIO

trait TradingService {
  def createAccount(traderId: TraderId, initialBalance: BigDecimal): ZIO[Any, TradingError, Unit]
  def getAccount(traderId: TraderId): ZIO[Any, TradingError, TraderAccount]
  def placeOrder(order: Order): ZIO[Any, TradingError, Unit]
  def updateBalance(traderId: TraderId, amount: BigDecimal): ZIO[Any, TradingError, Unit]
}



