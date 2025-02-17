package service

import domain.{Order, StockSymbol, TraderAccount, TraderId}
import zio.ZLayer
import zio.stm.TMap

object InMemoryTradingService {
  val live: ZLayer[Any, Nothing, TradingService] = ZLayer.scoped {
    for {
      orderBook <- TMap.empty[StockSymbol, List[Order]].commit
      accounts <- TMap.empty[TraderId, TraderAccount].commit
    } yield new TradingServiceLive(orderBook, accounts)
  }
}