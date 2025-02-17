import domain._
import service.{InMemoryTradingService, TradingService}
import zio.{Scope, _}
import zio.test.Assertion._
import zio.test._

import java.util.UUID

object TradingServiceSpec extends ZIOSpecDefault {

  def createTestOrder(
    traderId: TraderId,
    symbol: StockSymbol,
    orderType: OrderType,
    quantity: Int,
    price: BigDecimal
  ): Order = Order(traderId, OrderId(UUID.randomUUID().toString), symbol, orderType, Quantity(quantity), Price(price))

  val testLayer: ZLayer[Any, Nothing, TradingService] = InMemoryTradingService.live

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("TradingService")(
    suite("Account Management")(
      test("creating a valid account should succeed") {
        for {
          service <- ZIO.service[TradingService]
          traderId = TraderId("trader1")
          _ <- service.createAccount(traderId, 1000.0)
          account <- service.getAccount(traderId)
        } yield assertTrue(account.balance == 1000.0, account.positions.isEmpty)
      },
      test("creating a duplicate account should fail") {
        for {
          service <- ZIO.service[TradingService]
          traderId = TraderId("trader1")
          _ <- service.createAccount(traderId, 1000.0)
          result <- service.createAccount(traderId, 1000.0).exit
        } yield assert(result)(fails(isSubtype[AccountAlreadyExists](anything))) // explain
      }
    ),
    suite("Order Placement")(
      test("placing a buy order with sufficient funds should succeed") {
        for {
          service <- ZIO.service[TradingService]
          traderId = TraderId("trader1")
          _ <- service.createAccount(traderId, 1000.0)
          order = createTestOrder(traderId, StockSymbol("AAPL"), Buy, 10, 50.0)
          _ <- service.placeOrder(order)
          account <- service.getAccount(traderId)
        } yield assertTrue(account.balance == 500.0, account.positions.get(StockSymbol("AAPL")).map(_.value).get == 10)
      },
      test("placing buy order with insufficient funds should fail with InsufficientFunds") {
        for {
          service <- ZIO.service[TradingService]
          traderId = TraderId("trader1")
          _ <- service.createAccount(traderId, 100.0)
          order = createTestOrder(traderId, StockSymbol("AAPL"), Buy, 10, 50.0)
          result <- service.placeOrder(order).exit
        } yield assert(result)(fails(isSubtype[InsufficientFunds](anything)))
      },
      test("multiple concurrent withdrawals should maintain non-negative balance") {
        for {
          service <- ZIO.service[TradingService]
          traderId = TraderId("trader1")
          _ <- service.createAccount(traderId, 1000.0)

          // Create 10 concurrent orders that each try to spend 200
          // Total attempted to spend: 2000 from a 1000 balance
          orders = List.fill(10)(
            createTestOrder(traderId, StockSymbol("AAPL"), Buy, 2, 100.0)
          )

          // Run all orders concurrently

          // TODO explain this exit.fork

          fibers <- ZIO.foreach(orders)(order => service.placeOrder(order).exit.fork)

          // Gather all results
          results <- ZIO.foreach(fibers)(_.join)

          // Check final account state
          finalAccount <- service.getAccount(traderId)
        } yield
        // Verify exactly 5 orders succeeded (1000/200 = 5)
        assert(results.count(_.isSuccess))(equalTo(5)) &&
          // Verify balance is non-negative
          assertTrue(finalAccount.balance >= 0.0)
      },
      test("concurrent buy and sell orders should maintain position consistency") {
        for {
          service <- ZIO.service[TradingService]
          traderId = TraderId("trader1")
          _ <- service.createAccount(traderId, 2000.0)

          // First buy some initial position
          initialBuy = createTestOrder(traderId, StockSymbol("AAPL"), Buy, 10, 100.0)
          _ <- service.placeOrder(initialBuy)

          // Create concurrent buys and sells
          buyOrders = List.fill(5)(
            createTestOrder(traderId, StockSymbol("AAPL"), Buy, 2, 100.0)
          )
          sellOrders = List.fill(5)(
            createTestOrder(traderId, StockSymbol("AAPL"), Sell, 2, 100.0)
          )

          // Mix buys and sells and run them concurrently
          allOrders = util.Random.shuffle(buyOrders ++ sellOrders)
          fibers <- ZIO.foreach(allOrders)(order => service.placeOrder(order).exit.fork)

          _ <- ZIO.foreachDiscard(fibers)(_.join)
          finalAccount <- service.getAccount(traderId)

        } yield
        // Position should always be valid (>= 0)
        assertTrue(
          finalAccount.positions.get(StockSymbol("AAPL")).map(_.value).get >= 0,
          finalAccount.positions.get(StockSymbol("AAPL")).map(_.value).get <= 10
        )
      }
    )
  ).provide(testLayer)
}
