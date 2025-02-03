package domain

import java.time.Instant

/*
  An Auction represents a single auction for some `item`, that auction only occurs between two dates.

 */

case class AuctionId(id: String)

case class User(id: String, name: String)

case class Bid(
  amount: BigDecimal,
  bidder: User,
  timestamp: Instant)

case class Auction(
  id: AuctionId,
  item: String,
  startTime: Instant,
  endTime: Instant,
  currentHighestBid: Option[Bid],
  bids: List[Bid])
