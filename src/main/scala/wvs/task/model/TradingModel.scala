package wvs.task.model

import scala.util.{Failure, Success, Try}

object TradingModel {

  case class IllegalInitializationException(msg: String) extends RuntimeException(msg)
  case class UnexpectedPlayStatusException(msg: String) extends RuntimeException(msg)

  type StockId = String

  type TraiderId = String

  // amount of stocks by stock id
  type StocksPocket = Map[StockId, Int]

  type ParseError = String

  val stocksIds: Seq[String] = Vector("A", "B", "C", "D")

  sealed trait BidType {
    val symb: String
    def opposite: BidType
  }
  object BidType {
    object Buy extends BidType {
      override val symb = "b"
      override def opposite: BidType = Sell
    }
    object Sell extends BidType {
      override val symb = "s"
      override def opposite: BidType = Buy
    }

    def fromString(s: String): BidType =
      s.toLowerCase match {
        case BidType.Buy.symb => BidType.Buy
        case BidType.Sell.symb => BidType.Sell
        case _ =>
          throw new IllegalArgumentException("Illegal bid type symbol on bid type parse")
      }
  }

  // Sell or Buy intention
  case class Bid(traider: TraiderId, bType: BidType, stockId: StockId, price: Int, amount: Int) {
    def checkProperties(): Unit = {
      if (traider.isEmpty) {
        throw IllegalInitializationException("Empty trader id in bid")
      }
      if (stockId.isEmpty) {
        throw IllegalInitializationException("Empty stock id in bid")
      }
      if (price < 0) {
        throw IllegalInitializationException("Can't bid with zero or negative price")
      }
      if (amount < 0) {
        throw IllegalInitializationException("Can't bid with zero or negative amount")
      }
    }

    locally {
      checkProperties()
    }

    def partiallyDealed(dealed: Int): Bid = this.copy(amount = this.amount - dealed)
  }

  object Bid {
    private[this] val BidPartsCount = 5

    def parse(s: String): Either[ParseError, Bid] = {
      s.split("\t").take(BidPartsCount) match {
        case Array(traderId, symb, stockId, price, amount) =>
          Try {
            Bid(traderId, BidType.fromString(symb), stockId, price.toInt, amount.toInt)
          } match {
            case Success(bid) => Right(bid)
            case Failure(x)  => Left(x.getMessage)
          }
        case _ =>
          Left("Insufficient fields in bid")
      }
    }
  }

  case class TraderStatus(id: String, money: Int, pocket: StocksPocket) {

    def asPrintable: String = {
      val stocksAmount = stocksIds.map(pocket).mkString("\t")
      s"$id\t$money\t$stocksAmount"
    }

    def changeMoneyBy(change: Int): TraderStatus = copy(money = this.money + change)

    def changeStockBy(stockId: StockId, change: Int): TraderStatus = {
      pocket.get(stockId).fold(this) { v =>
        copy(pocket = this.pocket + (stockId -> (v + change)))
      }
    }
  }

  object TraderStatus {

    def parse(s: String): Either[ParseError, TraderStatus] = {
      s.split("\t").toList match {
        case traderId :: money :: stocks =>
          stocks.padTo(stocksIds.size, 0).zip(stocksIds)
          Try {
            TraderStatus(traderId, money.toInt, stocksIds.zip(stocks.map(_.toInt)).toMap)
          } match {
            case Success(ts) => Right(ts)
            case Failure(x)  => Left(x.getMessage)
          }
        case _ =>
          Left("Insufficient fields in trader status")
      }
    }

  }
}
