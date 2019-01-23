package wvs.task.model

import org.scalatest.{FunSuite, Matchers}
import wvs.task.model.TradingModel.{Bid, BidType, IllegalInitializationException}


class TradingModelSuite extends FunSuite with Matchers {
  test("Check Bid initialization") {

    noException should be thrownBy Bid("John", BidType.Sell, "StockA", 3, 5)

    the [IllegalInitializationException] thrownBy {
      Bid("", BidType.Sell, "StockA", 3, 5)
    } should have message "Empty trader id in bid"

    the [IllegalInitializationException] thrownBy {
      Bid("John", BidType.Sell, "", 3, 5)
    } should have message "Empty stock id in bid"

    the [IllegalInitializationException] thrownBy {
      Bid("John", BidType.Sell, "StockA", -3, 5)
    } should have message "Can't bid with zero or negative price"

    the [IllegalInitializationException] thrownBy {
      Bid("John", BidType.Sell, "StockA", 3, -5)
    } should have message "Can't bid with zero or negative amount"
  }

  test("Check bid parsing") {

    Bid.parse("C8\tb\tB\t6\t5") should equal (Right(Bid("C8", BidType.Buy, "B", 6, 5)))

    Bid.parse("C8\tb\tB") should equal (Left("Insufficient fields in bid"))

    Bid.parse("C8\tb\tB\tx\t5") should be ('left)
  }
}
