package wvs.task.app

import wvs.task.model.TradingModel.{Bid, BidType, TraderStatus}

import scala.annotation.tailrec
import scala.io.Source


object PlayStockExchangeApp {
  def main(args: Array[String]): Unit = {

    (new PlayStockExchangeApp).play()
      .map(_.asPrintable)
      .foreach(println)
  }
}

class PlayStockExchangeApp {

  type TraidersTeam = Map[String, TraderStatus]

  private[this] val tradersInitial = Source.fromResource("input/clients.txt")
    .getLines()
    .flatMap(s => TraderStatus.parse(s).toOption)
    .map(t => t.id -> t)
    .toMap

  private[this] val bidsInitial = Source.fromResource("input/orders.txt")
    .getLines()
    .flatMap(s => Bid.parse(s).toOption)
    .toVector

  case class DealResult(firstBid: Bid, secondBid: Bid, traders: TraidersTeam)

  // Some(_) if bids was applied fully; result is updated bids and traders statuses.
  // Updated bids can have void amount it means bid applied totally and will be removes further.
  // None if bids are not applied.
  private[this] def dealBidsPair(firstBid: Bid, secondBid: Bid, traders: TraidersTeam): Option[DealResult] = {
    if (firstBid.stockId == secondBid.stockId &&
      firstBid.traider != secondBid.traider &&
      firstBid.bType != secondBid.bType) {

      val (buyBid, sellBid) = if (firstBid.bType == BidType.Buy) {
        firstBid -> secondBid
      } else {
        secondBid -> firstBid
      }

      if (buyBid.price >= sellBid.price) {
        for (
          buyTraider <- traders.get(buyBid.traider);
          sellTraider <- traders.get(sellBid.traider);

          canBuyAmount = buyTraider.money / buyBid.price;
          dealAmount = Vector(canBuyAmount, buyBid.amount, sellBid.amount).min if dealAmount > 0) yield {

          val dealMoney = dealAmount * buyBid.price

          val buyTraiderUpdated = buyTraider
            .changeMoneyBy(-dealMoney)
            .changeStockBy(buyBid.stockId, dealAmount)

          val sellTraiderUpdated = sellTraider
            .changeMoneyBy(dealMoney)
            .changeStockBy(buyBid.stockId, -dealAmount)

          val tradersUpdated = traders +
            (buyTraider.id -> buyTraiderUpdated) +
            (sellTraider.id -> sellTraiderUpdated)

          val buyBidUpdated = buyBid.partiallyDealed(dealAmount)
          val sellBidUpdated = sellBid.partiallyDealed(dealAmount)

          DealResult(buyBidUpdated, sellBidUpdated, tradersUpdated)
        }
      } else {
        None
      }
    } else {
      None
    }
  }

  case class CuttedBids(headToApply: Seq[Bid], bidsTail: Seq[Bid]) {
    def glue: Seq[Bid] = headToApply ++ bidsTail

    def shift: CuttedBids = {
      if (bidsTail.nonEmpty) {
        CuttedBids(headToApply :+ bidsTail.head, bidsTail.tail)
      } else this
    }

    def activeBid: Option[Bid] = bidsTail.headOption
  }

  case class ApplyBidToHeadResult(gatheredBids: Seq[Bid], activeBid: Option[Bid],
    traders: TraidersTeam, headChanged: Boolean = false) {

    def glue: Seq[Bid] = gatheredBids ++ Vector(activeBid).flatten
  }

  private[this] def applyBidToHead(cuttedBids: CuttedBids, traders: TraidersTeam): ApplyBidToHeadResult = {
    cuttedBids.activeBid.fold(ApplyBidToHeadResult(cuttedBids.headToApply, None, traders)) { activeBid =>
      cuttedBids.headToApply
        .foldLeft(ApplyBidToHeadResult(Seq.empty, Some(activeBid), traders)) { case (res, candidateBid) =>
          res.activeBid.flatMap(dealBidsPair(_, candidateBid, res.traders)) match {
            case Some(dealRes) =>
              val activeUpdated = Some(dealRes.firstBid).filter(_.amount > 0)
              val gatheredUpdated = res.gatheredBids ++ Vector(dealRes.secondBid).filter(_.amount > 0)
              ApplyBidToHeadResult(gatheredUpdated, activeUpdated, dealRes.traders, headChanged = true)
            case _ => res
          }
        }
    }
  }

  def play(): Seq[TraderStatus] = {
    @tailrec
    def doPlayNext(bidsStatus: CuttedBids, traders: TraidersTeam): Unit = {
      if (bidsStatus.bidsTail.nonEmpty) {
        val applyRes = applyBidToHead(bidsStatus, traders)
        val (bidsStatusUpdated, tradersStatusUpdated) =
        if (applyRes.headChanged) {
          CuttedBids(Seq.empty, applyRes.glue ++ bidsStatus.bidsTail.tail) -> applyRes.traders
        } else {
          bidsStatus.shift -> traders
        }
        doPlayNext(bidsStatusUpdated, tradersStatusUpdated)
      }
    }

    doPlayNext(CuttedBids(Seq.empty, bidsInitial.tail), tradersInitial)

    tradersInitial.values.toSeq.sortBy(_.id)
  }
}