package ch.shastick

import ch.shastick.immutable.TSEntry

import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, Builder}

object NumericTimeSeries {
  
  /** 
   *  Defensive 'plus' operator: wherever one of the time series 
   *  is  undefined, the result is undefined.
   */
  def strictPlus[T](lhO: Option[T], rhO: Option[T])(implicit n: Numeric[T])
    : Option[T] = {
      import n._
      (lhO, rhO) match {
        case (Some(l), Some(r)) => Some(l + r)
        case _ => None
      }
  }
  
  /** 
   *  Defensive 'minus' operator: wherever one of the time series 
   *  is  undefined, the result is undefined.
   */
  def strictMinus[T](lhO: Option[T], rhO: Option[T])(implicit n: Numeric[T])
    : Option[T] = { 
      import n._
      (lhO, rhO) match {
        case (Some(l), Some(r)) => Some(l - r)
        case _ => None
      }
  }
  
  /** 
   *  Defensive multiplication operator: wherever one of the time series
   *  is  undefined, the result is undefined.
   */
  def strictMultiply[T](lhO: Option[T], rhO: Option[T])(implicit n: Numeric[T])
    : Option[T] = { 
      import n._
      (lhO, rhO) match {
        case (Some(l), Some(r)) => Some(l * r)
        case _ => None
      }
  }


  def slidingSum[T](entries: Seq[TSEntry[T]], window: Long)(implicit n: Numeric[T]): Seq[TSEntry[T]] =
    if (window < 1) {
      throw new IllegalArgumentException("Window must be strictly positive. Was " + window)
    } else if (entries.isEmpty) {
      Seq.empty
    } else {
      // Initialise the recursion
      slideMySum[T](
        entries,
        // TODO: passing 'entries' here could be made to work as well,
        // as long as we just return the sum result
        Seq(),
        0.asInstanceOf[T], // Sum starts at 0
        entries.head.timestamp, // initial entry's timestamp
        new ArrayBuffer() // an empty accumulator
      )(
        window,
        entries.last.definedUntil
      )
    }

  @tailrec
  def slideMySum[T](
                  remaining: Seq[TSEntry[T]],
                  inWindow: Seq[TSEntry[T]],
                  previousSum: T,
                  windowHeadTime: Long,
                  acc: Builder[TSEntry[T], Seq[TSEntry[T]]]
                )(
                  windowLength: Long,
                  endOfTime: Long
                )(
                  implicit n: Numeric[T]
                ): Seq[TSEntry[T]] = {

    if (windowHeadTime == endOfTime) {
      // For now we stop here
      return acc.result()
    }

    val windowTailTime = windowHeadTime - windowLength + 1

    val (nextRemaining, nextWindow, updatedSum) =
      updateCollectionsAndSum(remaining, inWindow, previousSum, windowHeadTime, windowTailTime)

    // For how long is the new window content valid?
    val nextHeadTime =
      newAdvance(nextRemaining, nextWindow, windowHeadTime, windowTailTime)(endOfTime)

    if (!nextWindow.isEmpty) {
      // Currently only add an entry to the result if the current window is non-empty
      // TODO think about the merits of doing this?$
      // This is both so that we have a 'unit operator when window is size 1' as well
      // as having no defined output when the window is completely over undefined input
      // At some point in the future we may want to let the user control what happens here.
      acc += TSEntry(windowHeadTime, updatedSum, nextHeadTime - windowHeadTime)
    }

    // Down the rabbit hole, baby
    slideMySum(
      nextRemaining,
      nextWindow,
      updatedSum,
      nextHeadTime,
      acc
    )(windowLength, endOfTime)
  }

  /**
    * @return how far we may advance the window until we reach a point where we need to add or remove something,
    *         or we reach the end of the domain we wish to compute a sliding window for.
    */
  def newAdvance[T](
                     remaining: Seq[TSEntry[T]],
                     inWindow: Seq[TSEntry[T]],
                     windowHeadTime: Long,
                     windowTailTime: Long
                   )(endOfTime: Long) =
    // TODO make this if/else block nicer?
    if (remaining.isEmpty) {
      windowHeadTime +
        Math.min(
          // Time to end of domain of interest, as there will be no new entries to add
          endOfTime - windowHeadTime,
          // Time to next entry to leave the window
          inWindow.headOption.map(_.definedUntil - windowTailTime).getOrElse(Long.MaxValue)
        )
    } else {
      windowHeadTime +
        Math.min(
          // Time to next entry to enter the window, if there is any
          remaining.headOption.map(_.timestamp - windowHeadTime).getOrElse(Long.MaxValue),
          // Time to next entry to leave the window
          inWindow.headOption.map(_.definedUntil - windowTailTime).getOrElse(Long.MaxValue)
        )
    }


  /**
    * Return a (remaining, inWindow) adapted from the passed 'remaining' and 'inWindow' entries according
    * to the specified window head and tail times.
    */
  def updateCollectionsAndSum[T](
                                   remaining: Seq[TSEntry[T]],
                                   inWindow: Seq[TSEntry[T]],
                                   currentSum: T,
                                   windowHeadTime: Long,
                                   windowTailTime: Long
                                 )(implicit n: Numeric[T]): (Seq[TSEntry[T]], Seq[TSEntry[T]], T) = {

    import n._

    (remaining.headOption, inWindow.headOption) match {
      // Both window head and tail are defined, and reached a new entry and an existing entry's end
      case (Some(next), Some(last))
        if next.timestamp == windowHeadTime
          && last.definedUntil == windowTailTime =>
        (
          remaining.tail, // Remove head from remaining
          inWindow.tail :+ next, // Keep current window's tail (head needs to be removed) and add remaining's head
          currentSum - last.value + next.value
        )

      case (Some(next), _)
        if next.timestamp == windowHeadTime =>
        // the window head reached an entry which now needs to be added
        (
          remaining.tail, // Remove head from remaining
          inWindow :+ next, // Add remaining's head to current window content
          currentSum + next.value
        )
      case (_, Some(last))
        if last.definedUntil == windowTailTime =>
        // the window tail reached the end of an entry's domain: it needs to be removed
        (
          remaining, // remaining remains as-is
          inWindow.tail, // window content is trimmed from its first element
          currentSum - inWindow.head.value
        )
      case _ =>
        throw new IllegalArgumentException("expecting exact boundary matches")
    }
  }
}