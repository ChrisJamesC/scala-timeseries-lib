package ch.poney.immutable

import ch.poney.TimeSeries

case class TSEntry[T]
    (timestamp: Long,  
     value: T,  
     validity: Long) 
     extends TimeSeries[T] {
  
  def at(t: Long): Option[T] = TSEntry.valueAt(timestamp, value, validity)(t)

  def size(): Int = 1
  
  /** Convert this entry to a value */
  lazy val toVal = TSValue(value, validity)
  
  /** Convert this entry to a time->TSVal tuple to be added to a map */
  lazy val toMapTuple = (timestamp -> toVal)
  
  /** Shorten this entry's validity if it exceed 'at'. No effect otherwise.
   *  
   *  If the entry's timestamp is after 'at', the entry remains unchanged.
   */
  def trimRight(at: Long) = 
    if (at <= timestamp) // Trim before or exactly on value start: result is empty.
      EmptyTimeSeries()
    else // Entry needs to have its validity adapted.
      trimEntryRight(at)
      
  /** Similar to trimLeft, but returns a TSEntry instead of a time series and throws 
   *  if 'at' is before the entry's timestamp. */
  def trimEntryRight(at: Long) =
    if (at <= timestamp) // Trim before or exactly on value start: result is empty.
      throw new IllegalArgumentException(
          s"Attempting to trim right at $at before entry's domain has started ($timestamp)")
    else if (at >= definedUntil) // No effect, return this instance
      this
    else // Entry needs to have its validity adapted.
      TSEntry(timestamp, value, at - timestamp)
  
  /** Move this entry's timestamp to 'at' and shorten the validity accordingly,
   *  if this entry is defined at 'at'. */
  def trimLeft(at: Long) =
    if(at >= definedUntil) // Nothing left from the value on the right side of the trim 
      EmptyTimeSeries()
    else 
      trimEntryLeft(at)
  
  /** Similar to trimLeft, but returns a TSEntry instead of a time series and throws 
   *  if 'at' exceeds the entry's definition. */
  def trimEntryLeft(at: Long) =
    if(at >= definedUntil) // Nothing left from the value on the right side of the trim 
      throw new IllegalArgumentException(
          s"Attempting to trim left at $at after entry's domain has ended ($definedUntil)")
    else if (at <= timestamp) // Trim before or exactly on value start, right side remains unchanged
      this
    else // Entry needs to have its timestamp and validity adapted.
      TSEntry(at, value, definedUntil - at)
      
  /** Equivalent to calling trimEntryLeft(l).trimEntryRight(r)
   *  without the intermediary step. */
  def trimEntryLeftNRight(l: Long, r: Long) =
      if(l >= definedUntil)
        throw new IllegalArgumentException(
          s"Attempting to trim left at $l after entry's domain has ended ($definedUntil)")
      else if(r <= timestamp)
        throw new IllegalArgumentException(
          s"Attempting to trim right at $r before entry's domain has started ($timestamp)")
      else if(l >= r)
        throw new IllegalArgumentException(
            s"Left time must be strictly lower than right time. Was: $l and $r")
      else if(l <= timestamp && r >= definedUntil)
        this
      else {
        val start = Math.max(timestamp, l) 
        TSEntry(start, value, Math.min(validity, r - start))
      }
        
  
  def defined(at: Long): Boolean = at >= timestamp && at < definedUntil
  
  /** the last moment where this entry is valid, non-inclusive */
  def definedUntil(): Long = timestamp + validity
  
  /** return true if this and the other entry have an overlapping domain of definition.
   *  False if the domains are only contiguous.*/
  def overlaps[O](other: TSEntry[O]) : Boolean = 
    this.timestamp < other.definedUntil && this.definedUntil > other.timestamp
    
  def toLeftEntry[O]: TSEntry[Either[T, O]] = TSEntry(timestamp, Left[T,O](value), validity)
  
  def toRightEntry[O]: TSEntry[Either[O, T]] = TSEntry(timestamp, Right[O,T](value), validity)
  
  /** Map value contained in this timeseries using the passed function */
  def map[O](f: T => O) = TSEntry(timestamp, f(value), validity)
      
}

object TSEntry {
  /** Build a TSEntry from a tuple containing the a TSValue and the time at which it starts.*/
  def apply[T](tValTup: (Long, TSValue[T])): TSEntry[T] = 
    TSEntry(tValTup._1, tValTup._2.value, tValTup._2.validity)
    
  /** Merge two overlapping TSEntries and return the result as an
   *  ordered sequence of TSEntries. 
   *    
   *  This method returns a Seq containing one to three TSEntries defining a timeseries valid from
   *  first.timestamp to max(first.validUntil, second.validUntil).
   *    - one entry if first and second share the exact same domain
   *    - two entries if first and second share one bound of their domain, 
   *    - three entries if the domains overlap without sharing a bound
   *    
   *  If the passed merge operator is commutative, then the 'merge' function is commutative as well.
   *  (merge(op)(E_a,E_b) == merge(op)(E_b,E_a) only if op(a,b) == op(b,a))
   */
  def mergeOverlapping[A,B,R]
    (a: TSEntry[A], b: TSEntry[B])
    (op: (Option[A], Option[B]) => Option[R])
    : Seq[TSEntry[R]] =
      {    
        // Handle first 'partial' definition
        (Math.min(a.timestamp, b.timestamp), Math.max(a.timestamp, b.timestamp)) match {
        case (from, to) if (from != to) => 
          // Compute the result of the merge operation for a partially defined input (either A or B is undefined for this segment)
          mergeValues(a,b)(from, to)(op)
        case _ => Seq.empty // a and b start at the same time. Nothing to do
        }
      } ++ {  
        // Merge the two values over the overlapping domain of definition of a and b.
        (Math.max(a.timestamp, b.timestamp), Math.min(a.definedUntil, b.definedUntil)) match {
        case (from, to) if (from < to) => mergeValues(a,b)(from,to)(op)
        case _ => throw new IllegalArgumentException("This function cannot merge non-overlapping entries.")
        }
      } ++ {
        // Handle trailing 'partial' definition
        (Math.min(a.definedUntil(), b.definedUntil()), Math.max(a.definedUntil(), b.definedUntil())) match {
          case (from, to) if (from != to) => mergeValues(a,b)(from, to)(op)
          case _ => Seq.empty; // Entries end at the same time, nothing to do.
        }
      }
  
  /** Merge two entries that have a disjoint domain. 
   *  The merge operator will be applied to each individually */
  def mergeDisjointDomain[A,B,R]
    (a: TSEntry[A], b: TSEntry[B])
    (op: (Option[A], Option[B]) => Option[R])
    : Seq[TSEntry[R]] =
          { op(Some(a.value), None).map(TSEntry(a.timestamp, _, a.validity)).toSeq ++ 
            emptyApply(Math.min(a.definedUntil, b.definedUntil), Math.max(a.timestamp, b.timestamp))(op).toSeq ++ 
            op(None, Some(b.value)).map(TSEntry(b.timestamp, _, b.validity)).toSeq
          }.sortBy(_.timestamp)
      
  private def emptyApply[A,B,R]
    (from: Long, to: Long)
    (op: (Option[A], Option[B]) => Option[R])
    : Option[TSEntry[R]] =
      if(from == to)
        None
      else
        op(None, None).map(TSEntry(from, _, to - from))
      
  /** Merge two entries.
   *  The domain covered by the returned entries (including a potential discontinuities)
   *  will be between min(a.timestamp, b.timestamp) and max(a.definedUntil, b.definedUntil) */
  def merge[A,B,R]
    (a: TSEntry[A], b: TSEntry[B])
    (op: (Option[A], Option[B]) => Option[R])
    : Seq[TSEntry[R]] = 
      if(!a.overlaps(b))
         mergeDisjointDomain(a, b)(op)
      else 
         mergeOverlapping(a,b)(op)
  
  /** Merge two TSEntries each containing an Either[A,B]
   *  Simply calls the normal merge function after determining which of the entries contains what type. */
  def mergeEithers[A,B,R]
    (a: TSEntry[Either[A,B]], b: TSEntry[Either[A,B]])
    (op: (Option[A], Option[B]) => Option[R])
    : Seq[TSEntry[R]] =
      (a,b) match {
      case (TSEntry(tsA, Left(valA), dA), TSEntry(tsB, Right(valB), dB)) => 
        mergeOverlapping(TSEntry(tsA, valA, dA), TSEntry(tsB, valB, dB))(op)
      case (TSEntry(tsB, Right(valB), dB), TSEntry(tsA, Left(valA), dA)) =>
        mergeOverlapping(TSEntry(tsA, valA, dA), TSEntry(tsB, valB, dB))(op)
      case _ => throw new IllegalArgumentException(s"Can't pass two entries with same sided-eithers: $a, $b")
      }
  
  /** Merge the 'single' TSEntry to the 'others'.
   *  The domain of definition of the 'single' entry is used: 
   *  non-overlapping parts of the other entries will not be merged.*/
  def mergeSingleToMultiple[A,B,R]
    (single: TSEntry[Either[A,B]], others: Seq[TSEntry[Either[A,B]]])
    (op: (Option[A], Option[B]) => Option[R])
    : Seq[TSEntry[R]] =
    others match {
            case Seq() => Seq.empty
            case Seq(alone) => mergeEithers(single, alone.trimEntryLeftNRight(single.timestamp, single.definedUntil))(op)
            case _ =>
              // Take care of the potentially undefined domain before the 'others'
              mergeEitherToNone(single)(single.timestamp, others.head.timestamp)(op) ++ 
              // Merge the others to the single entry, including potential undefined spaces between them
              others.map(_.trimEntryLeftNRight(single.timestamp, single.definedUntil))
                            // Group by pairs of entries to be able to trim the single one to the relevant domain 
                            // for the individual merges  
                            .sliding(2)
                            .flatMap(p => mergeEithers(single.trimEntryLeftNRight(p.head.timestamp, p.last.timestamp), p.head)(op)) ++
              // Take care of the potentially undefined domain after the others. 
              mergeEitherToNone(single)(others.last.definedUntil, single.definedUntil)(op)
          }
  
  /** Convenience function to merge the values present in the entries at time 'at' and
   *  create an entry valid until 'until' from the result, if the merge operation is defined
   *  for the input. */
  private def mergeValues[A, B, R]
    (a: TSEntry[A], b: TSEntry[B])
    (at: Long, until: Long)
    (op: (Option[A], Option[B]) => Option[R])
    : Seq[TSEntry[R]] = 
      op(a.at(at), b.at(at)).map(TSEntry(at, _ , until - at)).toSeq
      
  /** Merge the provided entry to a None */
  def mergeEitherToNone[A, B, R]
    (e: TSEntry[Either[A,B]])
    (at: Long, until: Long)
    (op: (Option[A], Option[B]) => Option[R])
    : Seq[TSEntry[R]] =
      if(at == until)
        Seq.empty
      else 
        {e match {
          case TSEntry(t, Left(valA), d) => op(valueAt(t, valA, d)(at), None)
          case TSEntry(t, Right(valB), d) => op(None, valueAt(t, valB, d)(at))
        }}.map(TSEntry(at, _, until - at)).toSeq
  
  /** 
   *  For an entry starting at time t with the given validity, return 'value'
   *  if 'at' is within the domain of definition.*/
  private def valueAt[T](t: Long, value: T, validity: Long)(at: Long) =
    if (t <= at && at < t + validity)
      Some(value)
    else 
      None
      
}