package ch.shastick

import org.junit.Test
import org.scalatest.junit.JUnitSuite
import ch.shastick.immutable.{TSEntry, TreeMapTimeSeries}
import NumericTimeSeries.slidingSum

/**
 * Assumes the merge logic to be well tested: 
 * just check that the general numerical operations
 * are implemented correctly.
 */
class NumericTimeSeriesTest extends JUnitSuite {
  
   val tsa = TreeMapTimeSeries(
        1L -> (1.0, 10L),
        // Leave a gap of 1 between the two entries
        12L -> (2.0, 10L))  
        
   val tsb = TreeMapTimeSeries(
        6L -> (3.0, 10L))

  /**
   * Check that we only have a correct sum wherever 
   * both time series are defined at the same time.
   */
  @Test def testStrictPlus {
    
    assert(tsa + tsb == tsb + tsa)
    
    assert(Seq(TSEntry(6, 4.0, 5), TSEntry(12, 5.0, 4))
        == (tsa + tsb).entries)
  }
  
  /**
   * Check that we only have a correct subtraction wherever 
   * both time series are defined at the same time.
   */
  @Test def testStrictMinus {
    
    assert(Seq(TSEntry(6, -2.0, 5), TSEntry(12, -1.0, 4))
        == tsa.minus(tsb).entries)
    
    assert(Seq(TSEntry(6, 2.0, 5), TSEntry(12, 1.0, 4))
        == tsb.minus(tsa).entries)
  }
  
  /**
   * Check that we only have a correct multiplication wherever 
   * both time series are defined at the same time.
   */
  @Test def testStrictMultiply {
    
    assert(tsa * tsb == tsb * tsa)
    
    assert(Seq(TSEntry(6, 3.0, 5), TSEntry(12, 6.0, 4))
        == (tsb * tsa).entries)
  }

  @Test def testUnitWindowSliding: Unit = {
    // A window of size 1 should be like applying a unitary operator
    // Case with two contiguous entries each of duration 1
    // We want this due to the inherent discrete nature of our time series representation

    // Starting at 0
    val twoA = Seq(
      TSEntry(0, 1, 1),
      TSEntry(1, 2, 1)
    )

    assert(
      slidingSum(twoA, 1) == twoA
    )

    // Starting at > 0
    val twoB = Seq(
      TSEntry(10, 1, 1),
      TSEntry(11, 2, 1)
    )

    assert(
      slidingSum(twoB, 1) == twoB
    )

    // With durations > 1
    val twoC = Seq(
      TSEntry(10, 1, 2),
      TSEntry(12, 2, 2)
    )

    assert(
      slidingSum(twoC, 1) == twoC
    )

    // With a gap
    val twoD = Seq(
      TSEntry(10, 1, 1),
      TSEntry(12, 2, 1)
    )

    assert(
      slidingSum(twoD, 1) == twoD
    )

    // With a gap and duration > 1
    val twoE = Seq(
      TSEntry(10, 1, 2),
      TSEntry(13, 2, 2)
    )

    assert(
      slidingSum(twoE, 1) == twoE
    )

    // Three entries, of duration 1, two of then contiguous
    val tri = Seq(
      TSEntry(10, 1, 1),
      TSEntry(11, 2, 1),
      TSEntry(13, 2, 1)
    )

    assert(
      slidingSum(tri, 1) == tri
    )
  }


  @Test def testSimpleSlidingSumContinuous: Unit = {
    // Test pair-wise continuous entries

    // Simple corner case
    assert(NumericTimeSeries.slidingSum[Int](Seq(), 10).isEmpty)

    // Case with one entry
    val single = Seq(TSEntry(10, 1, 10))

    assert(
      slidingSum(single, 1) == single
    )
    assert(
      slidingSum(single, 10) == single
    )
    assert(
      slidingSum(single, 20) == single
    )

    // Case with two contiguous entries
    val twoA = Seq(
      TSEntry(10, 1, 5),
      TSEntry(15, 2, 10)
    )

    // Window shorter than the shortest entry
    assert(
      slidingSum(twoA, 2) ==
        Seq(
          TSEntry(10, 1, 5),
          TSEntry(15, 3, 1),
          TSEntry(16, 2, 9)
        )
    )
    assert(
      slidingSum(twoA, 4) ==
        Seq(
          TSEntry(10, 1, 5),
          TSEntry(15, 3, 3),
          TSEntry(18, 2, 7)
        )
    )

    // Window equal to the shortest entry
    assert(
      slidingSum(twoA, 5) ==
        Seq(
          TSEntry(10, 1, 5),
          TSEntry(15, 3, 4),
          TSEntry(19, 2, 6)
        )
    )

    // Window equal to the longest entry
    assert(
      slidingSum(twoA, 10) ==
        Seq(
          TSEntry(10, 1, 5),
          TSEntry(15, 3, 9),
          TSEntry(24, 2, 1)
        )
    )

    // Window longer than the longest entry
    assert(
      slidingSum(twoA, 11) ==
        Seq(
          TSEntry(10, 1, 5),
          TSEntry(15, 3, 10)
        )
    )

    assert(
      slidingSum(twoA, 12) ==
        Seq(
          TSEntry(10, 1, 5),
          TSEntry(15, 3, 10)
        )
    )

    // Case with two contiguous entries
    val twoB = Seq(
      TSEntry(10, 1, 10),
      TSEntry(20, 2, 5)
    )

    // Window shorter than the shortest entry
    assert(
      slidingSum(twoB, 2) ==
        Seq(
          TSEntry(10, 1, 10),
          TSEntry(20, 3, 1),
          TSEntry(21, 2, 4)
        )
    )
    assert(
      slidingSum(twoB, 4) ==
        Seq(
          TSEntry(10, 1, 10),
          TSEntry(20, 3, 3),
          TSEntry(23, 2, 2)
        )
    )

    // Window equal to the shortest entry
    assert(
      slidingSum(twoB, 5) ==
        Seq(
          TSEntry(10, 1, 10),
          TSEntry(20, 3, 4),
          TSEntry(24, 2, 1)
        )
    )

    // Window equal to the longest entry
    assert(
      slidingSum(twoB, 10) ==
        Seq(
          TSEntry(10, 1, 10),
          TSEntry(20, 3, 5)
        )
    )

    // Window longer than the longest entry
    assert(
      slidingSum(twoB, 11) ==
        Seq(
          TSEntry(10, 1, 10),
          TSEntry(20, 3, 5)
        )
    )

    assert(
      slidingSum(twoB, 12) ==
        Seq(
          TSEntry(10, 1, 10),
          TSEntry(20, 3, 5)
        )
    )

  }

  @Test def testTripleContinuousEntriesSum: Unit = {
    // Test triple continuous entries with various configurations

   val triA =
     Seq(
       TSEntry(10, 1, 10),
       TSEntry(20, 2, 2),
       TSEntry(22, 3, 10)
     )

    assert(
      slidingSum(triA, 2) ==
        Seq(
          TSEntry(10, 1, 10),
          TSEntry(20, 3, 1),
          TSEntry(21, 2, 1),
          TSEntry(22, 5, 1),
          TSEntry(23, 3, 9)
        )
    )

    assert(
      slidingSum(triA, 3) ==
        Seq(
          TSEntry(10, 1, 10),
          TSEntry(20, 3, 2),
          TSEntry(22, 5, 2),
          TSEntry(24, 3, 8)
        )
    )

    assert(
      slidingSum(triA, 4) ==
        Seq(
          TSEntry(10, 1, 10),
          TSEntry(20, 3, 2),
          TSEntry(22, 6, 1),
          TSEntry(23, 5, 2),
          TSEntry(25, 3, 7)
        )
    )

    assert(
      slidingSum(triA, 10) ==
        Seq(
          TSEntry(10, 1, 10),
          TSEntry(20, 3, 2),
          TSEntry(22, 6, 7),
          TSEntry(29, 5, 2),
          TSEntry(31, 3, 1)
        )
    )

    assert(
      slidingSum(triA, 11) ==
        Seq(
          TSEntry(10, 1, 10),
          TSEntry(20, 3, 2),
          TSEntry(22, 6, 8),
          TSEntry(30, 5, 2)
        )
    )

    assert(
      slidingSum(triA, 12) ==
        Seq(
          TSEntry(10, 1, 10),
          TSEntry(20, 3, 2),
          TSEntry(22, 6, 9),
          TSEntry(31, 5, 1)
        )
    )

    assert(
      slidingSum(triA, 13) ==
        Seq(
          TSEntry(10, 1, 10),
          TSEntry(20, 3, 2),
          TSEntry(22, 6, 10)
        )
    )

    assert(
      slidingSum(triA, 14) ==
        Seq(
          TSEntry(10, 1, 10),
          TSEntry(20, 3, 2),
          TSEntry(22, 6, 10)
        )
    )

  }

  @Test def testSimpleSlidingSumDisContinuous: Unit = {
    // Test pair-wise discontinuous entries
    val twoA = Seq(
      TSEntry(10, 1, 5),
      TSEntry(17, 2, 10)
    )

    assert(
      slidingSum(twoA, 2) ==
        Seq(
          TSEntry(10, 1, 6),
          TSEntry(17, 2, 10)
        )
    )

    assert(
      slidingSum(twoA, 3) ==
        Seq(
          TSEntry(10, 1, 7),
          TSEntry(17, 2, 10)
        )
    )

    assert(
      slidingSum(twoA, 4) ==
        Seq(
          TSEntry(10, 1, 7),
          TSEntry(17, 3, 1),
          TSEntry(18, 2, 9)
        )
    )

    assert(
      slidingSum(twoA, 12) ==
        Seq(
          TSEntry(10, 1, 7),
          TSEntry(17, 3, 9),
          TSEntry(26, 2, 1)
        )
    )

    assert(
      slidingSum(twoA, 13) ==
        Seq(
          TSEntry(10, 1, 7),
          TSEntry(17, 3, 10)
        )
    )

  }

  @Test def testTripleDiscontinuousEntriesSum: Unit = {
    // Test triple continuous entries with various configurations

    val triA =
      Seq(
        TSEntry(10, 1, 10),
        TSEntry(21, 2, 2),
        TSEntry(24, 3, 10)
      )

    assert(
      slidingSum(triA, 2) ==
        Seq(
          TSEntry(10, 1, 11),
          TSEntry(21, 2, 3),
          TSEntry(24, 3, 10)
        )
    )

    assert(
      slidingSum(triA, 3) ==
        Seq(
          TSEntry(10, 1, 11),
          TSEntry(21, 3, 1),
          TSEntry(22, 2, 2),
          TSEntry(24, 5, 1),
          TSEntry(25, 3, 9)
        )
    )

    assert(
      slidingSum(triA, 4) ==
        Seq(
          TSEntry(10, 1, 11),
          TSEntry(21, 3, 2),
          TSEntry(23, 2, 1),
          TSEntry(24, 5, 2),
          TSEntry(26, 3, 8)
        )
    )

    assert(
      slidingSum(triA, 5) ==
        Seq(
          TSEntry(10, 1, 11),
          TSEntry(21, 3, 3),
          TSEntry(24, 5, 3),
          TSEntry(27, 3, 7)
        )
    )

    assert(
      slidingSum(triA, 6) ==
        Seq(
          TSEntry(10, 1, 11),
          TSEntry(21, 3, 3),
          TSEntry(24, 6, 1),
          TSEntry(25, 5, 3),
          TSEntry(28, 3, 6)
        )
    )

    assert(
      slidingSum(triA, 7) ==
        Seq(
          TSEntry(10, 1, 11),
          TSEntry(21, 3, 3),
          TSEntry(24, 6, 2),
          TSEntry(26, 5, 3),
          TSEntry(29, 3, 5)
        )
    )

    assert(
      slidingSum(triA, 8) ==
        Seq(
          TSEntry(10, 1, 11),
          TSEntry(21, 3, 3),
          TSEntry(24, 6, 3),
          TSEntry(27, 5, 3),
          TSEntry(30, 3, 4)
        )
    )

    assert(
      slidingSum(triA, 10) ==
        Seq(
          TSEntry(10, 1, 11),
          TSEntry(21, 3, 3),
          TSEntry(24, 6, 5),
          TSEntry(29, 5, 3),
          TSEntry(32, 3, 2)
        )
    )

    assert(
      slidingSum(triA, 11) ==
        Seq(
          TSEntry(10, 1, 11),
          TSEntry(21, 3, 3),
          TSEntry(24, 6, 6),
          TSEntry(30, 5, 3),
          TSEntry(33, 3, 1)
        )
    )

    assert(
      slidingSum(triA, 12) ==
        Seq(
          TSEntry(10, 1, 11),
          TSEntry(21, 3, 3),
          TSEntry(24, 6, 7),
          TSEntry(31, 5, 3)
        )
    )

    assert(
      slidingSum(triA, 14) ==
        Seq(
          TSEntry(10, 1, 11),
          TSEntry(21, 3, 3),
          TSEntry(24, 6, 9),
          TSEntry(33, 5, 1)
        )
    )

    assert(
      slidingSum(triA, 15) ==
        Seq(
          TSEntry(10, 1, 11),
          TSEntry(21, 3, 3),
          TSEntry(24, 6, 10)
        )
    )

  }
  
  
}