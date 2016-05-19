package ch.poney

import org.junit.Test
import org.scalatest.junit.JUnitSuite
import ch.poney.immutable.TSEntry
import ch.poney.immutable.TSValue
import ch.poney.immutable.EmptyTimeSeries

class TSEntryTest extends JUnitSuite {
  
  @Test def testAt() {
    assert(!TSEntry(0, "", 10).at(-1).isDefined)
    assert(TSEntry(0, "", 10).at(0) == Some(""))
    assert(TSEntry(0, "", 10).at(9) == Some(""))
    assert(!TSEntry(0, "", 10).at(10).isDefined)
  }
  
  @Test def testDefined() {
    assert(!TSEntry(0, "", 10).defined(-1))
    assert(TSEntry(0, "", 10).defined(0))
    assert(TSEntry(0, "", 10).defined(9))
    assert(!TSEntry(0, "", 10).defined(10))
  }
  
  @Test def testDefinedUntil() {
    assert(TSEntry(1, "", 10).definedUntil() == 11)
  }
  
  @Test def testToMapTuple() {
    assert(TSEntry(0, "Hi", 10).toMapTuple == (0 -> TSValue("Hi",10)))
  }
  
  @Test def testToVal() {
    assert(TSEntry(0, "Hi", 10).toVal == TSValue("Hi",10))
  }
  
  @Test def testTrimRight() {
    val tse = TSEntry(0, "", 10)
    assert(tse.trimRight(10) == tse)
    assert(tse.trimRight(9) == TSEntry(0, "", 9))
    assert(tse.trimRight(1) == TSEntry(0, "", 1))
    assert(tse.trimRight(0) == EmptyTimeSeries())
    assert(tse.trimRight(-1) == EmptyTimeSeries())
  }
  
  @Test def testTrimEntryRight() {
    val tse = TSEntry(0, "", 10)
    assert(tse.trimEntryRight(10) == tse)
    assert(tse.trimEntryRight(9) == TSEntry(0, "", 9))
    assert(tse.trimEntryRight(1) == TSEntry(0, "", 1))
    
    intercept[IllegalArgumentException] {
      tse.trimEntryRight(0)
    }
    intercept[IllegalArgumentException] {
      tse.trimEntryRight(-1)
    }
  }
  
  @Test def testTrimLeft() {
    val tse = TSEntry(0, "", 10)
    assert(tse.trimLeft(-1) == tse)
    assert(tse.trimLeft(0) == tse)
    assert(tse.trimLeft(1) == TSEntry(1, "", 10))
    assert(tse.trimLeft(9) == TSEntry(9, "", 10))
    assert(tse.trimLeft(10) == EmptyTimeSeries())
    assert(tse.trimLeft(11) == EmptyTimeSeries())
  }
  
  @Test def testTrimEntryLeft() {
    val tse = TSEntry(0, "", 10)
    assert(tse.trimEntryLeft(-1) == tse)
    assert(tse.trimEntryLeft(0) == tse)
    assert(tse.trimEntryLeft(1) == TSEntry(1, "", 10))
    assert(tse.trimEntryLeft(9) == TSEntry(9, "", 10))
    intercept[IllegalArgumentException] {
      tse.trimEntryLeft(10)
    }
    intercept[IllegalArgumentException] {
      tse.trimEntryLeft(11)
    }
  }
}