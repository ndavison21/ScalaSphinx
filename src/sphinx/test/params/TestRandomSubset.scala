package sphinx.test.params

import scala.util.Random

import org.junit.Assert
import org.junit.Test

import sphinx.exceptions.InvalidSetException
import sphinx.params.Params

class TestRandomSubset {
  
  /** Testing for lists **/
  
  @Test
  def testListOutputLength {
    val n = 5
    val in = Seq.fill(20)(Random.nextInt.toByte).toList
    val out = Params.randomSubset(in, n)
    Assert.assertEquals(n, out.length)
  }
  
  @Test
  def testListOutputIsSubset {
    val n = 5
    val in = Seq.fill(20)(Random.nextInt.toByte).toList
    val out = Params.randomSubset(in, n)
    
    out.foreach { x => if (!in.contains(x)) Assert.fail()}
    
    Assert.assertEquals(true, true)
  }
  
  @Test(expected = classOf[InvalidSetException])
  def testListNullInput {
    val n = 5
    val out = Params.randomSubset(null:List[Byte], n)
  }
  
  @Test(expected = classOf[InvalidSetException])
  def testListEmptyInput {
    val n = 5
    val list = Nil
    val out = Params.randomSubset(list, n)
  }
  
  /** Testing for Arrays **/
  
  @Test
  def testArrOutputLength {
    val n = 5
    val in = Array.fill(20)(Random.nextInt.toByte)
    val out = Params.randomSubset(in, n)
    Assert.assertEquals(n, out.length)
  }
  
  @Test
  def testArrOutputIsSubset {
    val n = 5
    val in = Array.fill(20)(Random.nextInt.toByte)
    val out = Params.randomSubset(in, n)
    
    out.foreach { x => if (!in.contains(x)) Assert.fail()}
    
    Assert.assertEquals(true, true)
  }
  
  @Test(expected = classOf[InvalidSetException])
  def testArrNullInput {
    val n = 5
    val out = Params.randomSubset(null:Array[Byte], n)
  }
  
  @Test(expected = classOf[InvalidSetException])
  def testArrEmptyInput {
    val n = 5
    val arr = Array.fill[Byte](0)(0.asInstanceOf[Byte])
    val out = Params.randomSubset(arr, n)
  }
}