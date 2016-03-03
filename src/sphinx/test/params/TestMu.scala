package sphinx.test.params

import sphinx.params.Params
import org.junit.Assert
import org.junit.Test
import sphinx.exceptions.KeyLengthException
import sphinx.exceptions.DataLengthException

class TestMu {
  val r = 5
  val p = new Params(r, true)
  
  @Test(expected = classOf[KeyLengthException])
  def testNullKey {
    val data = Array.fill[Byte](50)(0.asInstanceOf[Byte])
    val out = Params.mu(null, data, p)
  }
  
  @Test(expected = classOf[DataLengthException])
  def testNullData {
    val key = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte])
    val out = Params.mu(key, null, p)
  }
  
  @Test(expected = classOf[KeyLengthException])
  def testShortKey {
    val key = Array.fill[Byte](1)(0.asInstanceOf[Byte])
    val data = Array.fill[Byte](50)(0.asInstanceOf[Byte])
    val out = Params.mu(key, data, p)
  }
  
  @Test
  def testOutputLength {
    val key = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte])
    val data = Array.fill[Byte](50)(0.asInstanceOf[Byte])
    val out = Params.mu(key, data, p)
    Assert.assertEquals(Params.k, out.length) // HMAC SHA-256 truncated to length of k
  }
  
  @Test
  def testMuKeyOutputLength {
    val key = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte])
    val out = Params.muKey(key, p)
    Assert.assertEquals(Params.k, out.length)
  }
}