package sphinx.test.params

import org.junit.Assert
import org.junit.Test
import sphinx.params.Params
import sphinx.exceptions.KeyTooShortException
import sphinx.exceptions.DataTooShortException

class TestPi {
  
  @Test(expected = classOf[KeyTooShortException])
  def testNullKey {
    val data = Array.fill[Byte](Params.k * 2)(0.asInstanceOf[Byte])
    val out = Params.pi(null, data)
  }
  
  @Test(expected = classOf[KeyTooShortException])
  def testShortKey {
    val key = Array.fill[Byte](1)(0.asInstanceOf[Byte])
    val data = Array.fill[Byte](Params.k * 2)(0.asInstanceOf[Byte])
    val out = Params.pi(key, data)
  }
  
  @Test(expected = classOf[DataTooShortException])
  def testNullData {
    val key = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte])
    val out = Params.pi(key, null)
  }
  
  @Test(expected = classOf[DataTooShortException])
  def testShortData {
    val key = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte])
    val data = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte]) // should be >=2*k
    val out = Params.pi(key, data)
  }
  
  @Test
  def testOutputNotNull {
    val key = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte])
    val data = Array.fill[Byte](Params.k * 2)(0.asInstanceOf[Byte])
    val out = Params.pi(key, data)
    Assert.assertNotNull(out)
  }
  
  @Test
  def testOutputLength {
    val key = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte])
    val data = Array.fill[Byte](Params.k * 2)(0.asInstanceOf[Byte])
    val out = Params.pi(key, data)
    Assert.assertEquals(data.length, out.length)
  }
 
}