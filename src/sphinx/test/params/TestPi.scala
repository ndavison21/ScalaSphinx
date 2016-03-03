package sphinx.test.params

import org.junit.Assert
import org.junit.Test
import sphinx.params.Params
import sphinx.exceptions.KeyTooShortException
import sphinx.exceptions.DataTooShortException

/**
 * Testing of the (family) of pseudo-random permutations, pi
 * Implemented using the LIONESS pseudo-random permutation, defined by R. Anderson and E. Biham
 */
class TestPi {
  
  /* Checking Encryption */
  
  @Test(expected = classOf[KeyTooShortException])
  def testPiNullKey {
    val data = Array.fill[Byte](Params.k * 2)(0.asInstanceOf[Byte])
    val out = Params.pi(null, data)
  }
  
  @Test(expected = classOf[KeyTooShortException])
  def testPiShortKey {
    val key = Array.fill[Byte](1)(0.asInstanceOf[Byte])
    val data = Array.fill[Byte](Params.k * 2)(0.asInstanceOf[Byte])
    val out = Params.pi(key, data)
  }
  
  @Test(expected = classOf[DataTooShortException])
  def testPiNullData {
    val key = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte])
    val out = Params.pi(key, null)
  }
  
  @Test(expected = classOf[DataTooShortException])
  def testPiShortData {
    val key = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte])
    val data = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte]) // should be >=2*k
    val out = Params.pi(key, data)
  }
  
  @Test
  def testPiOutputNotNull {
    val key = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte])
    val data = Array.fill[Byte](Params.k * 2)(0.asInstanceOf[Byte])
    val out = Params.pi(key, data)
    Assert.assertNotNull(out)
  }
  
  @Test
  def testPiOutputLength {
    val key = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte])
    val data = Array.fill[Byte](Params.k * 2)(0.asInstanceOf[Byte])
    val out = Params.pi(key, data)
    Assert.assertEquals(data.length, out.length)
  }
  
  
  /* Checking decryption */
  
  @Test(expected = classOf[KeyTooShortException])
  def testPiiNullKey {
    val data = Array.fill[Byte](Params.k * 2)(0.asInstanceOf[Byte])
    val out = Params.pii(null, data)
  }
  
  @Test(expected = classOf[KeyTooShortException])
  def testPiiShortKey {
    val key = Array.fill[Byte](1)(0.asInstanceOf[Byte])
    val data = Array.fill[Byte](Params.k * 2)(0.asInstanceOf[Byte])
    val out = Params.pii(key, data)
  }
  
  @Test(expected = classOf[DataTooShortException])
  def testPiiNullData {
    val key = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte])
    val out = Params.pii(key, null)
  }
  
  @Test(expected = classOf[DataTooShortException])
  def testPiiShortData {
    val key = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte])
    val data = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte]) // should be >=2*k
    val out = Params.pii(key, data)
  }
  
  @Test
  def testPiiOutputNotNull {
    val key = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte])
    val data = Array.fill[Byte](Params.k * 2)(0.asInstanceOf[Byte])
    val out = Params.pii(key, data)
    Assert.assertNotNull(out)
  }
  
  @Test
  def testPiiOutputLength {
    val key = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte])
    val data = Array.fill[Byte](Params.k * 2)(0.asInstanceOf[Byte])
    val out = Params.pii(key, data)
    Assert.assertEquals(data.length, out.length)
  }
 
  
  /** Checking Encryption and Decryption Together **/
  
  @Test
  def testEncDec {
    val key = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte])
    val data = Array.fill[Byte](Params.k * 2)(0.asInstanceOf[Byte])
    
    val out = Params.pii(key, Params.pi(key, data))
    Assert.assertArrayEquals(data, out)
  }
}