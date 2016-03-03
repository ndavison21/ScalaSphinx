package sphinx.test.params

import org.junit.Assert
import org.junit.Test

import sphinx.exceptions.DataLengthException
import sphinx.exceptions.KeyLengthException
import sphinx.params.Params

/**
 * Testing of the (family) of pseudo-random permutations, pi
 * Implemented using the LIONESS pseudo-random permutation, defined by R. Anderson and E. Biham
 */
class TestPi {
  
  /* Checking Encryption */
  
  @Test(expected = classOf[KeyLengthException])
  def testPiNullKey {
    val data = Array.fill[Byte](Params.k * 2)(0.asInstanceOf[Byte])
    val out = Params.pi(null, data)
  }
  
  @Test(expected = classOf[KeyLengthException])
  def testPiShortKey {
    val key = Array.fill[Byte](1)(0.asInstanceOf[Byte])
    val data = Array.fill[Byte](Params.k * 2)(0.asInstanceOf[Byte])
    val out = Params.pi(key, data)
  }
  
  @Test(expected = classOf[DataLengthException])
  def testPiNullData {
    val key = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte])
    val out = Params.pi(key, null)
  }
  
  @Test(expected = classOf[DataLengthException])
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
  
  @Test(expected = classOf[KeyLengthException])
  def testPiiNullKey {
    val data = Array.fill[Byte](Params.k * 2)(0.asInstanceOf[Byte])
    val out = Params.pii(null, data)
  }
  
  @Test(expected = classOf[KeyLengthException])
  def testPiiShortKey {
    val key = Array.fill[Byte](1)(0.asInstanceOf[Byte])
    val data = Array.fill[Byte](Params.k * 2)(0.asInstanceOf[Byte])
    val out = Params.pii(key, data)
  }
  
  @Test(expected = classOf[DataLengthException])
  def testPiiNullData {
    val key = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte])
    val out = Params.pii(key, null)
  }
  
  @Test(expected = classOf[DataLengthException])
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