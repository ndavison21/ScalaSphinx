package sphinx.test.params

import sphinx.params.Params
import org.junit.Test
import sphinx.exceptions.KeyLengthException
import org.junit.Assert

class TestRho {
  val r = 5
  val p = new Params(r, true)
  
  @Test(expected = classOf[KeyLengthException])
  def testNullKey {
    val out = Params.rho(null, p)
  }
  
  @Test(expected = classOf[KeyLengthException])
  def testShortKey {
    val key = Array.fill[Byte](1)(0.asInstanceOf[Byte])
    val out = Params.rho(key, p)
  }
  
  @Test
  def testOutputLength {
    val key = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte])
    val out = Params.rho(key, p)
    Assert.assertEquals((2*r+3)*Params.k, out.length) // length is defined in the Sphinx Paper
  }
  
  @Test
  def testRhoKeyOutputLength {
    val key = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte])
    val out = Params.rhoKey(key, p)
    Assert.assertEquals(Params.k, out.length)
  }
}