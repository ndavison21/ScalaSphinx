package sphinx.test.params

import sphinx.params.Params
import org.junit.Test
import sphinx.exceptions.KeyLengthException
import sphinx.exceptions.DestinationLengthException
import org.junit.Assert

/**
 * Testing the encoding of external destinations (email addresses)
 */
class TestDestinationEncode {
  @Test(expected = classOf[DestinationLengthException])
  def testNullDestination {
    val out = Params.destinationEncode(null)
  }
  
  @Test(expected = classOf[DestinationLengthException])
  def testEmptyDestination {
    val dest = Array.fill[Byte](0)(0.asInstanceOf[Byte])
    val out = Params.destinationEncode(dest)
  }
  
  @Test(expected = classOf[DestinationLengthException])
  def testLongDestination {
    val dest = Array.fill[Byte](128)(0.asInstanceOf[Byte])
    val out = Params.destinationEncode(dest)
  }
  
  @Test
  def prefixLength {
    val dest = Array.fill[Byte](10)(0.asInstanceOf[Byte])
    val out = Params.destinationEncode(dest)
    Assert.assertEquals(out(0), dest.length)
  }
  
  @Test
  def postfixDestination {
    val dest = Array.fill[Byte](10)(0.asInstanceOf[Byte])
    val out = Params.destinationEncode(dest)
    Assert.assertArrayEquals(out.slice(1, out.length), dest)
  }
  
  @Test
  def testDistinguishedDestination {
    val expected = Array.fill[Byte](1)(0.asInstanceOf[Byte])
    Assert.assertArrayEquals(expected, Params.dSpecial)
  }
}