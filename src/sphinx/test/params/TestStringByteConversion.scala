package sphinx.test.params

import org.junit.Test
import sphinx.params.Params
import org.junit.Assert
import scala.util.Random

class TestStringByteConversion {
  
  @Test
  def testStringToByteToString {
    val s = "fooBar!?$"
    val out = Params.byteArrayToString(Params.stringToByteArray(s))
    Assert.assertEquals(s, out)
  }
  
  @Test
  def testByteToStringToByte {
    val b = new Array[Byte](10)
    Random.nextBytes(b)
    val out = Params.stringToByteArray(Params.byteArrayToString(b))
    Assert.assertArrayEquals(b, out)
  }
  
  
}