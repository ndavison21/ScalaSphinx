package sphinx.test.params

import sphinx.exceptions.MessageTooLargeException
import sphinx.params.Params
import org.junit.Test
import org.junit.Assert
import scala.util.Random

class TestMessageBodyPadding {
  
  @Test(expected = classOf[MessageTooLargeException])
  def sizeSmallerThanMessage {
    val msg = Array.fill[Byte](20)(0.asInstanceOf[Byte])
    Params.padMsgBody(10, msg)
  }
  
  @Test
  def correctPadding {
    val msg = new Array[Byte](10)
    Random.nextBytes(msg)
    val out = Params.padMsgBody(100, msg)
    val padding = (127).asInstanceOf[Byte] +: Array.fill[Byte]((out.length - msg.length) - 1)((-1).asInstanceOf[Byte])
    Assert.assertArrayEquals(padding, out.slice(msg.length, out.length))
  }
  
  @Test
  def correctMessage {
    val msg = new Array[Byte](10)
    Random.nextBytes(msg)
    val out = Params.padMsgBody(100, msg)
    Assert.assertArrayEquals(msg, out.slice(0, msg.length))
  }
  
  @Test
  def unpadUnpadded {
    val msg = new Array[Byte](10)
    do {
      Random.nextBytes(msg)
    } while (msg(9) == 127)
      
    val out = Params.unpadMsgBody(msg)
    Assert.assertArrayEquals(msg, out)
  }
  
  @Test
  def unpadPadded {
    val msg = new Array[Byte](10)
    Random.nextBytes(msg)
    val out = Params.unpadMsgBody(Params.padMsgBody(50, msg))
    Assert.assertArrayEquals(msg, out)
  }
  
}