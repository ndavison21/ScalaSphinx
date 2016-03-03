package sphinx.test.params

import org.junit.Test
import sphinx.params.Params
import org.junit.Assert

class TestXor {
  @Test
  def xorWithZeros {
    val in1 = new Array[Byte](10)
    val in2 = Array.fill[Byte](10)(0.asInstanceOf[Byte])
    val out = Params.xor(in1, in2)
    
    Assert.assertArrayEquals(in1, out)
  }
  
  @Test
  def differentLengths1 {
    val in1 = new Array[Byte](10)
    val in2 = Array.fill[Byte](50)(0.asInstanceOf[Byte])
    val out = Params.xor(in1, in2)
    
    Assert.assertArrayEquals(in1, out)
  }
  
    @Test
  def differentLengths2 {
    val in1 = new Array[Byte](10)
    val in2 = Array.fill[Byte](50)(0.asInstanceOf[Byte])
    val out = Params.xor(in2, in1)
    
    Assert.assertArrayEquals(in1, out)
  }
}