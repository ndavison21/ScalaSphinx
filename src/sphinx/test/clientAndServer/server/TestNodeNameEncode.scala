package sphinx.test.clientAndServer.server

import scala.util.Random
import org.junit.Assert
import org.junit.Test
import sphinx.params.Params
import sphinx.test.dissertation.SphinxServerModified
import sphinx.exceptions.IdLengthException

class TestNodeNameEncode {
  def cleanup {
    // Cleaning up
    Params.pki.clear()
    Params.pseudonymServer.db.clear()
  }
  
  @Test
  def validInput {
    cleanup
    val id = new Array[Byte](5)
    Random.nextBytes(id)
    val shouldBe = Array[Byte]((-1).asInstanceOf[Byte]) ++ id ++ Array.fill(Params.k - (id.length + 1))(0.asInstanceOf[Byte])
    
    val server = new SphinxServerModified(new Params(5, true))
    val name = server.nodeNameEncode(id)
    Assert.assertArrayEquals(name, shouldBe)
  }
  
  @Test
  def nullInput {
    cleanup
    
    val id = new Array[Byte](0)
    val shouldBe = Array[Byte]((-1).asInstanceOf[Byte]) ++ id ++ Array.fill(Params.k - (id.length + 1))(0.asInstanceOf[Byte])
    
    val server = new SphinxServerModified(new Params(5, true))
    val name = server.nodeNameEncode(null)
    Assert.assertArrayEquals(name, shouldBe)
  }
  
  @Test(expected = classOf[IdLengthException])
  def tooLong {
    cleanup
    
    val id = new Array[Byte](50)
    Random.nextBytes(id)
    val server = new SphinxServerModified(new Params(5, true))
    server.nodeNameEncode(id)
  }
}