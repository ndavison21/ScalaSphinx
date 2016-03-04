package sphinx.test.clientAndServer.client

import sphinx.params.Params
import sphinx.clientAndServer.Client
import sphinx.clientAndServer.SphinxServer
import org.junit.Test
import sphinx.exceptions.DestinationLengthException
import org.junit.Assert
import sphinx.exceptions.PathLengthException

class TestCreateMessageHeaderGroup {

  // Simulating input from command line
  val useEcc = false
  val r = 5 // path length
  val p = new Params(r, useEcc)

  def cleanup {
    // Cleaning up
    Params.pki.clear()
    Params.pseudonymServer.db.clear()

    // Create some sphinx servers (they add themselves to the pki)
    for (i <- 0 to r * 2) {
      new SphinxServer(new Params(r, useEcc))
    }

    // Create a client
    val client = new Client(new Params(r, useEcc))

  }

  
  @Test(expected = classOf[PathLengthException])
  def pathTooLong {
    cleanup
    val dest = Array.fill[Byte](10)(0.asInstanceOf[Byte])
    val message = Array.fill[Byte](200)(0.asInstanceOf[Byte])
    val nodeIds = Params.pki.keySet.toArray
    Client.creatForwardMessage(message, dest, nodeIds.map { x => Params.stringOfHexToByteArray(x) }, p)
  }
  
  @Test(expected = classOf[DestinationLengthException])
  def nullDestination {
    cleanup
    val dest = null
    val identifier = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte])
    val nodeIds = Params.randomSubset(Params.pki.keySet.toArray, r)
    Client.createMixHeader(dest, identifier, nodeIds.map { x => Params.stringOfHexToByteArray(x) }, p)
  }
  
  @Test(expected = classOf[DestinationLengthException])
  def longDestination {
    cleanup
    val dest = Array.fill[Byte](5 * Params.k)(0.asInstanceOf[Byte])
    val identifier = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte])
    val nodeIds = Params.randomSubset(Params.pki.keySet.toArray, r)
    Client.createMixHeader(dest, identifier, nodeIds.map { x => Params.stringOfHexToByteArray(x) }, p)
  }
  
  @Test
  def testHeaderLength {
    cleanup
    val dest = Array.fill[Byte](20)(0.asInstanceOf[Byte])
    val identifier = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte])
    val nodeIds = Params.randomSubset(Params.pki.keySet.toArray, r)
    val ((a,b,k), s) = Client.createMixHeader(dest, identifier, nodeIds.map { x => Params.stringOfHexToByteArray(x) }, p)
    Assert.assertEquals((2 * r + 1)*Params.k, b.length)
  }
  
  @Test
  def testKeyLength {
    cleanup
    val dest = Array.fill[Byte](20)(0.asInstanceOf[Byte])
    val identifier = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte])
    val nodeIds = Params.randomSubset(Params.pki.keySet.toArray, r)
    val ((a,b,k), s) = Client.createMixHeader(dest, identifier, nodeIds.map { x => Params.stringOfHexToByteArray(x) }, p)
    Assert.assertEquals(Params.k, k.length)
  }
  
  @Test
  def numberOfSecrets {
    cleanup
    val dest = Array.fill[Byte](20)(0.asInstanceOf[Byte])
    val identifier = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte])
    val nodeIds = Params.randomSubset(Params.pki.keySet.toArray, r)
    val ((a,b,k), s) = Client.createMixHeader(dest, identifier, nodeIds.map { x => Params.stringOfHexToByteArray(x) }, p)
    Assert.assertEquals(r, s.length)
  }

}