package sphinx.test.clientAndServer.client

import sphinx.clientAndServer.Client
import sphinx.params.Params
import sphinx.clientAndServer.SphinxServer
import org.junit.Test
import sphinx.exceptions.DataLengthException
import org.junit.Assert
import sphinx.exceptions.PathLengthException

class TestCreateForwardMessageEcc {
  // Simulating input from command line
  val useEcc = true
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

  @Test(expected = classOf[DataLengthException])
  def dataTooLong {
    cleanup
    val dest = Array.fill[Byte](100)(0.asInstanceOf[Byte])
    val message = Array.fill[Byte](1024)(0.asInstanceOf[Byte])
    val nodeIds = Params.randomSubset(Params.pki.keySet.toArray, r)
    Client.creatForwardMessage(message, dest, nodeIds.map { x => Params.stringOfHexToByteArray(x) }, p)
  }

  //  @Test
  //  def encryptedMessageLength {
  //    cleanup
  //    val dest = Array.fill[Byte](10)(0.asInstanceOf[Byte])
  //    val message = Array.fill[Byte](200)(0.asInstanceOf[Byte])
  //    val nodeIds = Params.randomSubset(Params.pki.keySet.toArray, r)
  //    val (header, msg) = Client.creatForwardMessage(message, dest, nodeIds.map { x => Params.stringOfHexToByteArray(x) }, p)
  //    Assert.assertEquals(message.length, msg.length)
  //  }

  @Test
  def headerLength {
    cleanup
    val dest = Array.fill[Byte](10)(0.asInstanceOf[Byte])
    val message = Array.fill[Byte](200)(0.asInstanceOf[Byte])
    val nodeIds = Params.randomSubset(Params.pki.keySet.toArray, r)
    val ((a, b, k), msg) = Client.creatForwardMessage(message, dest, nodeIds.map { x => Params.stringOfHexToByteArray(x) }, p)
    Assert.assertEquals((2 * r + 1) * Params.k, b.length)
  }

  @Test
  def keyLength {
    cleanup
    val dest = Array.fill[Byte](10)(0.asInstanceOf[Byte])
    val message = Array.fill[Byte](200)(0.asInstanceOf[Byte])
    val nodeIds = Params.randomSubset(Params.pki.keySet.toArray, r)
    val ((a, b, k), msg) = Client.creatForwardMessage(message, dest, nodeIds.map { x => Params.stringOfHexToByteArray(x) }, p)
    Assert.assertEquals(Params.k, k.length)
  }
}