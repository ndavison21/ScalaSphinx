package sphinx.test.clientAndServer.client

import org.junit.Assert
import org.junit.Test

import sphinx.clientAndServer.Client
import sphinx.clientAndServer.SphinxServer
import sphinx.exceptions.DataLengthException
import sphinx.exceptions.PathLengthException
import sphinx.params.Params

class TestCreateSURB {

  // Simulating input from command line
  val useEcc = false
  val r = 5 // path length
  val p = new Params(r, useEcc)
  
  // Create a client
  val client = new Client(new Params(r, useEcc))

  def cleanup {
    // Cleaning up
    Params.pki.clear()
    Params.pseudonymServer.db.clear()

    // Create some sphinx servers (they add themselves to the pki)
    for (i <- 0 to r * 2) {
      new SphinxServer(new Params(r, useEcc))
    }

  }
  
  @Test(expected = classOf[PathLengthException])
  def pathTooLong {
    cleanup
    val dest = Array.fill[Byte](10)(0.asInstanceOf[Byte])
    val nodeIds = Params.pki.keySet.toArray
    Client.createSurb(dest, nodeIds.map { x => Params.stringOfHexToByteArray(x) }, p)
  }

  @Test
  def keyTupleLength {
    cleanup
    val dest = Array.fill[Byte](10)(0.asInstanceOf[Byte])
    val nodeIds = Params.randomSubset(Params.pki.keySet.toArray, r)
    val (id, keyTuple, header) = Client.createSurb(dest, nodeIds.map { x => Params.stringOfHexToByteArray(x) }, p)
    Assert.assertEquals(r+1, keyTuple.length)
  }
  
  @Test
  def firstNodeInPath {
    cleanup
    val dest = Array.fill[Byte](10)(0.asInstanceOf[Byte])
    val nodeIds = Params.randomSubset(Params.pki.keySet.toArray, r)
    val (id, keyTuple, (n, h, k)) = Client.createSurb(dest, nodeIds.map { x => Params.stringOfHexToByteArray(x) }, p)
    Assert.assertEquals(nodeIds(0), Params.byteArrayToStringOfHex(n))
  }
  
  @Test
  def addToLocalTable {
    cleanup
    val elementsBefore = client.keyTable.size
    client.createPseudonymReply(r)
    Assert.assertEquals(elementsBefore + 1, client.keyTable.size)
  }
  
  @Test
  def addToNymServer {
    cleanup
    val nym = client.createPseudonymReply(r)
    Assert.assertTrue(Params.pseudonymServer.db.contains(nym))
  }
}