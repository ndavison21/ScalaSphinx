package sphinx.test.clientAndServer.server

import org.junit.Assert
import org.junit.Test

import sphinx.clientAndServer.Client
import sphinx.clientAndServer.SphinxServer
import sphinx.exceptions.MacMismatchException
import sphinx.params.Params

class TestProcessGroup {
  val useEcc = false
  val r = 5
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

  @Test
  def validInput {
    cleanup
    val dest = "nd359@cam.ac.uk"
    val message = "This is a Test"
    val nodeIds = Params.randomSubset(Params.pki.keySet.toArray, r)
    val (header, msg) = Client.creatForwardMessage(Params.stringToByteArray(message),
      Params.stringToByteArray(dest), nodeIds.map { x => Params.stringOfHexToByteArray(x) }, p)
    Params.pki.get(nodeIds(0)).get.process(header, msg)
    Assert.assertTrue(true)
  }

  @Test(expected = classOf[AssertionError])
  def randomInput { //of the correct length
    cleanup
    val header = (new Array[Byte](32), new Array[Byte](176), new Array[Byte](48))
    val msg = new Array[Byte](48)
    Params.pki.get(Params.pki.keySet.toVector(0)).get.process(header, msg)
  }

  @Test(expected = classOf[MacMismatchException])
  def invalidMac {
    cleanup
    val dest = Array.fill[Byte](10)(0.asInstanceOf[Byte])
    val message = Array.fill[Byte](200)(0.asInstanceOf[Byte])
    val nodeIds = Params.randomSubset(Params.pki.keySet.toArray, r)
    val ((a, b, g), msg) = Client.creatForwardMessage(message, dest, nodeIds.map { x => Params.stringOfHexToByteArray(x) }, p)
    val header = (a, b, new Array[Byte](g.length))
    Params.pki.get(nodeIds(0)).get.process(header, msg)
  }

  @Test
  def repeatedMessage {
    cleanup
    val dest = Array.fill[Byte](10)(0.asInstanceOf[Byte])
    val message = Array.fill[Byte](200)(0.asInstanceOf[Byte])
    val nodeIds = Params.randomSubset(Params.pki.keySet.toArray, r)
    val ((a, b, g), msg) = Client.creatForwardMessage(message, dest, nodeIds.map { x => Params.stringOfHexToByteArray(x) }, p)

    val node = Params.pki.get(nodeIds(0)).get
    node.process((a, b, g), msg)

    val s = p.group.expon(a, node.x)
    // Have we seen it already?
    val tag = Params.byteArrayToStringOfHex(Params.tauHash(s, p))
    Assert.assertTrue(node.seen.contains(tag))
  }

  /**
   * Server is an intermeiate node
   */
  @Test
  def nodeCaseOfProcessing {
    cleanup
    cleanup
    val dest = "nd359@cam.ac.uk"
    val message = "This is a Test"
    val nodeIds = Params.randomSubset(Params.pki.keySet.toArray, 2)
    val (header, msg) = Client.creatForwardMessage(Params.stringToByteArray(message),
      Params.stringToByteArray(dest), nodeIds.map { x => Params.stringOfHexToByteArray(x) }, p)
    Params.pki.get(nodeIds(0)).get.process(header, msg)
    Assert.assertTrue(true)
  }

  /**
   * Server is an exit node of forward message
   */
  @Test
  def destCaseOfProcessing {
    cleanup
    val dest = "nd359@cam.ac.uk"
    val message = "This is a Test"
    val nodeIds = Params.randomSubset(Params.pki.keySet.toArray, 1)
    val (header, msg) = Client.creatForwardMessage(Params.stringToByteArray(message),
      Params.stringToByteArray(dest), nodeIds.map { x => Params.stringOfHexToByteArray(x) }, p)
    Params.pki.get(nodeIds(0)).get.process(header, msg)
    Assert.assertTrue(true)
  }

  /**
   * Server is an exit node of a reply message
   */
  @Test
  def dSpecCaseOfProcessing {
    val pseudonym = "nd359"
    client.createPseudonymReply(Params.stringToByteArray(pseudonym), 1)
    val replyMessage = "This is a reply test"
    Params.pseudonymServer.sendToPseudonym(Params.stringToByteArray(pseudonym), Params.stringToByteArray(replyMessage))
    Assert.assertTrue(true)
  }

}