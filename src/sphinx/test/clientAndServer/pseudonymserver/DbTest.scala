package sphinx.test.clientAndServer.pseudonymserver

import sphinx.clientAndServer.Client
import sphinx.params.Params
import sphinx.clientAndServer.SphinxServer
import org.junit.Test
import org.junit.Assert
import sphinx.exceptions.NoReplyBlocksException

class DbTest {
  val useEcc = true
  val r = 5
  val client = new Client(new Params(r, useEcc))

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

  @Test
  def addingSurb {
    cleanup
    val pseudonym = "nd359"
    val nym = Params.stringToByteArray(pseudonym)
    client.createPseudonymReply(Params.stringToByteArray(pseudonym), r)
    Assert.assertTrue(Params.pseudonymServer.db.contains(Params.byteArrayToStringOfHex(nym)))
  }

  @Test
  def addingMultipleReplyBlocks {
    cleanup
    val pseudonym = "nd359"
    val nym = Params.stringToByteArray(pseudonym)
    client.createPseudonymReply(Params.stringToByteArray(pseudonym), r)
    client.createPseudonymReply(Params.stringToByteArray(pseudonym), r)
    client.createPseudonymReply(Params.stringToByteArray(pseudonym), r)
    
    Assert.assertEquals(Params.pseudonymServer.db.get(Params.byteArrayToStringOfHex(nym)).get.length, 3)
  }

  @Test
  def usingSurb {
    cleanup
    val pseudonym = "nd359"
    val nym = Params.stringToByteArray(pseudonym)
    client.createPseudonymReply(Params.stringToByteArray(pseudonym), r)

    val replyMessage = "This is a reply test"
    Params.pseudonymServer.sendToPseudonym(Params.stringToByteArray(pseudonym), Params.stringToByteArray(replyMessage))
    Assert.assertTrue(true)
  }
  
  @Test
  def usingMultipleSurbs {
    cleanup
    val pseudonym = "nd359"
    val nym = Params.stringToByteArray(pseudonym)
    client.createPseudonymReply(Params.stringToByteArray(pseudonym), r)
    client.createPseudonymReply(Params.stringToByteArray(pseudonym), r)
    client.createPseudonymReply(Params.stringToByteArray(pseudonym), r)
    
    val replyMessage = "This is a reply test"
    Params.pseudonymServer.sendToPseudonym(Params.stringToByteArray(pseudonym), Params.stringToByteArray(replyMessage))
    Params.pseudonymServer.sendToPseudonym(Params.stringToByteArray(pseudonym), Params.stringToByteArray(replyMessage))
    Params.pseudonymServer.sendToPseudonym(Params.stringToByteArray(pseudonym), Params.stringToByteArray(replyMessage))
    
    Assert.assertTrue(true)
  }
  

  @Test(expected = classOf[NoReplyBlocksException])
  def usingInvalidSurb {
    cleanup
    val pseudonym = "nd359"
    val nym = Params.stringToByteArray(pseudonym)

    val replyMessage = "This is a reply test"
    Params.pseudonymServer.sendToPseudonym(Params.stringToByteArray(pseudonym), Params.stringToByteArray(replyMessage))
  }

  @Test(expected = classOf[NoReplyBlocksException])
  def usingSameSurbTwice {
    cleanup
    val pseudonym = "nd359"
    val nym = Params.stringToByteArray(pseudonym)
    client.createPseudonymReply(Params.stringToByteArray(pseudonym), r)

    val replyMessage = "This is a reply test"
    Params.pseudonymServer.sendToPseudonym(Params.stringToByteArray(pseudonym), Params.stringToByteArray(replyMessage))
    Params.pseudonymServer.sendToPseudonym(Params.stringToByteArray(pseudonym), Params.stringToByteArray(replyMessage))
  }
  
  @Test(expected = classOf[NoReplyBlocksException])
  def sendingToPseudonymTooManyTimes {
    cleanup
    val pseudonym = "nd359"
    val nym = Params.stringToByteArray(pseudonym)
    client.createPseudonymReply(Params.stringToByteArray(pseudonym), r)
    client.createPseudonymReply(Params.stringToByteArray(pseudonym), r)
    client.createPseudonymReply(Params.stringToByteArray(pseudonym), r)
    
    val replyMessage = "This is a reply test"
    Params.pseudonymServer.sendToPseudonym(Params.stringToByteArray(pseudonym), Params.stringToByteArray(replyMessage))
    Params.pseudonymServer.sendToPseudonym(Params.stringToByteArray(pseudonym), Params.stringToByteArray(replyMessage))
    Params.pseudonymServer.sendToPseudonym(Params.stringToByteArray(pseudonym), Params.stringToByteArray(replyMessage))
    Params.pseudonymServer.sendToPseudonym(Params.stringToByteArray(pseudonym), Params.stringToByteArray(replyMessage))
  }
}