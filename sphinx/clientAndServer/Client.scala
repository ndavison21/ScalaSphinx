package sphinx.clientAndServer

import scala.util.Random
import sphinx.params.Params
import scala.collection.mutable.HashMap
import sphinx.params.Methods

class Client {
  // Initialising
  val clientIDbytes = new Array[Byte](4)
  Random.nextBytes(clientIDbytes)
  val clientID = Methods.byteArrayToStringOfHex(clientIDbytes)
  val params = new Params
  params.clients.put(clientID, this)
  
  val keyTable = new HashMap[String, Array[Array[Byte]]]
  
  /**
  * Create a SURB for the given pseudonym (passing through n nodes), and send it to the nymserver.
  */
  def createPseudonymReply(pseudonym: Array[Byte], n: Int) {
    val nodelist = new Array[Array[Byte]](n)
    var i = 0;
    Methods.randomSubset(params.pki.keySet.toArray, n).foreach { x => nodelist(i) = Methods.stringToByteArray(x); i+=1 }
    val (id, keytuple, nymtuple) = ClientMethods.createSurb(clientID, nodelist, params)
    
    keyTable.put(Methods.byteArrayToStringOfHex(id), keytuple)
    params.pseudonymServer.addSurb(pseudonym, nymtuple)
  }
  
  /**
   * Process a (still encrypted) reply message
   */
  def process(id: Array[Byte], delta: Array[Byte]) {
    if (!keyTable.contains(Methods.byteArrayToStringOfHex(id))) {
      println("Unreadable reply message received by " + this.clientID)
      return
    }
    val keyTuple = keyTable.remove(Methods.byteArrayToStringOfHex(id)).get
    val ktilde = keyTuple(0)
    val nu = keyTuple.length - 1
    
    var delta = new Array[Byte](0)
    for (i <- nu + 1 to 1) delta = Methods.pi(keyTuple(i), delta)
    delta = Methods.pii(ktilde, delta)
    
    val zeroKey = Array.fill[Byte](params.k)(0)
    if (zeroKey.deep == delta.slice(0, params.k).deep) {
      val msg = Methods.unpadMsgBody(delta.slice(params.k, delta.length))
      println("Message Received by: " + clientID)
      println("Message: " + Methods.byteArrayToString(msg))
    }
    
  }
  
  
  def main(args: Array[String]) {
    // TODO
  }
  
}
