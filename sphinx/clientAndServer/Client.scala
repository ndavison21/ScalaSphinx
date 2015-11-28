package sphinx.clientAndServer

import scala.collection.mutable.HashMap
import scala.util.Random
import sphinx.params.Params
import sphinx.params.Params
import scala.collection.mutable.ListBuffer

object Client {
  
  def createMixHeader(destination: Array[Byte], identifier: Array[Byte], nodeIDs: Array[Array[Byte]], params: Params): ((BigInt, Array[Byte], Array[Byte]), Array[BigInt]) = {
    assert(nodeIDs.length <= params.r)
    assert(identifier.length == Params.k)

    val nu = nodeIDs.length

    val x = params.group.genSecret

    val blinds = new ListBuffer[BigInt]()
    val asbTuples = new Array[(BigInt, BigInt, BigInt)](nu)

    // Compute the nu (alpha, b, s) tuples
    for (i <- 0 until nu) {
      val alpha = params.group.multiExpon(params.group.g, blinds.toList) // group elements
      val s = params.group.multiExpon(
                            Params.pki.get(Params.byteArrayToStringOfHex(nodeIDs(i))).get.y, blinds.toList) // D-H Shared secrets
      val b = Params.hb(alpha, s, params) // blinding factors
      blinds.append(b)
      asbTuples(i) = (alpha, s, b)
    }

    // Compute the nu filler strings (phi)
    var prevPhi = new Array[Byte](0);
    var phi = new Array[Byte](prevPhi.length + (2 * Params.k))
    for (i <- 1 until nu) {
      phi = new Array[Byte](prevPhi.length + (2 * Params.k))

      for (j <- 0 until prevPhi.length) phi(j) = prevPhi(j)
      for (j <- prevPhi.length until phi.length) phi(j) = 0

      phi = Params.xor(phi, Params.rho(Params.rhoKey(asbTuples(i - 1)._2, params), params)) // TODO: Check this only uses first few bits of second argument
      prevPhi = phi
    }

    // Compute the (beta, gamma) tuples
    val rand = new Array[Byte](((2 * (params.r - nu) + 2) * Params.k - destination.length))
    Random.nextBytes(rand);
    var beta = destination ++ identifier ++ rand
    beta = Params.xor(beta, Params.rho(Params.rhoKey(asbTuples(nu - 1)._2, params), params))
    var gamma = Params.mu(Params.muKey(asbTuples(nu - 1)._2, params), beta, params)

    for (i <- nu - 2 to 0) {
      var id = nodeIDs(i + 1)
      assert(id.length == Params.k)
      beta = Params.xor(id ++ gamma ++ beta.slice(0, 2 * (params.r - 1)), Params.rho(Params.rhoKey(asbTuples(i)._2, params), params))
      gamma = Params.mu(Params.muKey(asbTuples(nu - 1)._2, params), beta, params)
    }

    var sSequence = new Array[BigInt](nu)
    var i = 0
    asbTuples.foreach((t) => { sSequence(i) = t._2; i += 1 })

    ((asbTuples(0)._1, beta, gamma), sSequence)
  }
  
  def creatForwardMessage(message: Array[Byte], destination: String, nodeIDs: Array[Array[Byte]], params: Params): ((BigInt, Array[Byte], Array[Byte]), Array[Byte]) = {
    assert (Params.k + 1 + destination.length + message.length < Params.m)
    
    val nu = nodeIDs.length
    
    val id = new Array[Byte](Params.k)
    for (i <- 0 until id.length) id(i) = 0
    // Compute the header and secrets
    val (header, secrets) = createMixHeader(Params.dSpecial, id, nodeIDs, params)
    
    val dest = Params.dEnc(destination) // TODO Verify dEnc is doing what it should (try denc2 different implementations)
    
    val zeroes = new Array[Byte](Params.k)
    for (i <- 0 until zeroes.length) zeroes(i) = 0
    val body = Params.padMsgBody(Params.m, zeroes ++ dest ++ message)
    
    var delta = Params.pi( Params.piKey(secrets(nu-1), params) , body)
    for (i <- nu-2 to 0) delta = Params.pi(Params.piKey(secrets(i), params), delta)
    
    (header, delta)
  }
  
  /**
   * Create a single-use reply block
   * 
   * Output: I, the tuple (˜k, hpi(s0),...,hpi(s(nu−1))), and the tuple (n0, M0, ˜k)
   */
  def createSurb(destination: String, nodeIDs: Array[Array[Byte]], params: Params): (Array[Byte], Array[Array[Byte]], 
                                                            (Array[Byte], (BigInt, Array[Byte], Array[Byte]), Array[Byte])) = {
    val nu = nodeIDs.length
    val id = new Array[Byte](Params.k)
    Random.nextBytes(id)
    
    // Compute the header and secrets
    val (header, secrets) = createMixHeader(Params.dEnc(destination), id, nodeIDs, params)
    
    val ktilde = new Array[Byte](Params.k)
    Random.nextBytes(ktilde)
    val keyTuple = new Array[Array[Byte]](nu + 1)
    keyTuple(0) = ktilde
    for ( i <- 1 to nu) keyTuple(i) = Params.piKey(secrets(i-1), params) // actually want i to be a tuple :(
    
    (id, keyTuple, (nodeIDs(0), header, ktilde))
  }
}

class Client(p: Params) {
  // Initialising
  val clientIDbytes = new Array[Byte](4)
  Random.nextBytes(clientIDbytes)
  val clientID = Params.byteArrayToStringOfHex(clientIDbytes)
  val params = p
  Params.clients.put(clientID, this)
  
  val keyTable = new HashMap[String, Array[Array[Byte]]]
  
  /**
  * Create a SURB for the given pseudonym (passing through n nodes), and send it to the nymserver.
  */
  def createPseudonymReply(pseudonym: Array[Byte], n: Int) {
    val nodelist = new Array[Array[Byte]](n)
    var i = 0;
    Params.randomSubset(Params.pki.keySet.toArray, n).foreach { x => nodelist(i) = Params.stringToByteArray(x); i+=1 }
    val (id, keytuple, nymtuple) = Client.createSurb(clientID, nodelist, params)
    
    keyTable.put(Params.byteArrayToStringOfHex(id), keytuple)
    Params.pseudonymServer.addSurb(pseudonym, nymtuple)
  }
  
  /**
   * Process a (still encrypted) reply message
   */
  def process(id: Array[Byte], delta: Array[Byte]) {
    if (!keyTable.contains(Params.byteArrayToStringOfHex(id))) {
      println("Unreadable reply message received by " + this.clientID)
      return
    }
    val keyTuple = keyTable.remove(Params.byteArrayToStringOfHex(id)).get
    val ktilde = keyTuple(0)
    val nu = keyTuple.length - 1
    
    var delta = new Array[Byte](0)
    for (i <- nu + 1 to 1) delta = Params.pi(keyTuple(i), delta)
    delta = Params.pii(ktilde, delta)
    
    val zeroKey = Array.fill[Byte](Params.k)(0)
    if (zeroKey.deep == delta.slice(0, Params.k).deep) {
      val msg = Params.unpadMsgBody(delta.slice(Params.k, delta.length))
      println("Message Received by: " + clientID)
      println("Message: " + Params.byteArrayToString(msg))
    }
    
  }
  
}
