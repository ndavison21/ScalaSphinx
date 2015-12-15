package sphinx.clientAndServer

import scala.collection.mutable.HashMap
import scala.util.Random
import sphinx.params.Params
import sphinx.params.Params
import scala.collection.mutable.ListBuffer

object Client {
  /**
   * Procedure to make a Sphinx mix mesage header,
   * used as a subroutine to make forward messages and single-use reply blocks
   */
  def createMixHeader(destination: Array[Byte], identifier: Array[Byte], nodeIDs: Array[Array[Byte]], p: Params): ((BigInt, Array[Byte], Array[Byte]), Array[BigInt]) = {
    println
    assert(destination.length <= (2 * (p.r - nodeIDs.length) + 2) * Params.k)
    assert(nodeIDs.length <= p.r)
    assert(identifier.length == Params.k)

    val nu = nodeIDs.length

    val x = p.group.genSecret // TODO: Looks at the maths for this
    println("x: " + x)
    // Compute the nu (alpha, b, s) tuples
    /**
     * a(0) = g^x, s(0)=y(n(0))^x, b(0)=hb(a(0),s(0))
     * a(1) = g^(x*b(0)), s(1)=y(n(1))^(x*b(0)), b(1)=hb(a(1),s(1))
     * ...
     * a(nu-1) = ...
     * 
     * a(i): Group elements
     * s(i): the Diffie-Hellman shared secrets
     * b(i): the blinding factors
     */
    def computeASB(i: Integer, blinds: List[BigInt]):(BigInt,BigInt,BigInt) = {
      val alpha  = p.group.multiExpon(p.group.g, blinds)
      println("node " + i + ": " + Params.byteArrayToStringOfHex(nodeIDs(i)))
      println("alpha " + i + ": " + alpha)
      val node   = Params.pki.get(Params.byteArrayToStringOfHex(nodeIDs(i))).get
      val secret = p.group.multiExpon(node.y, blinds)
      println("secret " + i + ": " + secret)
      val blind  = Params.hb(alpha, secret, p)
      (alpha, secret, blind)
    }
    
    var blinds = x::Nil
    val asbTuples = new Array[(BigInt, BigInt, BigInt)](nu)
    
    for (i <- 0 until nu) { // TODO: do this in a properly functional way (recursively)
      val (a, s, b) = computeASB(i, blinds)
      blinds = b :: blinds
      asbTuples(i) = (a, s, b)
    }

    // Compute the nu filler strings (phi)
    // call using phiNu(0, new Array[Byte](0))
    def compPhi(i: Int, prevPhi: Array[Byte]): Array[Byte] = {
      assert(prevPhi.length == 2*i*Params.k)
      if (i == nu) return prevPhi
      val phi1 = prevPhi ++ Array.fill[Byte](2 * Params.k)(0.asInstanceOf[Byte])
      val phi2 = Params.rho(Params.rhoKey(asbTuples(i)._2, p), p)
      compPhi(i+1, Params.xor(phi1, phi2.slice(phi2.length - (2 * (i+1) * Params.k), phi2.length)))
    }

    // Compute the M = (alpha, beta, gamma) message headers
    def compHeader(i: Integer, prevBeta: Array[Byte], prevGamma: Array[Byte]): (BigInt, Array[Byte], Array[Byte]) = {
      if (i<0) return (asbTuples(0)._1, prevBeta, prevGamma)
      val beta1 = nodeIDs(i+1) ++ prevGamma ++ prevBeta.slice(0, (2*p.r - 1) * Params.k)
      assert(beta1.length == (2 * p.r +1) * Params.k)
      val beta2 = Params.rho(Params.rhoKey(asbTuples(i)._2, p), p).slice(0, (2 * p.r +1) * Params.k)
//      println("beta l: " + Params.byteArrayToStringOfHex(beta1))
//      println("beta r: " + Params.byteArrayToStringOfHex(beta2))
      val beta = Params.xor(beta1, beta2)
      val gamma = Params.mu(Params.muKey(asbTuples(i)._2, p), beta, p)
      println("beta " + i + ": " + Params.byteArrayToStringOfHex(beta))
      //println("s " + i + ": " + asbTuples(i)._2)
      //println("gamma " + i + ": " + Params.byteArrayToStringOfHex(gamma))
      compHeader(i-1, beta, gamma)
    }
    
    val phi = compPhi(0, new Array[Byte](0))    
    val betaNu1 = destination ++ identifier ++ Array.fill[Byte]((2 * (p.r - nu) + 2) * Params.k - destination.length)(0.asInstanceOf[Byte])
    val betaNu2 = Params.rho(Params.rhoKey(asbTuples(nu-1)._2, p), p).slice(0, (2 * p.r + 1) * Params.k)
    val betaNu = Params.xor(betaNu1, betaNu2) ++ phi
    val gammaNu = Params.mu(Params.muKey(asbTuples(nu-1)._2, p), betaNu, p)
    val m = compHeader(nu-2, betaNu, gammaNu)
    
    var sSequence = new Array[BigInt](nu)
    var i = 0
    asbTuples.foreach((t) => { sSequence(i) = t._2; i += 1 })

    (m, sSequence)
  }
  
  /**
   * Procedure to create a forward message to be sent through the Sphinx network
   * The forward message is the output of this procedure, and should be sent to node 0
   */
  def creatForwardMessage(message: Array[Byte], destination: Array[Byte], nodeIDs: Array[Array[Byte]], p: Params): ((BigInt, Array[Byte], Array[Byte]), Array[Byte]) = {
    assert(nodeIDs.length <= p.r)
    assert (Params.k + 1 + destination.length + message.length < Params.m)
    
    val nu = nodeIDs.length
    
    val id = Array.fill(Params.k)(0.asInstanceOf[Byte])
    
    // Compute the header and secrets
    val (header, secrets) = createMixHeader(Params.dSpecial, id, nodeIDs, p)

    def compDelta(i: Integer, prevDelta: Array[Byte]): Array[Byte] = {
      if (i<0) return prevDelta
      compDelta(i-1, Params.pi(Params.piKey(secrets(i), p), prevDelta))
    }
    
    val deltaNu = Params.pi(Params.piKey(secrets(nu-1), p), Array.fill(Params.k)(0.asInstanceOf[Byte]) ++ destination ++ message)
    val delta = compDelta(nu-2, deltaNu)
    
    (header, delta)
  }
  
  /**
   * Create a single-use reply block
   */
  def createSurb(destination: Array[Byte], nodeIDs: Array[Array[Byte]], p: Params): (Array[Byte], Array[Array[Byte]], 
                                                            (Array[Byte], (BigInt, Array[Byte], Array[Byte]), Array[Byte])) = {
    val nu = nodeIDs.length
    val id = new Array[Byte](Params.k)
    Random.nextBytes(id)
    
    // Compute the header and secrets
    val (header, secrets) = createMixHeader(destination, id, nodeIDs, p)
    
    val ktilde = new Array[Byte](Params.k)
    Random.nextBytes(ktilde)
    
    val keyTuple = new Array[Array[Byte]](nu + 1)
    keyTuple(0) = ktilde
    for ( i <- 1 to nu) keyTuple(i) = Params.piKey(secrets(i-1), p)
    
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
    val (id, keytuple, nymtuple) = Client.createSurb(clientIDbytes, nodelist, params)
    
    keyTable.put(Params.byteArrayToStringOfHex(id), keytuple)
    Params.pseudonymServer.addSurb(pseudonym, nymtuple)
  }
  
  /**
   * Reply message processing by pseudonym owners
   */
  def process(id: Array[Byte], delta: Array[Byte]) {
    if (!keyTable.contains(Params.byteArrayToStringOfHex(id))) {
      println("Unreadable reply message received by " + this.clientID)
      return
    }
    val keyTuple = keyTable.remove(Params.byteArrayToStringOfHex(id)).get
    val ktilde = keyTuple(0)
    val nu = keyTuple.length - 1
    
    def appPi(i: Integer, prevDelta: Array[Byte]): Array[Byte] = {
      if (i<0) delta
      appPi(i-1, Params.pi(keyTuple(i), prevDelta))
    }
    
    val deltaPrime = Params.pii(ktilde, appPi(nu, delta))
    
    val zeroKey = Array.fill[Byte](Params.k)(0)
    if (zeroKey.deep == delta.slice(0, Params.k).deep) {
      val msg = Params.unpadMsgBody(delta.slice(Params.k, delta.length))
      println("Message Received by: " + clientID)
      println("Message: " + Params.byteArrayToString(msg))
    } else {
      println("Unable to process message " + clientID)
    }
    
  }
  
}
