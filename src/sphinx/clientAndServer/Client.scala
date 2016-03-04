package sphinx.clientAndServer

import scala.collection.mutable.HashMap
import scala.util.Random
import sphinx.params.Params
import sphinx.exceptions.DestinationLengthException
import sphinx.exceptions.DataLengthException
import sphinx.exceptions.PathLengthException

object Client {

  /**
   * Procedure to make a Sphinx mix message header,
   * used as a subroutine to make forward messages and single-use reply blocks
   */
  def createMixHeader(destination: Array[Byte], identifier: Array[Byte], nodeIDs: Array[Array[Byte]], p: Params): ((Array[Byte], Array[Byte], Array[Byte]), Array[Array[Byte]]) = {
    if (destination == null || destination.length > (2 * (p.r - nodeIDs.length) + 2) * Params.k) throw new DestinationLengthException("Invalid Destination")
    if (nodeIDs == null || nodeIDs.length > p.r) throw new PathLengthException("Number of nodes length must be <= " + p.r)
    assert(identifier.length == Params.k)

    val nu = nodeIDs.length

    val x = p.group.genSecret

    /**
     * Compute the nu (alpha, s, b) tuples
     * alpha(i): Group elements
     * s(i): the Diffie-Hellman shared secrets
     * b(i): the blinding factors
     */
    def computeASB(i: Integer, blinds: List[Array[Byte]]): (Array[Byte], Array[Byte], Array[Byte]) = {
      val alpha = p.group.multiExpon(p.group.g, blinds)
      val node = Params.pki.get(Params.byteArrayToStringOfHex(nodeIDs(i))).get
      val secret = p.group.multiExpon(node.y, blinds)
      val blind = Params.hb(alpha, secret, p)
      (alpha, secret, blind)
    }

    var blinds = x :: Nil
    val asbTuples = new Array[(Array[Byte], Array[Byte], Array[Byte])](nu)

    for (i <- 0 until nu) { // TODO: do this in a properly functional way (recursively)
      val (a, s, b) = computeASB(i, blinds)
      blinds = b :: blinds
      asbTuples(i) = (a, s, b)
    }

    /**
     * Compute the nu filler strings (phi)
     * call using phiNu(0, new Array[Byte](0))
     */
    def compPhi(i: Int, prevPhi: Array[Byte]): Array[Byte] = {
      assert(prevPhi.length == 2 * i * Params.k)
      if (i == nu - 1) return prevPhi
      val phi1 = prevPhi ++ Array.fill[Byte](2 * Params.k)(0.asInstanceOf[Byte])
      val phi2 = Params.rho(Params.rhoKey(asbTuples(i)._2, p), p)
      val min = (2 * (p.r - (i + 1)) + 3) * Params.k
      val phi = Params.xor(phi1, phi2.slice(min, phi2.length))
      compPhi(i + 1, phi)
    }

    val phi = compPhi(0, new Array[Byte](0.asInstanceOf[Byte]))

    // Compute the M = (alpha, beta, gamma) message headers
    def compHeader(i: Integer, prevBeta: Array[Byte], prevGamma: Array[Byte]): (Array[Byte], Array[Byte], Array[Byte]) = {
      if (i < 0) return (asbTuples(0)._1, prevBeta, prevGamma)

      val beta1 = nodeIDs(i + 1) ++ prevGamma ++ prevBeta.slice(0, (2 * p.r - 1) * Params.k)
      val beta2 = Params.rho(Params.rhoKey(asbTuples(i)._2, p), p).slice(0, (2 * p.r + 1) * Params.k)

      val beta = Params.xor(beta1, beta2)
      //      println
      //      println("beta1: " + Params.byteArrayToStringOfHex(beta1))
      //      println("beta2: " + Params.byteArrayToStringOfHex(beta2))
      //      println("beta : " + Params.byteArrayToStringOfHex(beta))

      val gamma = Params.mu(Params.muKey(asbTuples(i)._2, p), beta, p)

      compHeader(i - 1, beta, gamma)
    }

    val betaNu1 = destination ++ identifier ++ Array.fill[Byte]((2 * (p.r - nu) + 2) * Params.k - destination.length)(0.asInstanceOf[Byte])
    val betaNu2 = Params.rho(Params.rhoKey(asbTuples(nu - 1)._2, p), p).slice(0, (2 * (p.r - nu) + 3) * Params.k)

    val betaNu = Params.xor(betaNu1, betaNu2) ++ phi
    val gammaNu = Params.mu(Params.muKey(asbTuples(nu - 1)._2, p), betaNu, p)

    val m = compHeader(nu - 2, betaNu, gammaNu)

    var sSequence = new Array[Array[Byte]](nu)
    var i = 0
    asbTuples.foreach((t) => { sSequence(i) = t._2; i += 1 })

    (m, sSequence)
  }

  /**
   * Procedure to create a forward message to be sent through the Sphinx network
   * The forward message is the output of this procedure, and should be sent to node 0
   */
  def creatForwardMessage(message: Array[Byte], destination: Array[Byte], nodeIDs: Array[Array[Byte]], p: Params): ((Array[Byte], Array[Byte], Array[Byte]), Array[Byte]) = {
    if (nodeIDs == null || nodeIDs.length > p.r) throw new PathLengthException("Number of nodes length must be <= " + p.r)
    if (Params.k + 1 + destination.length + message.length >= Params.m) throw new DataLengthException("Data too long, combined message, destination and key must by less than " + Params.m + " bytes")

    val nu = nodeIDs.length

    val id = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte])

    // Compute the header and secrets
    val (header, secrets) = createMixHeader(Params.dSpecial, id, nodeIDs, p)

    def compDelta(i: Integer, prevDelta: Array[Byte]): Array[Byte] = {
      if (i < 0) return prevDelta
      compDelta(i - 1, Params.pi(Params.piKey(secrets(i), p), prevDelta))
    }

    val body = Params.padMsgBody(Params.m, Array.fill[Byte](Params.k)(0.asInstanceOf[Byte]) ++ Params.destinationEncode(destination) ++ message)
    val deltaNu = Params.pi(Params.piKey(secrets(nu - 1), p), body)
    val delta = compDelta(nu - 2, deltaNu)

    (header, delta)
  }

  /**
   * Create a single-use reply block
   */
  def createSurb(destination: Array[Byte], nodeIDs: Array[Array[Byte]], p: Params): (Array[Byte], Array[Array[Byte]], (Array[Byte], (Array[Byte], Array[Byte], Array[Byte]), Array[Byte])) = {
    if (nodeIDs == null || nodeIDs.length > p.r) throw new PathLengthException("Number of nodes length must be <= " + p.r)

    val nu = nodeIDs.length
    val id = new Array[Byte](Params.k)
    Random.nextBytes(id)

    // Compute the header and secrets
    val (header, secrets) = createMixHeader(Params.destinationEncode(destination), id, nodeIDs, p)

    val ktilde = new Array[Byte](Params.k)
    Random.nextBytes(ktilde)

    val keyTuple = new Array[Array[Byte]](nu + 1)
    keyTuple(0) = ktilde
    for (i <- 1 to nu) keyTuple(i) = Params.piKey(secrets(i - 1), p)

    (id, keyTuple, (nodeIDs(0), header, ktilde))
  }

  def main(args: Array[String]) {
    println("Order of flags [-ecc] [path length] [forward message] [email] [reply message] [pseudonym]")
    val useEcc = (args.length > 0 && args(0) == "-ecc")
    val r = (if (args.length > 1) args(1).toInt else 5)
    val p = new Params(r, useEcc)

    // Create some sphinx servers (they add themselves to the pki)
    for (i <- 0 to r * 2) {
      new SphinxServer(new Params(r, useEcc))
    }

    // Create a client
    val client = new Client(new Params(r, useEcc))

    val useNodes = Params.randomSubset(Params.pki.keySet.toArray, r)
    println("Creating forward message")

    val message = (if (args.length > 2) args(2) else "This is a test")
    val email = (if (args.length > 3) args(3) else "nd359@cam.ac.uk")

    val (header, delta) = Client.creatForwardMessage(Params.stringToByteArray(message),
      Params.stringToByteArray(email), useNodes.map { x => Params.stringOfHexToByteArray(x) }, p)
    println("Finished Creating forward message")

    // Send it to the first node for processing
    println
    println("Processing message")
    Params.pki.get(useNodes(0)).get.process(header, delta)
    println("Finished Processing Message")
    println
    // Create a reply block for the client
    val pseudonym = (if (args.length > 4) args(4) else "nd359")
    println("Creating a reply block")
    client.createPseudonymReply(Params.stringToByteArray(pseudonym), r)
    println("Finished creating a reply block")
    println
    // Send a message to it
    println("Using the reply block")

    val replyMessage = (if (args.length > 4) args(4) else "This is a reply test")
    Params.pseudonymServer.sendToPseudonym(Params.stringToByteArray(pseudonym), Params.stringToByteArray(replyMessage))
    println("Finished using the reply block")
  }
}

class Client(p: Params) {
  // Initialising
  val clientId = new Array[Byte](4)
  Random.nextBytes(clientId)
  val name = "client " + Params.byteArrayToStringOfHex(clientId)
  val params = p
  Params.clients.put(Params.byteArrayToStringOfHex(clientId), this)

  val keyTable = new HashMap[String, Array[Array[Byte]]]

  /**
   * Create a SURB for the given pseudonym (passing through n nodes), and send it to the nymserver.
   */
  def createPseudonymReply(n: Int): String = {
    val nym = new Array[Byte](8)
    Random.nextBytes(nym)
    createPseudonymReply(nym, n)
    Params.byteArrayToStringOfHex(nym)
  }

  def createPseudonymReply(pseudonym: Array[Byte], n: Int) {
    val nodelist = new Array[Array[Byte]](n)
    var i = 0;
    Params.randomSubset(Params.pki.keySet.toArray, n).foreach { x => nodelist(i) = Params.stringOfHexToByteArray(x); i += 1 }
    val (headerId, keytuple, nymtuple) = Client.createSurb(clientId, nodelist, params)

    keyTable.put(Params.byteArrayToStringOfHex(headerId), keytuple)
    Params.pseudonymServer.addSurb(pseudonym, nymtuple) // This needs to be done over a secure channel
  }

  /**
   * Reply message processing by pseudonym owners
   */
  def process(id: Array[Byte], delta: Array[Byte]) {
    if (!keyTable.contains(Params.byteArrayToStringOfHex(id))) {
      println(name + " Unreadable reply message received")
      return
    }
    val keyTuple = keyTable.remove(Params.byteArrayToStringOfHex(id)).get
    val ktilde = keyTuple(0)
    val nu = keyTuple.length - 1

    def appPi(i: Integer, prevDelta: Array[Byte]): Array[Byte] = {
      if (i < 1) return prevDelta
      appPi(i - 1, Params.pi(keyTuple(i), prevDelta))
    }

    val deltaPrime = Params.pii(ktilde, appPi(nu, delta))

    val zeroKey = Array.fill[Byte](Params.k)(0)
    val msg = Params.unpadMsgBody(deltaPrime.slice(Params.k, deltaPrime.length))
    if (zeroKey.deep == deltaPrime.slice(0, Params.k).deep) {

      println(name + " Message Received")
      println("Message: " + Params.byteArrayToString(msg))
    } else {
      println(name + " Unable to process message ")
      println("Message: " + Params.byteArrayToString(msg))

    }

  }

}
