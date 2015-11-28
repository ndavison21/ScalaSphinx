package sphinx.clientAndServer

import scala.collection.mutable.ListBuffer
import sphinx.params.Params
import sphinx.params.Methods
import scala.util.Random

object ClientMethods {
  
  
  def createMixHeader(destination: Array[Byte], identifier: Array[Byte], nodeIDs: Array[Array[Byte]], params: Params): ((BigInt, Array[Byte], Array[Byte]), Array[BigInt]) = {
    assert(nodeIDs.length <= params.r)
    assert(identifier.length == params.k)

    val nu = nodeIDs.length

    val x = params.group.genSecret

    val blinds = new ListBuffer[BigInt]()
    val asbTuples = new Array[(BigInt, BigInt, BigInt)](nu)

    // Compute the nu (alpha, b, s) tuples
    for (i <- 0 until nu) {
      val alpha = params.group.multiExpon(params.group.g, blinds.toList) // group elements
      val s = params.group.multiExpon(params.pki.get(Methods.byteArrayToStringOfHex(nodeIDs(i))).get.y, blinds.toList) // D-H Shared secrets
      val b = Methods.hb(alpha, s, params) // blinding factors
      blinds.append(b)
      asbTuples(i) = (alpha, s, b)
    }

    // Compute the nu filler strings (phi)
    var prevPhi = new Array[Byte](0);
    var phi = new Array[Byte](prevPhi.length + (2 * params.k))
    for (i <- 1 until nu) {
      phi = new Array[Byte](prevPhi.length + (2 * params.k))

      for (j <- 0 until prevPhi.length) phi(j) = prevPhi(j)
      for (j <- prevPhi.length until phi.length) phi(j) = 0

      phi = Methods.xor(phi, Methods.rho(Methods.rhoKey(asbTuples(i - 1)._2, params), params)) // TODO: Check this only uses first few bits of second argument
      prevPhi = phi
    }

    // Compute the (beta, gamma) tuples
    val rand = new Array[Byte](((2 * (params.r - nu) + 2) * params.k - destination.length))
    Random.nextBytes(rand);
    var beta = destination ++ identifier ++ rand
    beta = Methods.xor(beta, Methods.rho(Methods.rhoKey(asbTuples(nu - 1)._2, params), params))
    var gamma = Methods.mu(Methods.muKey(asbTuples(nu - 1)._2, params), beta, params)

    for (i <- nu - 2 to 0) {
      var id = nodeIDs(i + 1)
      assert(id.length == params.k)
      beta = Methods.xor(id ++ gamma ++ beta.slice(0, 2 * (params.r - 1)), Methods.rho(Methods.rhoKey(asbTuples(i)._2, params), params))
      gamma = Methods.mu(Methods.muKey(asbTuples(nu - 1)._2, params), beta, params)
    }

    var sSequence = new Array[BigInt](nu)
    var i = 0
    asbTuples.foreach((t) => { sSequence(i) = t._2; i += 1 })

    ((asbTuples(0)._1, beta, gamma), sSequence)
  }
  
  def creatForwardMessage(message: Array[Byte], destination: String, nodeIDs: Array[Array[Byte]], params: Params): ((BigInt, Array[Byte], Array[Byte]), Array[Byte]) = {
    assert (params.k + 1 + destination.length + message.length < params.m)
    
    val nu = nodeIDs.length
    
    val id = new Array[Byte](params.k)
    for (i <- 0 until id.length) id(i) = 0
    // Compute the header and secrets
    val (header, secrets) = createMixHeader(params.dSpecial, id, nodeIDs, params)
    
    val dest = Methods.dEnc(destination) // TODO Verify dEnc is doing what it should (try denc2 different implementations)
    
    val zeroes = new Array[Byte](params.k)
    for (i <- 0 until zeroes.length) zeroes(i) = 0
    val body = Methods.padMsgBody(params.m, zeroes ++ dest ++ message)
    
    var delta = Methods.pi( Methods.piKey(secrets(nu-1), params) , body)
    for (i <- nu-2 to 0) delta = Methods.pi(Methods.piKey(secrets(i), params), delta)
    
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
    val id = new Array[Byte](params.k)
    Random.nextBytes(id)
    
    // Compute the header and secrets
    val (header, secrets) = createMixHeader(Methods.dEnc(destination), id, nodeIDs, params)
    
    val ktilde = new Array[Byte](params.k)
    Random.nextBytes(ktilde)
    val keyTuple = new Array[Array[Byte]](nu + 1)
    keyTuple(0) = ktilde
    for ( i <- 1 to nu) keyTuple(i) = Methods.piKey(secrets(i-1), params) // actually want i to be a tuple :(
    
    (id, keyTuple, (nodeIDs(0), header, ktilde))
  }
}