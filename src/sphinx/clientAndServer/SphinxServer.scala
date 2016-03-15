package sphinx.clientAndServer

import scala.collection.mutable.HashSet
import scala.util.Random

import sphinx.exceptions.NoSuchClientException
import sphinx.params.Params

/**
 * Note: any protected methods should be private, but were made protected for use in the testing version
 */
class SphinxServer(p: Params) {

  val params = p

  // Generating identifier
  val genId = new Array[Byte](5)
  Random.nextBytes(genId)
  val id = nodeNameEncode(genId)
  assert(id.length == Params.k)
  assert(id(0) == -1)
  val name = "node " + Params.byteArrayToStringOfHex(id)

  protected val x = params.group.genSecret // private key (should be private, but made protected for testing)
  val y = params.group.expon(params.group.g, x) // public key

  val seen = new HashSet[String]
  Params.pki.put(Params.byteArrayToStringOfHex(id), this)

  /**
   * used to enforce the prefix property, node identifiers start with a -1
   */
  protected def nodeNameEncode(id: Array[Byte]): Array[Byte] = Array[Byte]((-1).asInstanceOf[Byte]) ++ id ++ Array.fill(Params.k - (id.length + 1))(0.asInstanceOf[Byte])

  /**
   * Decodes the prefix-free encoding
   * Returns the type, value and the remainder of the input string
   */
  protected def prefixDecode(s: Array[Byte]): (String, Array[Byte], Array[Byte]) = {
    if (s.length == 0) (null, null, null)
    else if (s(0) == 0) ("dSpec", null, s.slice(1, s.length))
    else if (s(0) == -1) ("node", s.slice(0, Params.k), s.slice(Params.k, s.length))
    else {
      val l = s(0)
      ("dest", s.slice(1, l + 1), s.slice(l + 1, s.length))
    }
  }

  def process(header: (Array[Byte], Array[Byte], Array[Byte]), delta: Array[Byte]) {
    // println("Processing at server: " + name)

    val (alpha, beta, gamma) = header

    // Check that alpha is in the group
    assert((params.group.inGroup(alpha)))
    assert(beta.length == (2 * p.r + 1) * Params.k)
    assert(gamma.length == Params.k)

    // Compute the shared secret
    val s = params.group.expon(alpha, x)

    // Have we seen it already?
    val tag = Params.byteArrayToStringOfHex(Params.tauHash(s, params))
    if (seen.contains(tag)) {
      println(name + ": message tag already seen, returning")
      return
    }

    // Verifying MAC
    val mac = Params.mu(Params.muKey(s, params), beta, params)
    if (gamma.deep != mac.deep) {
      println("MAC Mismatch at: " + name)
    }

    seen += tag

    val beta1 = beta ++ Array.fill[Byte](2 * Params.k)(0.asInstanceOf[Byte])
    val beta2 = Params.rho(Params.rhoKey(s, params), params)

    val b = Params.xor(beta1, beta2)

//    println("beta1: " + Params.byteArrayToStringOfHex(beta1))
//    println("beta2: " + Params.byteArrayToStringOfHex(beta2))
//    println("b    : " + Params.byteArrayToStringOfHex(b))

    val (msgType, value, rest) = prefixDecode(b)

    if (msgType == "node") {
      println("msgType == node")
      val nextHop = Params.pki.get(Params.byteArrayToStringOfHex(value)).get
      //println("Next hop is: " + nextHop.name)
      val b2 = Params.hb(alpha, s, params)
      val alpha2 = params.group.expon(alpha, b2)
      val gamma2 = b.slice(Params.k, Params.k * 2)
      // val beta2 = b.slice(Params.k * 2, (2 * p.r + 3) * Params.k -1)
      val beta2 = b.slice(Params.k * 2, b.length)
      val delta2 = Params.pii(Params.piKey(s, params), delta)
      return nextHop.process((alpha2, beta2, gamma2), delta2)
    }

    if (msgType == "dSpec") {
      println("msgType == dspec")
      val delta2 = Params.pii(Params.piKey(s, params), delta)
      if (delta2.slice(0, Params.k).deep == Array.fill[Byte](Params.k)(0).deep) {
        val (msgType2, destination, message) = prefixDecode(delta2.slice(Params.k, delta2.length))
        if (msgType2 == "dest") {
          val body = Params.unpadMsgBody(message)
          println("Messsage: " + Params.byteArrayToString(body))
          println("Destination: " + Params.byteArrayToString(destination))
          return
        }
      }
    }

    if (msgType == "dest") {
      println("msgType == dest")
      val id = rest.slice(0, Params.k)
      val delta2 = Params.pii(Params.piKey(s, params), delta)
      println("Deliver reply message to " + Params.byteArrayToStringOfHex(value))
      if (Params.clients.contains(Params.byteArrayToStringOfHex(value))) {
        val client = Params.clients.get(Params.byteArrayToStringOfHex(value)).get
        return client.process(id, delta2)
      } else {
        throw new NoSuchClientException(Params.byteArrayToStringOfHex(value))
      }

    }
  }

  def main(args: Array[String]) {
    val p = new Params
    val server = new SphinxServer(p)

    println("Name: " + server.name)
    println("y: " + p.group.printable(server.y))
  }
}