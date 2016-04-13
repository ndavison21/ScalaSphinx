package sphinx.test.dissertation

import sphinx.clientAndServer.SphinxServer
import sphinx.exceptions.IdLengthException
import sphinx.exceptions.NoSuchClientException
import sphinx.params.Params
import sphinx.exceptions.MacMismatchException

/**
 * A modified version of the Sphinx server for testing. 
 * 
 * Does not forward to the next stage, in order to test the processing overhead at a single node
 * Has a public nodeNameEncode method, in order to unit test.
 */
class SphinxServerModified(p: Params) extends SphinxServer(p) {
  
  override val x = params.group.genSecret // private key (should be private, but made protected for testing)
  
  override def nodeNameEncode(id: Array[Byte]): Array[Byte] = {
    val mod_id = if (id == null) Array.fill(0)(0.asInstanceOf[Byte]) else id
    if (mod_id.length >= Params.k - 1) throw new IdLengthException("Max length of id is " + Params.k)
    Array[Byte]((-1).asInstanceOf[Byte]) ++ mod_id ++ Array.fill(Params.k - (mod_id.length + 1))(0.asInstanceOf[Byte])
  }

  
  override def process(header: (Array[Byte], Array[Byte], Array[Byte]), delta: Array[Byte]) {
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
      throw new MacMismatchException("MAC Mismatch at: " + name)
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
      //println("msgType == node")
      val nextHop = Params.pki.get(Params.byteArrayToStringOfHex(value)).get
      //println("Next hop is: " + nextHop.name)
      val b2 = Params.hb(alpha, s, params)
      val alpha2 = params.group.expon(alpha, b2)
      val gamma2 = b.slice(Params.k, Params.k * 2)
      // val beta2 = b.slice(Params.k * 2, (2 * p.r + 3) * Params.k -1)
      val beta2 = b.slice(Params.k * 2, b.length)
      val delta2 = Params.pii(Params.piKey(s, params), delta)
      // return nextHop.process((alpha2, beta2, gamma2), delta2)
      return
    }

    if (msgType == "dSpec") {
      //println("msgType == dspec")
      val delta2 = Params.pii(Params.piKey(s, params), delta)
      if (delta2.slice(0, Params.k).deep == Array.fill[Byte](Params.k)(0).deep) {
        val (msgType2, destination, message) = prefixDecode(delta2.slice(Params.k, delta2.length))
        if (msgType2 == "dest") {
          val body = Params.unpadMsgBody(message)
          //println("Messsage: " + Params.byteArrayToString(body))
          //println("Destination: " + Params.byteArrayToString(destination))
          return
        }
      }
    }

    if (msgType == "dest") {
      //println("msgType == dest")
      val id = rest.slice(0, Params.k)
      val delta2 = Params.pii(Params.piKey(s, params), delta)
      //println("Deliver reply message to " + Params.byteArrayToStringOfHex(value))
      if (Params.clients.contains(Params.byteArrayToStringOfHex(value))) {
        val client = Params.clients.get(Params.byteArrayToStringOfHex(value)).get
        // return client.process(id, delta2)
        return
      } else {
        throw new NoSuchClientException(Params.byteArrayToStringOfHex(value))
      }

    }
  }
}