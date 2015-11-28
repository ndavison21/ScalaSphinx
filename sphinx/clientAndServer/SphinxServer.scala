

package sphinx.clientAndServer

import sphinx.params.Params
import scala.util.Random
import sphinx.params.Params
import scala.collection.mutable.HashMap

class SphinxServer(p: Params) {
  def params = p
  private val x = params.group.genSecret
  val y = params.group.expon(params.group.g, x)
  val idNum = new Array[Byte](4)
  Random.nextBytes(idNum)
  val id = nenc(idNum)
  val name = "Node " + Params.byteArrayToStringOfHex(idNum)
  val seen = new HashMap[String, Boolean]
  Params.pki.put(Params.byteArrayToStringOfHex(idNum), this)
  
  
  
  private def nenc(idNum: Array[Byte]): Array[Byte] = {
    val id: Array[Byte] = new Array[Byte](Params.k)
    id(0) = -1 // two's complement 11111111
    
    for (i <- 0 until idNum.length)
      id(i+1) = idNum(i)
      
    for (i <- (idNum.length + 1) until Params.k)
      id(i) = 0
      
    return id
  }
  
  /**
   * Decodes the prefix-free encoding
   * Returns the type, value and the remainder of the input string
   */
  private def prefixDecode(s: Array[Byte]): (String, Array[Byte], Array[Byte]) = {
    if (s.length == 0) (null, null, null)
    if (s(0) == 0) ("dSpec", null, s.slice(1, s.length))
    if (s(0) == -1) ("node", s.slice(0, Params.k), s.slice(Params.k, s.length))
    val l = s(0)
    ("dest", s.slice(1, l+1), s.slice(l+1, s.length)) // TODO Check this does what it should
  }
  
  def process(header: (BigInt, Array[Byte], Array[Byte]), delta: Array[Byte]) {
    println("Processing at server: " + name)
    val (alpha, beta, gamma) = header
    
    // Check that alpha is in the group
    if (!params.group.inGroup(alpha)) return
    
    // Computer the shared secret
    val s = params.group.expon(alpha, x)
    
    // Have we seen it already?
    val tag = Params.byteArrayToStringOfHex(Params.tauHash(s, params))
    if (!seen.get(tag).isEmpty && seen.get(tag).get) return
        
    // Verifying MAC
    if (gamma.deep != Params.mu(Params.muKey(s, params), beta, params).deep) {
      println("MAC Mismatch")
      println("alpha = " + params.group.printable(alpha))
      println("s     = " + params.group.printable(s))
      println("beta  = " + Params.byteArrayToStringOfHex(beta))
      println("gamma  = " + Params.byteArrayToStringOfHex(gamma))
      return
    }
    
    seen.put(tag, true)
    
    val b = Params.xor(beta ++ Array.fill[Byte](2 * Params.k)(0), Params.rho(Params.rhoKey(s, params), params))
    val (msgType, value, rest) = prefixDecode(b)
    
    if (msgType == "node") {
      val nextHop = Params.pki.get(Params.byteArrayToStringOfHex(value)).get
      println("Next hop is: " + nextHop.name)
      val b2 = Params.hb(alpha, s, params)
      val alpha2 = params.group.expon(alpha, b2)
      val gamma2 = b.slice(Params.k, Params.k*2)
      val beta2 = b.slice(Params.k*2, b.length)
      val delta2 = Params.pii(Params.piKey(s, params), delta)
      return nextHop.process((alpha2, beta2, gamma2), delta2)
    }
    
    if (msgType == "dSpec") {
      val delta2 = Params.pii(Params.piKey(s, params), delta)
      if (delta2.slice(0, Params.k).deep == Array.fill[Byte](Params.k)(0).deep) {
        val(msgType2, value2, rest2) = prefixDecode(delta2.slice(Params.k, delta2.length))
        if (msgType2 == "dest") {
          val body = Params.unpadMsgBody(rest2)
          println("Deliver " + Params.byteArrayToStringOfHex(body) + " to " + Params.byteArrayToString(value2))
          // TODO: actually deliver messages
          return
        }
      }
    }
    
    if (msgType == "dest") {
      val id = rest.slice(Params.k, rest.length)
      val delta2 = Params.pii(Params.piKey(s, params), delta)
      println("Deliver reply message to " + Params.byteArrayToStringOfHex(value))
      if (Params.clients.contains(Params.byteArrayToStringOfHex(value))) {
        val client = Params.clients.get(Params.byteArrayToStringOfHex(value)).get
        return client.process(id, delta2)
      } else {
        println("No such client: " + Params.byteArrayToStringOfHex(value))
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