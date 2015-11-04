package sphinx.params

import scala.collection.mutable.HashMap
import sphinx.clientAndServer.Client
import sphinx.clientAndServer.SphinxServer

/**
 * r: minimum number of hops before reaching destination, default is 5 (TOR uses 3)
 * ecc: elliptic curve cryptography
 */
class Params(r: Int, ecc: Boolean) {
  def this() = this( 5, false) // constructor with default values
  
  val group = {
    if (ecc) new Group_ECC()
    else new Group_P()
  }
  
  val k = 16 // security parameter, in bytes (16 bytes = 128 bits)
  val m = 1024 // size of the message body, in bytes. This needs to be kept constant in order to prevent attackers being able to correlate message length with number of hops
  val pki = new HashMap[String, SphinxServer]() // public key infrastructure, mapping of server ID to server, we generate the String using the Byte.toBinaryString method
  val clients = new HashMap[String, Client]()
}