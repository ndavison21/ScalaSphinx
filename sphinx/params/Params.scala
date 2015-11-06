package sphinx.params

import scala.collection.mutable.HashMap
import sphinx.clientAndServer.Client
import sphinx.clientAndServer.SphinxServer
import sphinx.clientAndServer.PseudonymServer
import java.security.MessageDigest

/**
 * r: minimum number of hops before reaching destination, default is 5 (TOR uses 3)
 * ecc: elliptic curve cryptography
 */
class Params(maxhops: Int, useEcc: Boolean) {
  def this() = this( 5, false) // constructor with default values
  
  val r = maxhops
  val group: Group = {
    if (useEcc) new Group_ECC() // Group_ECC is not implemented
    else new Group_P()
  }
  
  val k = 16 // security parameter, in bytes (16 bytes = 128 bits)
  val m = 1024 // size of the message body, in bytes. This needs to be kept constant in order to prevent attackers being able to correlate message length with number of hops
  val pki = new HashMap[String, SphinxServer]() // public key infrastructure, mapping of server ID to server, we generate the String using the Byte.toBinaryString method
  val clients = new HashMap[String, Client]()
  
  val pseudonymServer = new PseudonymServer(this)  
  
  /**
   * The Various Hashes Needed
   */
  
  def hash(data: String): Array[Byte] = {
    val digest = MessageDigest.getInstance("SHA-256");
    return digest.digest(data.getBytes("UTF-8"));
  }
  
  // Hash of alpha and s to use as a blinding factor
  def hb(alpha: BigInt, s: BigInt): BigInt = 	return group.makeExp(hash("hb:" + group.printable(alpha) + " , " + group.printable(s)))
  
  
}