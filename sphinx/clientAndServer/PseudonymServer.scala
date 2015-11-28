package sphinx.clientAndServer

import sphinx.params.Params
import scala.collection.mutable.HashMap
import sphinx.params.Methods
import scala.collection.mutable.ListBuffer

class PseudonymServer(sphinxParams: Params) {
  val params = sphinxParams
  val db = new HashMap[String, ListBuffer[(Array[Byte], (BigInt, Array[Byte], Array[Byte]), Array[Byte])]]

  def addSurb(pseudonym: Array[Byte], nymtuple: (Array[Byte], (BigInt, Array[Byte], Array[Byte]), Array[Byte])) {
    val nym = Methods.byteArrayToStringOfHex(pseudonym)
    val list = {
      if (db.contains(nym)) { val l = db.get(nym).get; l.append(nymtuple); l } 
      else { val l = new ListBuffer[(Array[Byte], (BigInt, Array[Byte], Array[Byte]), Array[Byte])](); l.append(nymtuple); l }
    }
    db.put(nym, list)
  }
  
  def sendToPseudonym(pseudonym: Array[Byte], message: Array[Byte]) {
    val nym = Methods.byteArrayToStringOfHex(pseudonym)
    println("Pseudonym server received message for: " + nym)
    if (db.contains(nym)){
      val l = db.remove(nym).get
      if (l.length > 0) {
        val (n0, header0, ktilde) = l.remove(0)
        val zeroes = Array.fill[Byte](params.k)(0)
        val body = Methods.pi(ktilde, Methods.padMsgBody(params.m, zeroes ++ message))
        params.pki.get(Methods.byteArrayToStringOfHex(n0)).get.process(header0, body)
        
        if (l.length > 0) db.put(nym, l)
      } else println("No reply blocks available for pseudonym " + nym)
    } else println("No reply blocks available for pseudonym " + nym)
  }

}