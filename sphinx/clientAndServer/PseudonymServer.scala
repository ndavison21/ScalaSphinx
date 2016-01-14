package sphinx.clientAndServer

import sphinx.params.Params
import scala.collection.mutable.HashMap
import sphinx.params.Params
import scala.collection.mutable.ListBuffer

class PseudonymServer{
  val db = new HashMap[String, ListBuffer[(Array[Byte], (BigInt, Array[Byte], Array[Byte]), Array[Byte])]]

  def addSurb(pseudonym: Array[Byte], nymtuple: (Array[Byte], (BigInt, Array[Byte], Array[Byte]), Array[Byte])) {
    val nym = Params.byteArrayToStringOfHex(pseudonym)
    val list = {
      if (db.contains(nym)) { val l = db.get(nym).get; l.append(nymtuple); l } 
      else { val l = new ListBuffer[(Array[Byte], (BigInt, Array[Byte], Array[Byte]), Array[Byte])](); l.append(nymtuple); l }
    }
    db.put(nym, list)
  }
  
  def sendToPseudonym(pseudonym: Array[Byte], message: Array[Byte]) {
    val nym = Params.byteArrayToStringOfHex(pseudonym)
    println("Pseudonym server received message for: " + nym)
    if (db.contains(nym)){
      val l = db.remove(nym).get
      if (l.length > 0) {
        val (n0, header0, ktilde) = l.remove(0)
        val zeroes = Array.fill[Byte](Params.k)(0)
        val body = Params.pi(ktilde, Params.padMsgBody(Params.m, zeroes ++ message))
        println("Pseudonym server: forwarding to client")
        Params.pki.get(Params.byteArrayToStringOfHex(n0)).get.process(header0, body)
        
        if (l.length > 0) db.put(nym, l)
      } else println("No reply blocks available for pseudonym " + nym)
    } else println("No reply blocks available for pseudonym " + nym)
  }

}