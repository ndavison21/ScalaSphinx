package sphinx.clientAndServer

import sphinx.params.Params
import scala.collection.mutable.ListBuffer
import sphinx.params.Methods

class Client {
  def createMixHeader(destination: String, identifier: Array[Byte], nodeIDs: Array[Array[Byte]], params: Params) {
    assert(nodeIDs.length <= params.r)
    assert(identifier.length == params.k)
    
    val x = params.group.genSecret
    
    val blinds = new ListBuffer[BigInt]()
    val absTuples = new Array[(BigInt, BigInt, BigInt)](nodeIDs.length)
    
    // Compute the nu (alpha, b, s) tuples
    for (i <- 0 until nodeIDs.length) {
      val alpha = params.group.multiExpon(params.group.g, blinds.toList) // group elements
      val s = params.group.multiExpon(params.pki.get(Methods.byteArrayToString(nodeIDs(i))).get.y, blinds.toList) // D-H Shared secrets
      val b = params.hb(alpha, s) // blinding factors
      blinds.append(b)
      absTuples(i) = (alpha, s, b)
    }
    
    // Computer the nu filler strings (phi)
    val phi0 = ""
    for (i <- 1 until nodeIDs.length) {
      
    }
  }
  
  
  
  
  def main(args: Array[String]) {
//    def list = randomSubset(1 to 10 toList, 5)
//    list.foreach { println }
    
    def b: Byte = -1
    println(b.toBinaryString)
    
  }
}
