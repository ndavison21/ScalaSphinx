

package sphinx.clientAndServer

import sphinx.params.Params
import scala.util.Random
import sphinx.params.Methods
import scala.collection.mutable.HashMap

class SphinxServer(p: Params) {
  def params = p
  val x = params.group.genSecret
  val y = params.group.expon(params.group.g, x)
  val idNum = new Array[Byte](4)
  Random.nextBytes(idNum)
  val id = nenc(idNum)
  val name = "Node " + Methods.byteArrayToStringOfHex(idNum)
  val seen = new HashMap[String, Boolean]
  params.pki.put(Methods.byteArrayToStringOfHex(id), this)
  
  
  
  private def nenc(idNum: Array[Byte]): Array[Byte] = {
    val id: Array[Byte] = new Array[Byte](p.k)
    id(0) = -1 // two's complement 11111111
    
    for (i <- 0 until idNum.length)
      id(i+1) = idNum(i)
      
    for (i <- (idNum.length + 1) until p.k)
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
    if (s(0) == -1) ("node", s.slice(0, params.k), s.slice(params.k, s.length))
    val l = s(0)
    ("dest", s.slice(1, l+1), s.slice(l+1, s.length)) // TODO Check this does what it should
  }
  
  def process(header: Array[Byte], delta: Array[Byte]) {
    
  }
}