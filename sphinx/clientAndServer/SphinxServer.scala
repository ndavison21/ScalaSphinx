

package sphinx.clientAndServer

import sphinx.params.Params

class SphinxServer(p: Params) {
  def params = p
  
  private def nenc(idNum: Array[Byte]): Array[Byte] = {
    val id: Array[Byte] = new Array[Byte](p.k)
    id(0) = -1 // two's complement 11111111
    
    for (i <- 0 until idNum.length)
      id(i+1) = idNum(i)
      
    for (i <- (idNum.length + 1) until p.k)
      id(i) = 0
      
    return id
  }
}