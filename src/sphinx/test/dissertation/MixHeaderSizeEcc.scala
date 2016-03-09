package sphinx.test.dissertation

import sphinx.params.Params
import sphinx.clientAndServer.SphinxServer
import sphinx.clientAndServer.Client

object MixHeaderSizeEcc {
  
  def main(args: Array[String]) {
    Framework.writeToFile("output/mixHeaderSizeEcc.csv", "r,header-size\n")
    for (r <- 1 to 100) {
      Framework.cleanup
      val p = new Params(r, true)
      
      for (i <- 0 to r*2) {
        new SphinxServer(p)
      }
      
      val client = new Client(p)
      
      val id = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte])
      val nodeIDs = Params.randomSubset(Params.pki.keySet.toArray, r)

      val ((a,b,c), delta)  = Client.createMixHeader(Params.dSpecial, id, nodeIDs.map{ x => Params.stringOfHexToByteArray(x) }, p)
      val overhead = a.length + b.length + c.length + delta.length
      println("r = " + r + " overhead = " + overhead)
      Framework.appendToFile("output/mixHeaderSizeEcc.csv", r + "," + overhead)
    }
  }
}