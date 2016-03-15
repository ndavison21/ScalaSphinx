package sphinx.test.dissertation

import sphinx.clientAndServer.Client
import sphinx.clientAndServer.SphinxServer
import sphinx.params.Params

object MixHeaderProcessingEcc {
  def main(args: Array[String]) {
    println("Test Started")
    for (r <- 1 to 10) {

      var mean = 0D
      var variance = 0D

      runTest(r) // running once to initialise objects, so we can assume the same conditions for each iteration

      for (i <- 1 to 1000)
        runTest(r)
        
      println("r = " + r)
      Thread.sleep(5000)
    }
  }

  def runTest(r: Int) {
    Framework.cleanup
    val p = new Params(r, true)

    for (i <- 0 to r * 2) {
      new SphinxServer(p)
    }

    val client = new Client(p)

    val id = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte])
    val nodeIDs = Params.randomSubset(Params.pki.keySet.toArray, r)

    val ((a, b, c), delta) = Client.createMixHeader(Params.dSpecial, id, nodeIDs.map { x => Params.stringOfHexToByteArray(x) }, p)
  }
}