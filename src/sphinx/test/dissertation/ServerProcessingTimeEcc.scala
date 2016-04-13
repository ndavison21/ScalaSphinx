package sphinx.test.dissertation

import sphinx.clientAndServer.Client
import sphinx.params.Params

object ServerProcessingTimeEcc {

  def main(args: Array[String]) {
    Framework.writeToFile("output/serverProcessingTimeEcc.csv", "r,avg. time, variance\n")

    for (r <- 1 to 10) {
      var mean = 0D
      var variance = 0D
      
      runTest(r) // running once to initialise objects, so we can assume the same conditions for each iteration

      for (i <- 1 to 10000) {
        val time = runTest(r)
        val prev_mean = mean
        mean = mean + (time - mean) / i
        variance = ((i-1)*variance + (time - prev_mean)*(time - mean))/i
      }

      val samplevar = variance

      println("r = " + r + " avg. time = " + mean + " variance = " + samplevar)
      Framework.appendToFile("output/serverProcessingTimeEcc.csv", r + "," + mean + "," + samplevar)
    }
  }

  def runTest(r: Int): Long = {
    Framework.cleanup
    val p = new Params(r, true)

    for (i <- 0 to r * 2) {
      new SphinxServerModified(p)
    }
    val client = new Client(p)

    val useNodes = Params.randomSubset(Params.pki.keySet.toArray, r)
    val (header, delta) = Client.creatForwardMessage(Params.stringToByteArray("This is a test"), Params.stringToByteArray("natemail@mail.com"), useNodes.map { x => Params.stringOfHexToByteArray(x) }, p)

    val server = Params.pki.get(useNodes(0)).get
    val start = System.nanoTime
    server.process(header, delta)
    val end = System.nanoTime

    return end - start
  }
}