package sphinx.test.dissertation

import sphinx.params.Params
import sphinx.clientAndServer.Client
import sphinx.clientAndServer.SphinxServer

object RunTests {
  def main(args: Array[String]) {
    benchmark(1000)
    

    def testSystem(args: Array[String]) {
      // Cleaning up
      Params.pki.clear()
      Params.clients.clear()
      Params.pseudonymServer.db.clear()

      // Simulating input from command line
      val useEcc = (args.length > 0 && args(0) == "-ecc")
      val r = 5 // path length
      val p = new Params(r, useEcc)

      // Create some sphinx servers (they add themselves to the pki)
      for (i <- 0 to r * 2) {
        new SphinxServer(p)
      }

      // Create a client
      val client = new Client(p)

      // Creating a forward message
      val useNodes = Params.randomSubset(Params.pki.keySet.toArray, r)
      val (header, delta) = Client.creatForwardMessage(Params.stringToByteArray("This is a test"), Params.stringToByteArray("natemail@mail.com"), useNodes.map { x => Params.stringOfHexToByteArray(x) }, p)

      // Sending a forward message through the mix
      Params.pki.get(useNodes(0)).get.process(header, delta)

      // Creating a reply block for the client
      client.createPseudonymReply(Params.stringToByteArray("nd359"), r)

      // Send a message using the reply block
      Params.pseudonymServer.sendToPseudonym(Params.stringToByteArray("nd359"), Params.stringToByteArray("This is a reply test"))

    }

    def benchmark(iterations: Integer) {
      var time = 0L
      var ecc_time = 0L
      for (i <- 1 to iterations.toInt) {
        val start = System.nanoTime()
        testSystem(Array())
        val end = System.nanoTime()
        time += end - start

        val ecc_start = System.nanoTime()
        testSystem(Array("-ecc"))
        val ecc_end = System.nanoTime()
        ecc_time += ecc_end - ecc_start
      }

      println("average time using prime group: " + time / iterations)
      println("average time using Curve25519:  " + ecc_time / iterations)
      println("Curve25519 is on average " + time.toDouble / ecc_time + " times faster")
    }

  }
}