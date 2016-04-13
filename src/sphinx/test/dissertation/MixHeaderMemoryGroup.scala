package sphinx.test.dissertation

import sphinx.clientAndServer.Client
import sphinx.clientAndServer.SphinxServer
import sphinx.params.Params

object MixHeaderMemoryGroup {
  val r = 5
  
  val p = new Params(r, false)

  for (i <- 0 to r * 2) {
    new SphinxServer(p)
  }

  val client = new Client(p)

  def test {
    for (i <- 0 to 1000) {
      println(i)

      val id = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte])
      val nodeIDs = Params.randomSubset(Params.pki.keySet.toArray, r)
      
      val message = Params.stringToByteArray("This is a Test")
      val destination = Params.stringToByteArray("nd359@cam.ac.uk")

      Client.creatForwardMessage(message, destination, nodeIDs.map { x => Params.stringOfHexToByteArray(x) }, p)
    }
  }
}