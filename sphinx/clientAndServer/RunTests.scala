package sphinx.clientAndServer

import scala.math.BigInt.int2bigInt
import scala.util.Random

import sphinx.params.Params

object RunTests {
  def main(args: Array[String]) {
    def testParamsMethods {
      val testArr5 = new Array[Byte](5)
      Random.nextBytes(testArr5)
      val testArr20 = new Array[Byte](20)
      Random.nextBytes(testArr20)
  
      val p = new Params
  
      println("Params.dspecial: " + Params.dSpecial)
      println("Params.byteArrayToStringOfBits(testArr5): " + Params.byteArrayToStringOfBits(testArr5))
      println("Params.byteArrayToStringOfHex(testArr5): " + Params.byteArrayToStringOfHex(testArr5))
      //    println("Params.dEnc(testArr5):" + Params.dEnc(testArr5))
      println("Params.hash(\"Hello, World\"): " + Params.byteArrayToStringOfBits(Params.hash("Hello, World")))
      println("Params.hb(1, 1, p): " + Params.hb(1, 1, p))
  
      val muk = Params.muKey(1, p)
      println("Params.muKey(1, p): " + Params.byteArrayToStringOfBits(muk))
      println("Params.mu(muk, testArr5, p): " + Params.byteArrayToStringOfBits(Params.mu(muk, testArr5, p)))
  
      val padded = Params.padMsgBody(10, testArr5)
      println("Params.padMsgBody(10, testArr5): " + Params.byteArrayToStringOfBits(padded))
      println("Params.unpadMsgBody(padded)    : " + Params.byteArrayToStringOfBits(Params.unpadMsgBody(padded)))
  
      var testList: List[Int] = Nil
      for (i <- 0 to 5) testList = testList ::: List(i)
      println("Params.randomSubset(testList, 3): " + Params.randomSubset(testList, 3))
  
      val rhok = Params.rhoKey(1, p)
      println("Params.rhoKey(1, p): " + Params.byteArrayToStringOfBits(rhok))
      println("Params.rho(rhok, p): " + Params.byteArrayToStringOfBits(Params.rho(rhok, p)))
  
      println("Params.xor(testArr5, testArr20): " + Params.byteArrayToStringOfBits(Params.xor(testArr5, testArr20)))
  
      val s = Params.byteArrayToString(testArr5)
      println("Params.byteArrayToString(testArr5): " + s)
      println("testArr5:                            " + Params.byteArrayToStringOfBits(testArr5))
      println("Params.byteArrayToString(testArr5): " + Params.byteArrayToStringOfBits(Params.stringToByteArray(s)))
  
      //    val pik = Params.piKey(1, p)
      //    println("Params.piKey(1, p): " + Params.byteArrayToStringOfBits(pik))
      //    val permuteArr5 = Params.pi(pik, testArr5)
      //    println("Params.pi(pik, p): " + Params.byteArrayToStringOfBits(permuteArr5))
      //    println("Params.pii(pik, permuteArr5): " + Params.byteArrayToStringOfBits(Params.pii(pik,permuteArr5)))

    }
    
    def testSystem(args: Array[String]) {
      val useEcc = (args.length > 0 && args(0) == "-ecc")
      if (useEcc) println("ECC is not currently implemented, defaulting to basic group") // TODO Remove this once ECC is implemented
      val r = 5
      val p = new Params(r, useEcc) // TODO Check pseudonym server is correctly passed

      // Create some sphinx servers (they add themselves to the pki)
      for (i <- 0 to r * 2)  {
        new SphinxServer(p)
        println("Params.pki.size = " + Params.pki.size)
      }

      // Create a client
      val client = new Client(p)

      val useNodes = Params.randomSubset(Params.pki.keySet.toArray, r)
      val (header, delta) = Client.creatForwardMessage(Params.stringToByteArray("This is a test"), "dest", useNodes.map { x => Params.stringToByteArray(x) }, p)

      // Send it to the first node for processing
      Params.pki.get(useNodes(0)).get.process(header, delta)

      // Create a reply block for the client
      client.createPseudonymReply(Params.stringToByteArray("nd359"), r)

      // Send a message to it
      Params.pseudonymServer.sendToPseudonym(Params.stringToByteArray("nd359"), Params.stringToByteArray("This is a reply test"))

    }
    
    // testParamsMethods
    testSystem(new Array[String](0))

  }
}