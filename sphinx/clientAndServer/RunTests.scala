package sphinx.clientAndServer

import scala.math.BigInt.int2bigInt
import scala.util.Random
import sphinx.params.Params
import java.io.PrintStream
import java.io.FileOutputStream
import java.io.BufferedOutputStream
import java.util.Calendar

object RunTests {
  def main(args: Array[String]) {

    def testParamsMethods {
      val testArr5 = new Array[Byte](5)
      Random.nextBytes(testArr5)
      val testArr20 = new Array[Byte](20)
      Random.nextBytes(testArr20)

      val p = new Params

      //      println("Params.dspecial: " + Params.dSpecial)
      //      println("Params.byteArrayToStringOfBits(testArr5): " + Params.byteArrayToStringOfBits(testArr5))
      //      val testArr5HexString = Params.byteArrayToStringOfHex(testArr5)
      //      println("Params.byteArrayToStringOfHex(testArr5): " + testArr5HexString)
      //      println("testArr5:                                         " + Params.byteArrayToStringOfBits(testArr5))
      //      println("Params.stringOfHexToByteArray(testArr5HexString): " + Params.byteArrayToStringOfBits(Params.stringOfHexToByteArray(testArr5HexString))) 
      //      //    println("Params.dEnc(testArr5):" + Params.dEnc(testArr5))
      //      println("Params.hash(\"Hello, World\"): " + Params.byteArrayToStringOfBits(Params.hash(Params.stringToByteArray("Hello, World"))))
      //      println("Params.hb(1, 1, p): " + Params.hb(1, 1, p))
      //  
      val muk = Params.muKey(1, p)
      println("Params.muKey(1, p): " + Params.byteArrayToStringOfBits(muk))
      println("Params.mu(muk, testArr5, p): " + Params.byteArrayToStringOfBits(Params.mu(muk, testArr5, p)))
      println("Params.mu(muk, testArr5, p): " + Params.byteArrayToStringOfBits(Params.mu(muk, testArr5, p)))

      //  
      //      val padded = Params.padMsgBody(10, testArr5)
      //      println("Params.padMsgBody(10, testArr5): " + Params.byteArrayToStringOfBits(padded))
      //      println("Params.unpadMsgBody(padded)    : " + Params.byteArrayToStringOfBits(Params.unpadMsgBody(padded)))
      //  
      //      var testList: List[Int] = Nil
      //      for (i <- 0 to 5) testList = testList ::: List(i)
      //      println("Params.randomSubset(testList, 3): " + Params.randomSubset(testList, 3))
      //  
      //      val rhok = Params.rhoKey(1, p)
      //      println("Params.rhoKey(1, p): " + Params.byteArrayToStringOfHex(rhok))
      //      println("Params.rho(rhok, p): " + Params.byteArrayToStringOfHex(Params.rho(rhok, p)))
      //      println("Params.rho(rhok, p): " + Params.byteArrayToStringOfHex(Params.rho(rhok, p)))
      //      println("Params.rho(rhok, p): " + Params.byteArrayToStringOfHex(Params.rho(rhok, p)))
      //      println("Params.rho(rhok, p): " + Params.byteArrayToStringOfHex(Params.rho(rhok, p)))
      //  
      //      println("Params.xor(testArr5, testArr20): " + Params.byteArrayToStringOfBits(Params.xor(testArr5, testArr20)))
      //  
      //      val s = Params.byteArrayToString(testArr5)
      //      println("Params.byteArrayToString(testArr5): " + s)
      //      println("testArr5:                           " + Params.byteArrayToStringOfBits(testArr5))
      //      println("Params.byteArrayToString(testArr5): " + Params.byteArrayToStringOfBits(Params.stringToByteArray(s)))

    }

    def testPRP {
      val testArr2k = new Array[Byte](2 * Params.k)
      Random.nextBytes(testArr2k)

      val p = new Params

      val pik = Params.piKey(1, p)
      println("Params.piKey(1, p): " + Params.byteArrayToStringOfBits(pik))

      //val testXor = Params.xor(Params.xor(testArr2k, pik), pik)
      // println("testArr2k: " + Params.byteArrayToStringOfBits(testArr2k))
      //println("testXor  : " + Params.byteArrayToStringOfBits(testXor))

      //println("Params.hash(testArr2k): " + Params.byteArrayToStringOfBits(Params.hash(testArr2k)))

      val permuteArr2k = Params.pi(pik, testArr2k)
      println("Params.pi(pik, testArr2k)    : " + Params.byteArrayToStringOfBits(permuteArr2k))
      val unpermuteArr2k = Params.pii(pik, permuteArr2k)
      println("testArr2k                    : " + Params.byteArrayToStringOfBits(testArr2k))
      println("Params.pii(pik, permuteArr2k): " + Params.byteArrayToStringOfBits(unpermuteArr2k))

      println

      val ipermuteArr2k = Params.pii(pik, testArr2k)
      println("Params.pii(pik, testArr2k)    : " + Params.byteArrayToStringOfBits(ipermuteArr2k))
      val unipermuteArr2k = Params.pi(pik, ipermuteArr2k)
      println("testArr2k                     : " + Params.byteArrayToStringOfBits(testArr2k))
      println("Params.pi(pik, ipermuteArr2k) : " + Params.byteArrayToStringOfBits(unipermuteArr2k))

    }

    def testSystem(args: Array[String]) {
      val useEcc = (args.length > 0 && args(0) == "-ecc")
      if (useEcc) println("ECC is not currently implemented, defaulting to basic group") // TODO Remove this once ECC is implemented
      val r = 5
      val p = new Params(r, useEcc)

      // Create some sphinx servers (they add themselves to the pki)
      for (i <- 0 to r * 2) {
        new SphinxServer(new Params(r, useEcc))
      }

      // Create a client
      val client = new Client(new Params(r, useEcc))

      val useNodes = Params.randomSubset(Params.pki.keySet.toArray, r)
      println("Creating forward message")
      val (header, delta) = Client.creatForwardMessage(Params.stringToByteArray("This is a test"), Params.stringToByteArray("natemail@mail.com"), useNodes.map { x => Params.stringOfHexToByteArray(x) }, p)
      println("Finished Creating forward message")

      // Send it to the first node for processing
      println
      println("Processing message")
      Params.pki.get(useNodes(0)).get.process(header, delta)
      println("Finished Processing Message")
      println
      // Create a reply block for the client
      println("Creating a reply block")
      client.createPseudonymReply(Params.stringToByteArray("nd359"), r)
      println("Finished creating a reply block")
      println
      // Send a message to it
      println("Using the reply block")
      Params.pseudonymServer.sendToPseudonym(Params.stringToByteArray("nd359"), Params.stringToByteArray("This is a reply test"))
      println("Finished using the reply block")

    }

    println(Calendar.getInstance.getTime)

    // testParamsMethods
    // testPRP
    testSystem(new Array[String](0))

    println(Calendar.getInstance.getTime)

  }
}