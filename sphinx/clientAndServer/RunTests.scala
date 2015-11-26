package sphinx.clientAndServer

import sphinx.params.Params
import sphinx.params.Methods
import scala.util.Random

object RunTests {
  def main(args: Array[String]) {
    val testArr5 = new Array[Byte](5)
    Random.nextBytes(testArr5)
    val testArr20 = new Array[Byte](20)
    Random.nextBytes(testArr20)
    
    val p = new Params
    
    println("Methods.dspecial: " + Methods.dSpecial)
    println("Methods.byteArrayToString(testArr5): " + Methods.byteArrayToString(testArr5))
//    println("Methods.dEnc(testArr5):" + Methods.dEnc(testArr5))
    println("Methods.hash(\"Hello, World\"): " + Methods.byteArrayToString(Methods.hash("Hello, World")))
    println("Methods.hb(1, 1, p): " + Methods.hb(1, 1, p))
    
    val muk = Methods.muKey(1, p)
    println("Methods.muKey(1, p): " + Methods.byteArrayToString(muk))
    println("Methods.mu(muk, testArr5, p): " + Methods.byteArrayToString(Methods.mu(muk, testArr5, p)))
    
    val padded = Methods.padMsgBody(10, testArr5)
    println("Methods.padMsgBody(10, testArr5): " + Methods.byteArrayToString(padded))
    println("Methods.unpadMsgBody(padded)    : " + Methods.byteArrayToString(Methods.unpadMsgBody(padded)))
    
    var testList:List[Int] = Nil
    for(i <- 0 to 5) testList = testList ::: List(i)
    println("Methods.randomSubset(testList, 3): " + Methods.randomSubset(testList, 3))
    
    val rhok = Methods.rhoKey(1, p)
    println("Methods.rhoKey(1, p): " + Methods.byteArrayToString(rhok))
    println("Methods.rho(rhok, p): " + Methods.byteArrayToString(Methods.rho(rhok, p)))
    
    println("Methods.xor(testArr5, testArr20): " + Methods.byteArrayToString(Methods.xor(testArr5, testArr20)))
    
    
//    val client = new Client
//    var testArr5x5 = new Array[Array[Byte]](5);
//    val testArrk = new Array[Byte](p.k)
//    Random.nextBytes(testArrk)
//    for (i <- 0 until 5) testArr5x5(i) = testArr5 
//    val ((a, b, c), s) = client.createMixHeader(testArr5, testArrk, testArr5x5, p)
//    println("*********")
//    println("alpha0: " + a)
//    println("beta0 : " + Methods.byteArrayToString(b))
//    println("gamma0: " + Methods.byteArrayToString(c))
//    print("s sequence: ")
//    s.foreach { x => print(x) + "', " }
//    println
   
  }
}