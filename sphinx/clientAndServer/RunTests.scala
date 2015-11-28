package sphinx.clientAndServer

import scala.math.BigInt.int2bigInt
import scala.util.Random

import sphinx.params.Methods
import sphinx.params.Params

object RunTests {
  def main(args: Array[String]) {
    val testArr5 = new Array[Byte](5)
    Random.nextBytes(testArr5)
    val testArr20 = new Array[Byte](20)
    Random.nextBytes(testArr20)
    
    val p = new Params
    
    println("Methods.dspecial: " + p.dSpecial)
    println("Methods.byteArrayToString(testArr5): " + Methods.byteArrayToStringOfBits(testArr5))
//    println("Methods.dEnc(testArr5):" + Methods.dEnc(testArr5))
    println("Methods.hash(\"Hello, World\"): " + Methods.byteArrayToStringOfBits(Methods.hash("Hello, World")))
    println("Methods.hb(1, 1, p): " + Methods.hb(1, 1, p))
    
    val muk = Methods.muKey(1, p)
    println("Methods.muKey(1, p): " + Methods.byteArrayToStringOfBits(muk))
    println("Methods.mu(muk, testArr5, p): " + Methods.byteArrayToStringOfBits(Methods.mu(muk, testArr5, p)))
    
    val padded = Methods.padMsgBody(10, testArr5)
    println("Methods.padMsgBody(10, testArr5): " + Methods.byteArrayToStringOfBits(padded))
    println("Methods.unpadMsgBody(padded)    : " + Methods.byteArrayToStringOfBits(Methods.unpadMsgBody(padded)))
    
    var testList:List[Int] = Nil
    for(i <- 0 to 5) testList = testList ::: List(i)
    println("Methods.randomSubset(testList, 3): " + Methods.randomSubset(testList, 3))
    
    val rhok = Methods.rhoKey(1, p)
    println("Methods.rhoKey(1, p): " + Methods.byteArrayToStringOfBits(rhok))
    println("Methods.rho(rhok, p): " + Methods.byteArrayToStringOfBits(Methods.rho(rhok, p)))
    
    println("Methods.xor(testArr5, testArr20): " + Methods.byteArrayToStringOfBits(Methods.xor(testArr5, testArr20)))
    
    val s = Methods.byteArrayToString(testArr5)
    println("Methods.byteArrayToString(testArr5): " + s)
    println("testArr5:                            " + Methods.byteArrayToStringOfBits(testArr5))
    println("Methods.byteArrayToString(testArr5): " + Methods.byteArrayToStringOfBits(Methods.stringToByteArray(s)))
    
//    val pik = Methods.piKey(1, p)
//    println("Methods.piKey(1, p): " + Methods.byteArrayToStringOfBits(pik))
//    val permuteArr5 = Methods.pi(pik, testArr5)
//    println("Methods.pi(pik, p): " + Methods.byteArrayToStringOfBits(permuteArr5))
//    println("Methods.pii(pik, permuteArr5): " + Methods.byteArrayToStringOfBits(Methods.pii(pik,permuteArr5)))
    
   
  }
}