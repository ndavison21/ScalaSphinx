package sphinx.params

import java.security.MessageDigest

import scala.util.Random

import edu.biu.scapi.primitives.prf.PseudorandomPermutation
import edu.biu.scapi.tools.Factories.PrfFactory
import javax.crypto.Cipher
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

/**
 * TODO: look into using BitSet instead of Array[Byte]
 */
object Methods {

  /**
   * Returns a list of nu random elements of the input (with replacement)
   */
  def randomSubset[T](list: List[T], nu: Int): List[T] = {

    def collect(vect: Vector[T], sampleSize: Int, acc: List[T]): List[T] = {
      if (sampleSize == 0) acc
      else {
        val index = Random.nextInt(vect.size)
        collect(vect, sampleSize - 1, vect(index) :: acc)
      }
    }

    collect(list toVector, nu, Nil)
  }
  
  def randomSubset[T: ClassManifest](arr: Array[T], nu: Int): Array[T] = {
    val out = new Array[T](nu)
    for (i <- 0 to nu) out(i) = arr(Random.nextInt(nu))
    out
  }
  
  def stringToByteArray(s: String): Array[Byte] = {
    s.toCharArray.map(_.toByte)
  }
  def byteArrayToString(a: Array[Byte]): String = {
    new String(a.map(_.toChar))
  }

  /**
   * used to add padding to make sure there is no correlation between message size and number of 'hops'
   *
   * msgSize: the size (in bits) the message needs to be padded to
   * msgBody: the current body, padding is added as a 0 followed by as many 1's as the message needs to be padded to
   */
  def padMsgBody(msgSize: Int, msgBody: Array[Byte]): Array[Byte] = {
    val paddedMsgBody: Array[Byte] = new Array[Byte](msgSize)

    for (i <- 0 until msgBody.length) // 'until' goes to msgBody.length - 1, 'to' goes to msgBody.length
      paddedMsgBody(i) = msgBody(i)

    paddedMsgBody(msgBody.length) = 127 // two's complement 01111111
    for (i <- msgBody.length + 1 until msgSize)
      paddedMsgBody(i) = -1 // two's complement 11111111

    paddedMsgBody
  }

  /**
   * inverse of padMsgBody, used for removing trailing padding
   *
   * TODO: currently throws ArrayIndexOutOfBounds if the string has not been padded, instead should fail nicely
   */
  def unpadMsgBody(msgBody: Array[Byte]): Array[Byte] = {

    def paddingStart(i: Int, msg: Array[Byte]): Int = {
      if (msg(i) == 127) i
      else
        paddingStart(i - 1, msg)
    }

    msgBody.slice(0, paddingStart(msgBody.length - 1, msgBody))
  }

  /**
   * any other destination
   *
   * TODO: check implementation is correct
   */
  def dEnc(destination: String): Array[Byte] = {
    assert(destination.length > 0 && destination.length < 128)
    
    destination.length.toByte +: stringToByteArray(destination)
  }

  def byteArrayToStringOfBits(byteArray: Array[Byte]): String = {
    def innerByteArrayToString(i: Int, byteArray: Array[Byte], bitString: String): String = {
      if (i == byteArray.length) bitString
      else
        innerByteArrayToString(i + 1, byteArray, bitString + String.format("%8s", Integer.toBinaryString(byteArray(i) & 0xFF)).replace(' ', '0'))
        
    }

    innerByteArrayToString(0, byteArray, "")
  }
  
  def byteArrayToStringOfHex(byteArray: Array[Byte]): String = {
    def innerByteArrayToString(i: Int, byteArray: Array[Byte], hexString: String): String = {
      if (i == byteArray.length) hexString
      else
        innerByteArrayToString(i + 1, byteArray, hexString + String.format("%8s", Integer.toHexString(byteArray(i) & 0xFF)).replace(' ', '0'))
        
    }

    innerByteArrayToString(0, byteArray, "")
  }

  def xor(a: Array[Byte], b: Array[Byte]): Array[Byte] = {
    //require(a.length == b.length, "Byte arrays have to have the same length")

    (a.toList zip b.toList).map(elements => (elements._1 ^ elements._2).toByte).toArray
  }

  /**
   * a pseudo random generator
   */
  def rho(k: Array[Byte], p: Params): Array[Byte] = {
    assert(k.length == p.k)
    val cipher = Cipher.getInstance("AES/CBC/NoPadding") // 128 bit key
    val key = new SecretKeySpec(k, "AES")
    cipher.init(Cipher.ENCRYPT_MODE, key)
    val enc = new Array[Byte](((2 * p.r) + 3) * p.k)
    for (i <- 0 until enc.length) enc(i) = 0
    cipher.doFinal(enc)
  }

  /**
   * a hash to generate a key for rho()
   */
  def rhoKey(s: BigInt, p: Params): Array[Byte] = {
    val fullHash = hash(byteArrayToStringOfHex(s.toByteArray))
    fullHash.slice(0, p.k)
  }
  
  /**
   * The MAC, key and output both of length k
   */
  def mu(k: Array[Byte], data: Array[Byte], p: Params): Array[Byte] = {
    assert(k.length == p.k)
    val mac = Mac.getInstance("HmacSHA1")
    val key = new SecretKeySpec(k, "HmacSHA1")
    mac.init(key)
    mac.doFinal(data)
  }
  
  /**
   * a hash to generate a key for mu()
   */
  def muKey(s: BigInt, p: Params): Array[Byte] = {
    val fullHash = hash(byteArrayToStringOfHex(s.toByteArray))
    fullHash.slice(0, p.k)
  }
  
   /**
    * a family of pseudo-random permutations (PRPs)
    * key is of length k, data is of length m
    */
  def pi(k: Array[Byte], data: Array[Byte]): Array[Byte] = {
    val prp = PrfFactory.getInstance.getObject("AES", "OpenSSL")
                          .asInstanceOf[PseudorandomPermutation]
    val key = new SecretKeySpec(k, "AES")
    prp.setKey(key)
    val out = new Array[Byte](data.length)
    prp.computeBlock(data, 0, out, 0)
    out
  }
  
  /**
   * the inverse of pi
   */
  def pii(k: Array[Byte], data: Array[Byte]): Array[Byte] = {
    val prp: PseudorandomPermutation  = PrfFactory.getInstance.getObject("AES", "OpenSSL")
                                                    .asInstanceOf[PseudorandomPermutation]
    val key = new SecretKeySpec(k, "AES")
    prp.setKey(key)
    val out = new Array[Byte](data.length)
    prp.invertBlock(data, 0, out, 0)
    out
  }
  
  /**
   * a has to generate a key for pi()
   */
  def piKey(s: BigInt, p: Params): Array[Byte] = {
    val fullHash = hash(byteArrayToStringOfHex(s.toByteArray))
    fullHash.slice(0, p.k)
  }

  /**
   * The Various Hashes Needed
   */
  def hash(data: String): Array[Byte] = {
    val digest = MessageDigest.getInstance("SHA-256");
    digest.digest(data.getBytes("UTF-8"));
  }

  // Hash of alpha and s to use as a blinding factor
  def hb(alpha: BigInt, s: BigInt, p:Params): BigInt = p.group.makeExp(hash("hb:" + p.group.printable(alpha) + " , " + p.group.printable(s)))

}