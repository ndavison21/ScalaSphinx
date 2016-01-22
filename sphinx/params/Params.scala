package sphinx.params

import java.security.MessageDigest

import scala.collection.mutable.HashMap
import scala.util.Random

import javax.crypto.Cipher
import javax.crypto.Mac
import javax.crypto.spec.IvParameterSpec
import javax.crypto.spec.SecretKeySpec
import sphinx.clientAndServer.Client
import sphinx.clientAndServer.PseudonymServer
import sphinx.clientAndServer.SphinxServer

object Params {
  val k = 16 // security parameter, in bytes (16 bytes = 128 bits)
  val m = 1024 // size of the message body, in bytes. This needs to be kept constant in order to prevent attackers being able to correlate message length with number of hops
  val pki = new HashMap[String, SphinxServer] // public key infrastructure, mapping of server ID to server, we generate the String using the Byte.toBinaryString method
  val clients = new HashMap[String, Client]
  val pseudonymServer = new PseudonymServer

  /**
   * special destination
   */
  val dSpecial = new Array[Byte](1)
  dSpecial(0) = 0

  /**
   * any other destination
   * prefixes the destination with its length
   */
  def destinationEncode(dest: Array[Byte]): Array[Byte] = {
    assert(dest.length >=1)
    assert(dest.length <=127)
    dest.length.asInstanceOf[Byte] +: dest
  }
  
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
    for (i <- 0 until nu) out(i) = arr(Random.nextInt(nu))
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
    val paddedMsgBody: Array[Byte] = Array.fill[Byte](msgSize)((-1).asInstanceOf[Byte])

    for (i <- 0 until msgBody.length)
      paddedMsgBody(i) = msgBody(i)

    paddedMsgBody(msgBody.length) = 127 // two's complement 01111111

    paddedMsgBody
  }

  /**
   * inverse of padMsgBody, used for removing trailing padding
   *
   * TODO: currently throws ArrayIndexOutOfBounds if the string has not been padded, instead should fail nicely
   */
  def unpadMsgBody(msgBody: Array[Byte]): Array[Byte] = {

    def paddingStart(i: Int): Int = {
      if (i<0) return msgBody.length
      else if (msgBody(i) == 127) return i
      else
        paddingStart(i - 1)
    }

    msgBody.slice(0, paddingStart(msgBody.length - 1))
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
    byteArray.map("%02x".format(_)).mkString
  }

  def stringOfHexToByteArray(hexString: String): Array[Byte] = {
    hexString.replaceAll("[^0-9A-Fa-f]", "").sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
  }

  def xor(a: Array[Byte], b: Array[Byte]): Array[Byte] = {
    //require(a.length == b.length, "Byte arrays have to have the same length")

    (a.toList zip b.toList).map(elements => (elements._1 ^ elements._2).toByte).toArray
  }

  /**
   * a pseudo random generator
   * 
   * output is of length (2*r+3)*k
   */
  def rho(k: Array[Byte], p: Params): Array[Byte] = {
    assert(k.length == Params.k)
    
    val ivSpec = new IvParameterSpec(Array.fill[Byte](Params.k)(0.asInstanceOf[Byte]))
    val cipher = Cipher.getInstance("AES/CBC/NoPadding") // 128 bit key
    val key = new SecretKeySpec(k, "AES")
    cipher.init(Cipher.ENCRYPT_MODE, key, ivSpec)
    
    val enc = Array.fill[Byte](((2 * p.r) + 3) * Params.k)(0.asInstanceOf[Byte])
    val out = cipher.doFinal(enc)
    out.slice(0, ((2 * p.r) + 3) * Params.k)
  }

  /**
   * a hash to generate a key for rho()
   */
  def rhoKey(s: Array[Byte], p: Params): Array[Byte] = {
    val fullHash = hash(s)
    fullHash.slice(0, Params.k)
  }

  /**
   * The MAC, key and output both of length k
   */
  def mu(k: Array[Byte], data: Array[Byte], p: Params): Array[Byte] = {
    assert(k.length == Params.k)
    val mac = Mac.getInstance("HmacSHA256")
    val key = new SecretKeySpec(k, "HmacSHA256")
    mac.init(key)
    mac.doFinal(data).slice(0, Params.k)
  }

  /**
   * a hash to generate a key for mu()
   */
  def muKey(s: Array[Byte], p: Params): Array[Byte] = {
    val fullHash = hash(s)
    fullHash.slice(0, Params.k)
  }

  /**
   * key is of length k, data is of length m
   */
  def pi(key: Array[Byte], data: Array[Byte]): Array[Byte] = {
    assert(data.length >= 2 * k)
    assert(key.length == k)

    var l = data.slice(0, k)
    var r = data.slice(k, data.length)
    val k1, k3 = key
    
    r = xor(r, hash(xor(l, k1)))
    val k2 = xor(r.slice(r.length - k, r.length), k1)
    l = xor(l, keyedHash(k2, r))
    r = xor(r, hash(xor(l, k3)))
    val k4 = xor(r.slice(r.length - k, r.length), k3)
    l = xor(l, keyedHash(k4, r))
    
    l ++ r
  }

  /**
   * the inverse of pi
   */
  def pii(key: Array[Byte], data: Array[Byte]): Array[Byte] = {
    assert(data.length >= 2 * k)
    assert(key.length == k)

    var l = data.slice(0, k)
    var r = data.slice(k, data.length)

    val k1, k3 = key

    val k4 = xor(r.slice(r.length - k, r.length), k3)
    l = xor(l, keyedHash(k4, r))
    r = xor(r, hash(xor(l, k3)))
    val k2 = xor(r.slice(r.length - k, r.length), k1)
    l = xor(l, keyedHash(k2, r))
    r = xor(r, hash(xor(l, k1)))
    
    l ++ r
  }

  /**
   * Hash for deciding if a node has seen a secret before
   */
  def tauHash(s: Array[Byte], p: Params): Array[Byte] = {
    val fullHash = hash(s)
    fullHash.slice(0, 2 * Params.k)
  }

  /**
   * generate a key for pi
   */
  def piKey(s: Array[Byte], p: Params): Array[Byte] = {
    val fullHash = hash(s)
    fullHash.slice(0, Params.k)
  }

  /**
   * The Hashes Needed
   */
  def hash(data: Array[Byte]): Array[Byte] = {
    val digest = MessageDigest.getInstance("SHA-256");
    digest.digest(data);
  }

  def keyedHash(k: Array[Byte], data: Array[Byte]): Array[Byte] = {
    assert(k.length == Params.k)

    val ivSpec = new IvParameterSpec(Array.fill(Params.k)(0.asInstanceOf[Byte]))
    
    val cipher = Cipher.getInstance("AES/CTR/NoPadding") // 128 bit key
    val key = new SecretKeySpec(k, "AES")
    cipher.init(Cipher.ENCRYPT_MODE, key, ivSpec)
    cipher.doFinal(data)
  }

  // Hash of alpha and s to use as a blinding factor
  def hb(alpha: Array[Byte], s: Array[Byte], p: Params): Array[Byte] = p.group.makeExp(hash(alpha ++ s))

  def padTo32Bytes(a: Array[Byte]): Array[Byte] = {
    assert(a.length <= 32)
    Array.fill(32 - a.length)(0.asInstanceOf[Byte]) ++ a
  }
}

/**
 * r: minimum number of hops before reaching destination, default is 5 (TOR uses 3)
 * ecc: elliptic curve cryptography
 */
class Params(maxhops: Int, useEcc: Boolean) {
  def this() = this(5, false) // constructor with default values

  val r = maxhops
  val group: Group = {
    if (useEcc) new Group_ECC()
    else new Group_P()
  }
}