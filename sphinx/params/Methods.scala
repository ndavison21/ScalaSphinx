package sphinx.params

import util.Random

/**
 * TODO: look into using BitSet instead of Array[Byte]
 */
object Methods {

  /**
   * Returns a list of nu random elements of the input (with replacement)
   */
  def randomSubset[T](list: List[T], nu: Int) = {

    def collect(vect: Vector[T], sampleSize: Int, acc: List[T]): List[T] = {
      if (sampleSize == 0) acc
      else {
        val index = Random.nextInt(vect.size)
        collect(vect, sampleSize - 1, vect(index) :: acc)
      }
    }

    collect(list toVector, nu, Nil)
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

    return paddedMsgBody
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

    return msgBody.slice(0, paddingStart(msgBody.length - 1, msgBody))
  }

  /**
   * special destination
   */
  val dSpecial = 0

  /**
   * any other destination
   *
   * TODO: check type needed of output
   */
  def dEnc(destination: Array[Byte]): String = {
    assert(destination.length > 0 && destination.length < 128)

    return destination.length.toChar + byteArrayToString(destination)
  }

  /**
   * Used for accessing the node in the pki
   */
  def byteArrayToString(byteArray: Array[Byte]): String = {
    def innerByteArrayToString(i: Int, byteArray: Array[Byte], bitString: String): String = {
      if (i == byteArray.length) bitString
      else
        innerByteArrayToString(i + 1, byteArray, bitString + byteArray(i).toBinaryString)
    }
    
    return innerByteArrayToString(0, byteArray, "")
  }
  

}