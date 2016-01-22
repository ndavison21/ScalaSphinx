package sphinx.params

import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION

import sphinx.curve25519.Keys.Private
import sphinx.curve25519.Keys.Public

class Group_ECC extends Group {
  // Group operations in ECC
  
  val g = Array.fill[Byte](31)(0.asInstanceOf[Byte]) :+ 9.asInstanceOf[Byte]
	
	def genSecret: Array[Byte] = {
	  val key = new Private
	  key.priv
	}
	
	def expon(base: Array[Byte], power: Array[Byte]): Array[Byte] = {
	  val key = new Private(power)
	  key.get_shared_key(new Public(base))
	}
	
	def multiExpon(base: Array[Byte], powers: List[Array[Byte]]): Array[Byte] = {
	  val baseandexps = base :: powers
	  baseandexps.reduceLeft(expon)
	}
	
	def makeExp(data: Array[Byte]): Array[Byte] = {
	  assert(data.length == 32)
	  val key = new Private(data)
	  key.priv
	}
	
	def inGroup(alpha: Array[Byte]): Boolean = (alpha.length == 32) // All strings of length 32 are in the group, says DJB

}