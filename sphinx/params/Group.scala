package sphinx.params

trait Group {
  def g: BigInt
  
	def genSecret: BigInt
	
	def expon(base: BigInt, power: BigInt)
	
	def multiExpon(base: BigInt, powers: List[BigInt]): BigInt
	
	def makeExp(data: Array[Byte]): BigInt
	
	def inGroup(alpha: BigInt): Boolean
	
	def printable(alpha: BigInt): String = alpha.toString()
}