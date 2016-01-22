package sphinx.params

trait Group {
  def g: Array[Byte]
  
	def genSecret: Array[Byte]
	
	def expon(base: Array[Byte], power: Array[Byte]): Array[Byte]
	
	def multiExpon(base: Array[Byte], powers: List[Array[Byte]]): Array[Byte]
	
	def makeExp(data: Array[Byte]): Array[Byte]
	
	def inGroup(alpha: Array[Byte]): Boolean
	
	def printable(alpha: Array[Byte]): String = Params.byteArrayToStringOfBits(alpha)
	
}