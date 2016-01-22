package sphinx.curve25519.Keys

class Public(p: Array[Byte]) {
  assert(p.length == 32)
  val pub = p
}