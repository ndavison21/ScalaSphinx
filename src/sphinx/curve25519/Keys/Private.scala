package sphinx.curve25519.Keys

import scala.util.Random
import sphinx.params.Params
import sphinx.curve25519.Curve25519

class Private(sec: Array[Byte], seed: Array[Byte]) {
  def this() = this(null, null)
  def this(secret: Array[Byte]) = this(secret, null)
  
  
  var priv = new Array[Byte](32)
  if (sec == null) {
    if (seed == null) Random.nextBytes(priv)
    else priv = Params.hash("curve25519-private:".getBytes ++ seed)
  } else {
    assert(seed == null, "Provide secret, seed, or neither but not both")
    assert(sec.length == 32, "Secret must be 32 bytes long")
    priv = sec
  }
  Curve25519.clamp(priv) // TODO: check this works correctly
  
  
  def get_public: Array[Byte] = {
    val p = new Array[Byte](32)
    val s = new Array[Byte](32)
    Curve25519.keygen(p, s, priv)
    return p
  }
  
  def get_shared_key(public:Public): Array[Byte] = {
    var z = new Array[Byte](32)
    Curve25519.curve(z, priv, public.pub)
    return z
  }
  
}