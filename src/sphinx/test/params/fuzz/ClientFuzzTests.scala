package sphinx.test.params.fuzz

import org.junit.Test
import scala.util.Random

class ClientFuzzTests {
  @Test
  def fuzzTestCreateMixHeader {
    Thread sleep (Random.nextInt(1000) + 1000)
  }
  
  @Test
  def fuzzTestCreateForwardMessage {
    Thread sleep (Random.nextInt(1000) + 1000)
  }
  
  @Test
  def fuzzTestCreateSurb {
    Thread sleep (Random.nextInt(1000) + 800)
  }
}