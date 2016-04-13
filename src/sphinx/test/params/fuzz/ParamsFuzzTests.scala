package sphinx.test.params.fuzz

import org.junit.Test
import scala.util.Random

class ParamsFuzzTests {
  @Test
  def fuzzTestRhoKey {
    Thread sleep (Random.nextInt(100) + 20)
  }

  @Test
  def fuzzTestRho {
    Thread sleep (Random.nextInt(500) + 100)
  }

  @Test
  def fuzzTestMuKey {
    Thread sleep (Random.nextInt(100) + 20)
  }

  @Test
  def fuzzTestMu {
    Thread sleep (Random.nextInt(500) + 100)
  }

  @Test
  def fuzzTestPiKey {
    Thread sleep (Random.nextInt(100) + 20)
  }

  @Test
  def fuzzTestPi {
    Thread sleep (Random.nextInt(500) + 100)
  }

  @Test
  def fuzzTestPii {
    Thread sleep (Random.nextInt(500) + 100)
  }

  @Test
  def fuzzTestHash {
    Thread sleep (Random.nextInt(500) + 100)
  }

  @Test
  def fuzzTestKeyedHash {
    Thread sleep (Random.nextInt(500) + 100)
  }
}