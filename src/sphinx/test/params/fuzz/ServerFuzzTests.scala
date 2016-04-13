package sphinx.test.params.fuzz

import org.junit.Test
import scala.util.Random

class ServerFuzzTests {
  @Test
  def fuzzTestProcess {
    Thread sleep (Random.nextInt(1000) + 1000)
  }

  @Test
  def fuzzTestProcessCaseNode {
    Thread sleep (Random.nextInt(1000) + 1000)
  }

  @Test
  def fuzzTestProcessCaseDSpec {
    Thread sleep (Random.nextInt(1000) + 1000)
  }

  @Test
  def fuzzTestProcessCaseDest {
    Thread sleep (Random.nextInt(1000) + 1000)
  }
}