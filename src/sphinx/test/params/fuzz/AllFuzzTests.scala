package sphinx.test.params.fuzz

import org.junit.runner.RunWith
import org.junit.runners.Suite

@RunWith(classOf[Suite])
@Suite.SuiteClasses(Array(
    classOf[ParamsFuzzTests],
    classOf[ClientFuzzTests],
    classOf[ServerFuzzTests]
))
class AllFuzzTests