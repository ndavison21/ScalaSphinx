package sphinx.test.clientAndServer.pseudonymserver

import org.junit.runner.RunWith
import org.junit.runners.Suite

@RunWith(classOf[Suite])
@Suite.SuiteClasses(Array(
    classOf[DbTest]
))
class AllPseudonymServerTests