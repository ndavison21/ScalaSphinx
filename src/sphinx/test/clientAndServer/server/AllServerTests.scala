package sphinx.test.clientAndServer.server

import org.junit.runner.RunWith
import org.junit.runners.Suite

@RunWith(classOf[Suite])
@Suite.SuiteClasses(Array(
    classOf[TestNodeNameEncode],
    classOf[TestProcessEcc],
    classOf[TestProcessGroup]
    ))
class AllServerTests