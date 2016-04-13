package sphinx.test

import org.junit.runner.RunWith
import org.junit.runners.Suite
import sphinx.test.params.AllParamsTests
import sphinx.test.clientAndServer.client.AllClientTests
import sphinx.test.clientAndServer.server.AllServerTests
import sphinx.test.clientAndServer.pseudonymserver.AllPseudonymServerTests

@RunWith(classOf[Suite])
@Suite.SuiteClasses(Array(
    classOf[AllClientTests],
    classOf[AllParamsTests],
    classOf[AllServerTests],
    classOf[AllPseudonymServerTests]
    ))
class AllTests