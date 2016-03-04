package sphinx.test

import org.junit.runner.RunWith
import org.junit.runners.Suite
import sphinx.test.params.AllParamsTests
import sphinx.test.clientAndServer.client.AllClientTests

@RunWith(classOf[Suite])
@Suite.SuiteClasses(Array(
    classOf[AllClientTests],
    classOf[AllParamsTests]
    ))
class AllTests