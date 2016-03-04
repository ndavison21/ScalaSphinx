package sphinx.test.clientAndServer.client

import org.junit.runner.RunWith
import org.junit.runners.Suite

@RunWith(classOf[Suite])
@Suite.SuiteClasses(Array(
    classOf[TestCreateMessageHeaderGroup],
    classOf[TestCreateMessageHeaderEcc],
    classOf[TestCreateForwardMessageGroup],
    classOf[TestCreateForwardMessageEcc],
    classOf[TestCreateSURB]
    ))
class AllClientTests