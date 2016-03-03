package sphinx.test.params

import org.junit.runner.RunWith
import org.junit.runners.Suite

@RunWith(classOf[Suite])
@Suite.SuiteClasses(Array(
    classOf[TestDestinationEncode],
    classOf[TestPi],
    classOf[TestRandomSubset],
    classOf[TestStringByteConversion],
    classOf[TestMessageBodyPadding]
    ))
class AllParamsTests