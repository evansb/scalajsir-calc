package calc

import org.junit.Test
import TestHelper._

/** End-to-end tests, with parsing, compiling and running.
 *
 *  You can add more "blackbox" unit testing here. A blackbox test checks that
 *  compiling and running some piece of code produces the expected final
 *  result.
 */
class SimpleValueRunTest {

  @Test def literal(): Unit = {
    assertRun(54.3, "54.3")
  }

}
