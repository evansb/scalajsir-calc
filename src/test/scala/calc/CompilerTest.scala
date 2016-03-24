package calc

import org.junit.Test
import org.scalajs.core.ir
import ir.{Trees => irt, Types => irtpe}
import TestHelper._

/** Tests focused on the Compiler.
  *
  *  You can add more "whitebox" tests here. A whitebox test checks that the
  *  compiler precisely emits the Trees we expect from some input.
  */
class SimpleValueTest {
  implicit val DummyPos = ir.Position.NoPosition

  @Test def literal(): Unit = {
    assertCompile(irt.DoubleLiteral(234), Literal(234))
  }
}
