package calc

import org.junit.Assert._
import org.scalajs.core.ir
import org.scalajs.core.ir.{Trees => irt, Types => irtpe}
import org.scalajs.core.tools.logging.NullLogger
import org.scalajs.jsenv.JSConsole

sealed trait ComparisonMethod

case object ExactString extends ComparisonMethod
case object ApproxDouble extends ComparisonMethod

object TestHelpers {

  implicit val DummyPos = ir.Position.NoPosition

  private val epsilon = 1e-5

  def assertRun(expected: Double, code: String)(implicit comparison: ComparisonMethod): Unit = {
    val tree = Parser.parse(code).get.value
    val classDef = Compiler.compileMainClass(tree)
    val linked = Linker.link(classDef, NullLogger)

    val lines = new java.io.StringWriter
    val console = new JSConsole {
      def log(msg: Any): Unit = lines.append(msg.toString() + "\n")
    }

    Runner.run(linked, NullLogger, console)

    comparison match  {
      case ExactString => assertEquals(expected.toString(), lines.toString().trim)
      case ApproxDouble => assertEquals(expected, lines.toString().toDouble, epsilon)
    }
  }

  def assertCompile(expected: irt.Tree, sourceTree: Tree): Unit = {
    /* IR Trees do not have a meaningful equals() method, so we test equality
     * through hashes.
     */

    def hashOf(body: irt.Tree): irt.TreeHash = {
      // Can only hash entire methods
      val methodDef = irt.MethodDef(static = false, irt.Ident("main__D"),
        Nil, irtpe.DoubleType, body)(
        irt.OptimizerHints.empty, None)
      ir.Hashers.hashMethodDef(methodDef).hash.get
    }

    val expectedHash = hashOf(expected)
    val actual = Compiler.compileExpr(sourceTree)
    val actualHash = hashOf(actual)

    assertTrue(s"Expected $expected but got $actual",
      ir.Hashers.hashesEqual(actualHash, expectedHash, considerPos = true))
  }
}
