package labrpc

import zio._
import zio.test._
import zio.test.Assertion._

object LabRCPTest extends DefaultRunnableSpec {
  def spec = suite("LabRCPTest")(
    test("todo") {
      assert(1)(equalTo(1))
    }
  )
}
