package porcEpic

import zio._
import zio.test._
import zio.test.Assertion._

object PorcEpicTest extends DefaultRunnableSpec {
  def spec = suite("PorcEpicTest")(
    test("todo") {
      assert(1)(equalTo(1))
    }
  )
}
