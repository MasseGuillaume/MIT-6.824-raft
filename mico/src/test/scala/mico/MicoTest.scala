package mico

import zio._
import zio.test._
import zio.test.Assertion._

object MicoTest extends DefaultRunnableSpec {
  def spec = suite("MicoTest")(
    test("todo") {
      assert(1)(equalTo(1))
    }
  )
}
