package mico

import zio._
import zio.test._
import zio.test.Assertion._

object TatuTest extends DefaultRunnableSpec {
  def spec = suite("TatuTest")(
    test("todo") {
      assert(1)(equalTo(1))
    }
  )
}
