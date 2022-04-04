package porcEpic

import zio._
import zio.test._
import zio.test.Assertion._

object PorcEpicTest extends DefaultRunnableSpec {

  enum Input:
    case Put(value: Int) extends Input
    case Get extends Input

  import Input._
  
  val model = new Model[Int, RegisterInput]{

    def initial: Int = 0

    def step(state: Int, input: Input, output: Int): (Boolean, S) = {
      input match {
        case Put(value) => (true, value)
        case Get => (output == state, state)
      }
    }

    def describeOperation(input: Input, output: Int): String = {
      input match {
        case Put(value) => s"put($value)"
        case Get => s"get() -> $output"
      }
    }
  }

  def spec = suite("PorcEpicTest")(
    test("model 1") {
      val ops = List(
        Operation(clientId = cid(0), input = Put(100), invocation = t(0), output = 0, response = t(1)),
        Operation(cid(0), )
      )
      assert(1)(equalTo(1))
    }
  )
}
