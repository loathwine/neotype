package comptime

import zio.test.*

object VarSpec extends ZIOSpecDefault:
  val spec =
    suite("VarSpec (comptime)")(
      suite("basic var")(
        test("var declaration and read") {
          assertTrue(
            comptime {
              var x = 1
              x = x
              x
            } == 1
          )
        },
        test("var assignment") {
          assertTrue(
            comptime {
              var x = 1
              x = 2
              x
            } == 2
          )
        },
        test("multiple assignments") {
          assertTrue(
            comptime {
              var x = 1
              x = 2
              x = 3
              x
            } == 3
          )
        },
        test("var with string") {
          assertTrue(
            comptime {
              var s = "hello"
              s = "world"
              s
            } == "world"
          )
        }
      ),
      suite("var with expressions")(
        test("increment") {
          assertTrue(
            comptime {
              var x = 1
              x = x + 1
              x
            } == 2
          )
        },
        test("compound operations") {
          assertTrue(
            comptime {
              var x = 10
              x = x * 2
              x = x - 5
              x
            } == 15
          )
        }
      ),
      // Note: foreach is not supported in comptime, so loop-based accumulation
      // patterns would need to use foldLeft or similar instead
      suite("multiple vars")(
        test("two vars") {
          assertTrue(
            comptime {
              var x = 1
              var y = 2
              y = y
              x = x + y
              x
            } == 3
          )
        },
        test("swap values") {
          assertTrue(
            comptime {
              var a   = 1
              var b   = 2
              val tmp = a
              a = b
              b = tmp
              (a, b)
            } == (2, 1)
          )
        }
      ),
      suite("var with conditionals")(
        test("conditional assignment") {
          assertTrue(
            comptime {
              var x = 0
              if true then x = 1 else x = 2
              x
            } == 1
          )
        },
        test("conditional with false") {
          assertTrue(
            comptime {
              var x = 0
              if false then x = 1 else x = 2
              x
            } == 2
          )
        }
      ),
      suite("var with closures")(
        test("closure captures var reference") {
          assertTrue(
            comptime {
              var x = 1
              val f = () => x
              x = 2
              f()
            } == 2
          )
        },
        test("closure mutates var") {
          assertTrue(
            comptime {
              var x = 0
              val f = () =>
                x = x + 1; x
              val a = f()
              val b = f()
              (a, b, x)
            } == (1, 2, 2)
          )
        },
        test("var mutation in map") {
          assertTrue(
            comptime {
              var count = 0
              val result = List(1, 2, 3).map { i =>
                count = count + 1
                i * count
              }
              (result, count)
            } == (List(1, 4, 9), 3)
          )
        }
      ),
      suite("var with match")(
        test("match scrutinee sees latest var value (#473)") {
          assertTrue(
            comptime {
              var x = 1
              x = 2
              x match
                case 1 => "one"
                case 2 => "two"
                case _ => "other"
            } == "two"
          )
        },
        test("match scrutinee with computed expression on var") {
          assertTrue(
            comptime {
              var x = 1
              x = 3
              (x * 2) match
                case 2 => "two"
                case 6 => "six"
                case _ => "other"
            } == "six"
          )
        },
        test("match on var in case body referencing var") {
          assertTrue(
            comptime {
              var x = 10
              x = 20
              x match
                case n if n > 15 => n + 1
                case _           => 0
            } == 21
          )
        }
      ),
      suite("var with try/catch")(
        test("try expression sees latest var value") {
          assertTrue(
            comptime {
              var x = 0
              x = 42
              try x
              finally ()
            } == 42
          )
        },
        test("try-catch body reads latest var") {
          assertTrue(
            comptime {
              var x = 0
              x = 10
              try x
              catch case _: Throwable => -1
            } == 10
          )
        },
        test("finally writes to var observed after try") {
          assertTrue(
            comptime {
              var seen = 0
              try
                val y = 1
                y
              finally seen = 100
              seen
            } == 100
          )
        },
        test("var-dependent failure caught by try") {
          assertTrue(
            comptime {
              var xs: List[Int] = List(1, 2, 3)
              xs = Nil
              try xs.head
              catch case _: Throwable => -1
            } == -1
          )
        }
      ),
      suite("var in nested blocks")(
        test("inner block sees outer var") {
          assertTrue(
            comptime {
              var x = 1
              val y =
                x = x + 10
                x
              (x, y)
            } == (11, 11)
          )
        },
        test("shadowing var with val") {
          assertTrue(
            comptime {
              var x = 1
              x = x
              val result =
                val x = 100 // shadows the var
                x
              (x, result)
            } == (1, 100)
          )
        }
      )
    )
