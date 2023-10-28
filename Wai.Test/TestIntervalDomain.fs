module TestIntervalDomain

open Wai.Domains.Interval.Types
open Wai.Domains.Interval.IntervalDomain
open Wai.AbstractState
open Wai.Evaluate
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestIntervalDomain() =

  [<TestMethod>]
  member _.TestBasicIf() =
    let input =
      """
      var x = 40;
      if (x >= 40) {
        x = 1;
      } else {
        x = 0;
      }
      """

    let program = evaluate input
    let interval_domain = IntervalDomain()
    let abstract_state = AbstractState<_>(interval_domain)
    let result, program_points = abstract_state.eval program

    let resultExpected = Map.empty.Add("x", (Range(Num 1, Num 1)))

    Assert.AreEqual(resultExpected, result)

  [<TestMethod>]
  member _.TestIfRange() =
    let input =
      """
      var x = [-40; 100];
      if (x >= 40) {
        skip;
      } else {
        skip;
      }
      """

    let program = evaluate input
    let interval_domain = IntervalDomain()
    let abstract_state = AbstractState<_>(interval_domain)
    let result, program_points = abstract_state.eval program

    let true_branch = Map.empty.Add("x", (Range(Num 40, Num 100)))
    let false_branch = Map.empty.Add("x", (Range(Num -40, Num 39)))

    Assert.AreEqual(true_branch, program_points.[2])
    Assert.AreEqual(false_branch, program_points.[4])

  [<TestMethod>]
  member _.TestIfAnd() =
    let input =
      """
      var x = [-40; 100];
      if (x >= 40 && x < 80) {
        skip;
      } else {
        skip;
      }
      """

    let program = evaluate input
    let interval_domain = IntervalDomain()
    let abstract_state = AbstractState<_>(interval_domain)
    let result, program_points = abstract_state.eval program

    let true_branch = Map.empty.Add("x", (Range(Num 40, Num 79)))
    let false_branch = Map.empty.Add("x", (Range(Num -40, Num 100)))

    Assert.AreEqual(true_branch, program_points.[2])
    Assert.AreEqual(false_branch, program_points.[4])

  [<TestMethod>]
  member _.TestWhileIncrement() =
    let input =
      """
      var x = 0;
      while (x < 40) {
        x = x + 1;
      }
      """

    let program = evaluate input
    let interval_domain = IntervalDomain()
    let abstract_state = AbstractState<_>(interval_domain)
    let result, program_points = abstract_state.eval program

    let pre_body = Map.empty.Add("x", (Range(Num 0, Num 39)))
    let post_body = Map.empty.Add("x", (Range(Num 1, Num 40)))
    let resultExpected = Map.empty.Add("x", (Range(Num 40, Num 40)))

    Assert.AreEqual(pre_body, program_points.[2])
    Assert.AreEqual(post_body, program_points.[3])
    Assert.AreEqual(resultExpected, result)

  [<TestMethod>]
  member _.TestWhileDecrement() =
    let input =
      """
      var x = 40;
      while (x != 0) {
        x = x - 1;
      }
      """

    let program = evaluate input
    let interval_domain = IntervalDomain()
    let abstract_state = AbstractState<_>(interval_domain)
    let result, program_points = abstract_state.eval program

    let pre_body = Map.empty.Add("x", (Range(Num 1, Num 40)))
    let post_body = Map.empty.Add("x", (Range(Num 0, Num 39)))
    let resultExpected = Map.empty.Add("x", (Range(Num 0, Num 0)))

    Assert.AreEqual(pre_body, program_points.[2])
    Assert.AreEqual(post_body, program_points.[3])
    Assert.AreEqual(resultExpected, result)

  [<TestMethod>]
  member _.TestWhileAnd() =
    let input =
      """
      var x = 0;
      var y = 0;
      while (x < 10 && y < 5) {
        x = x + 1;
        y = y + 1;
      }
      """

    let program = evaluate input
    let interval_domain = IntervalDomain()
    let abstract_state = AbstractState<_>(interval_domain)
    let result, program_points = abstract_state.eval program

    let pre_body =
      Map.empty
      |> Map.add "x" (Range(Num 0, Num 9))
      |> Map.add "y" (Range(Num 0, Num 4))

    let post_body =
      Map.empty
      |> Map.add "x" (Range(Num 1, Num 10))
      |> Map.add "y" (Range(Num 1, Num 5))

    let resultExpected =
      Map.empty
      |> Map.add "x" (Range(Num 0, Num 10))
      |> Map.add "y" (Range(Num 0, Num 5))

    Assert.AreEqual(pre_body, program_points.[3])
    Assert.AreEqual(post_body, program_points.[5])
    Assert.AreEqual(resultExpected, result)

  [<TestMethod>]
  member _.TestFactorial() =
    let input =
      """
      var n = 5;
      var tmp = n - 1;
      var factorial = n;
      while (tmp > 1) {
        factorial = factorial * tmp;
        tmp = tmp - 1;
      }
      """

    let program = evaluate input
    let interval_domain = IntervalDomain()
    let abstract_state = AbstractState<_>(interval_domain)
    let result, program_points = abstract_state.eval program

    let pre_body =
      Map.empty
      |> Map.add "factorial" (Range(Num 5, PlusInf))
      |> Map.add "n" (Range(Num 5, Num 5))
      |> Map.add "tmp" (Range(Num 2, Num 4))

    let post_body =
      Map.empty
      |> Map.add "factorial" (Range(Num 10, PlusInf))
      |> Map.add "n" (Range(Num 5, Num 5))
      |> Map.add "tmp" (Range(Num 1, Num 3))

    let resultExpected =
      Map.empty
      |> Map.add "factorial" (Range(Num 5, PlusInf))
      |> Map.add "n" (Range(Num 5, Num 5))
      |> Map.add "tmp" (Range(Num 0, Num 1))

    Assert.AreEqual(pre_body, program_points.[4])
    Assert.AreEqual(post_body, program_points.[6])
    Assert.AreEqual(resultExpected, result)

  [<TestMethod>]
  member _.TestFibonacci() =
    let input =
      """
      var n = 5;
      var pred1 = 1;
      var pred2 = 1;
      var i = 2;
      var fib = 1;
      if (n > 2) {
        while (i < n) {
          fib = pred1 + pred2;
          pred2 = pred1;
          pred1 = fib;
          i = i + 1;
        }
      } else {
        skip;
      }
      """

    let program = evaluate input
    let interval_domain = IntervalDomain()
    let abstract_state = AbstractState<_>(interval_domain)
    let result, program_points = abstract_state.eval program

    let pre_while_body =
      Map.empty
      |> Map.add "fib" (Range(Num 1, PlusInf))
      |> Map.add "i" (Range(Num 2, Num 4))
      |> Map.add "n" (Range(Num 5, Num 5))
      |> Map.add "pred1" (Range(Num 1, PlusInf))
      |> Map.add "pred2" (Range(Num 1, PlusInf))

    let post_while_body =
      Map.empty
      |> Map.add "fib" (Range(Num 2, PlusInf))
      |> Map.add "i" (Range(Num 3, Num 5))
      |> Map.add "n" (Range(Num 5, Num 5))
      |> Map.add "pred1" (Range(Num 2, PlusInf))
      |> Map.add "pred2" (Range(Num 1, PlusInf))

    let resultExpected =
      Map.empty
      |> Map.add "fib" (Range(Num 1, PlusInf))
      |> Map.add "i" (Range(Num 5, Num 5))
      |> Map.add "n" (Range(Num 5, Num 5))
      |> Map.add "pred1" (Range(Num 1, PlusInf))
      |> Map.add "pred2" (Range(Num 1, PlusInf))


    Assert.AreEqual(pre_while_body, program_points.[7])
    Assert.AreEqual(post_while_body, program_points.[11])
    Assert.AreEqual(resultExpected, result)
