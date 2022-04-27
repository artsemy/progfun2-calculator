package calculator

enum Expr:
  case Literal(v: Double)
  case Ref(name: String)
  case Plus(a: Expr, b: Expr)
  case Minus(a: Expr, b: Expr)
  case Times(a: Expr, b: Expr)
  case Divide(a: Expr, b: Expr)

object Calculator extends CalculatorInterface :

  import Expr.*

  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] =
    namedExpressions.map {
      case (key, sig) => (key, Signal(eval(sig(), namedExpressions)))
    }

  def eval(expr: Expr, references: Map[String, Signal[Expr]])(using Signal.Caller): Double =
    expr match {
      case Expr.Literal(v) => v
      case Expr.Ref(name) =>
        if references.contains(name) then
          eval(references(name)(), references.removed(name))
        else
          Double.NaN
      case Expr.Plus(a, b) => eval(a, references) + eval(b, references)
      case Expr.Minus(a, b) => eval(a, references) - eval(b, references)
      case Expr.Times(a, b) => eval(a, references) * eval(b, references)
      case Expr.Divide(a, b) => eval(a, references) / eval(b, references)
    }

  /** Get the Expr for a referenced variables.
   * If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
                               references: Map[String, Signal[Expr]])(using Signal.Caller): Expr =
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
