package pm

object PM extends App{

  trait Expr
  case class Number(n: Int) extends Expr
  case class Sum(e1: Expr, e2: Expr) extends Expr
  case class Prod(e1: Expr, e2: Expr) extends Expr

  def show(expr: Expr): String = {
    expr match {
      case Number(n) => n.toString
      case Sum(e1, e2) => s"${show(e1)} + ${show(e2)}"
      case Prod(e1, e2) => {
        def maybeShowParentheses(expr: Expr) = expr match {
          case Number(n) => n.toString
          case Prod(_, _) => show(expr)
          case _ => s"(${show(expr)})"
        }
        s"${maybeShowParentheses(e1)} * ${maybeShowParentheses(e2)}"
      }
      case _ => ""
    }
  }
}

