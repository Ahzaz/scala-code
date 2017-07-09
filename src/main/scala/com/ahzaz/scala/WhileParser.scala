/*
https://www.hackerrank.com/challenges/while-language-fp
Parser for while language
*
* */
package com.ahzaz.scala

import java.util.Scanner

import com.ahzaz.scala.WhileParser.State

import scala.collection.mutable
import scala.util.parsing.combinator.JavaTokenParsers

trait Statement {
  def execute()(implicit state: State): Unit
}

object Statement {
  def apply(variable: String, value: ArithmeticExpression): Statement = Assignment(variable, value)

  def apply(condition: BooleanExpression, thenPart: Statement, elsePart: Statement): Statement = IfStatement(condition, thenPart, elsePart)

  def apply(condition: BooleanExpression, statements: Statement): Statement = WhileStatement(condition, statements)

  def apply(statement1: Statement, statement2: Statement): Statement = Statements(statement1, statement2)
}

case class Statements(statement1: Statement, statement2: Statement) extends Statement {
  override def execute()(implicit state: State): Unit = {
    statement1.execute()
    statement2.execute()
  }
}

case class Assignment(variable: String, value: ArithmeticExpression) extends Statement {
  override def execute()(implicit state: State): Unit = state += (variable -> value.value)
}

case class IfStatement(condition: BooleanExpression, thenPart: Statement, elsePart: Statement) extends Statement {
  override def execute()(implicit state: State): Unit = if (condition.value) thenPart.execute() else elsePart.execute()
}

case class WhileStatement(condition: BooleanExpression, statements: Statement) extends Statement {
  override def execute()(implicit state: State): Unit = {
    if (condition.value) {
      statements.execute()
      execute()
    }
  }
}

trait BooleanExpression {
  def value(implicit state: State): Boolean
}

object BooleanExpression {
  def apply(value: Boolean): BooleanExpression = if (value) True() else False()

  def apply(op1: BooleanExpression, op2: BooleanExpression, operator: String): BooleanExpression = BExp(op1, op2, operator)

  def apply(op1: ArithmeticExpression, op2: ArithmeticExpression, operator: String): BooleanExpression = RExp(op1, op2, operator)
}

case class True() extends BooleanExpression {
  override def value(implicit state: State): Boolean = true
}

case class False() extends BooleanExpression {
  override def value(implicit state: State): Boolean = false
}

case class BExp(op1: BooleanExpression, op2: BooleanExpression, operator: String) extends BooleanExpression {
  override def value(implicit state: State): Boolean = operator match {
    case "and" => op1.value && op2.value
    case "or" => op1.value && op2.value
  }
}

case class RExp(op1: ArithmeticExpression, op2: ArithmeticExpression, operator: String) extends BooleanExpression {
  override def value(implicit state: State): Boolean = operator match {
    case "<" => op1.value < op2.value
    case ">" => op1.value > op2.value
  }
}


trait ArithmeticExpression {
  def value(implicit state: State): Long
}

object ArithmeticExpression {
  def apply(name: String): ArithmeticExpression = Variable(name)

  def apply(n: Long): ArithmeticExpression = Number(n)

  def apply(op1: ArithmeticExpression, op2: ArithmeticExpression, operator: String): ArithmeticExpression = AExp(op1, op2, operator)
}

case class Variable(name: String) extends ArithmeticExpression {
  override def value(implicit state: State): Long = state(name)
}

case class Number(n: Long) extends ArithmeticExpression {
  override def value(implicit state: State): Long = n
}

case class AExp(op1: ArithmeticExpression, op2: ArithmeticExpression, operator: String) extends ArithmeticExpression {
  override def value(implicit state: State): Long = operator match {
    case "+" => op1.value + op2.value
    case "-" => op1.value - op2.value
    case "*" => op1.value * op2.value
    case "/" => op1.value / op2.value
  }
}

class ProgramParser(program: String) extends JavaTokenParsers {

  def parens[T](parser: Parser[T]): Parser[T] = "(" ~> parser <~ ")"

  def compound[T](parser: Parser[T]): Parser[T] = "{" ~> parser <~ "}"

  def aExp: Parser[ArithmeticExpression] = {
    def aExp0: Parser[ArithmeticExpression] = {
      val variable = ident ^^ { x => ArithmeticExpression(x) }
      val number = floatingPointNumber ^^ { n => ArithmeticExpression(n.toLong) }
      variable | number | parens(aExp)
    }

    def aExp1: Parser[ArithmeticExpression] = {
      lazy val exp = aExp0 ~ rep("""[*/]""".r ~ aExp0) ^^ {
        case op1 ~ rest => rest.foldLeft(op1) {
          case (op1, op ~ op2) => ArithmeticExpression(op1, op2, op)
        }
      }
      exp
    }

    def aExp2: Parser[ArithmeticExpression] = {
      lazy val exp = aExp1 ~ rep("""[+-]""".r ~ aExp1) ^^ {
        case op1 ~ rest => rest.foldLeft(op1) {
          case (op1, op ~ op2) => ArithmeticExpression(op1, op2, op)
        }
      }
      exp
    }

    aExp2
  }

  def bExp: Parser[BooleanExpression] = {
    def trueExp = literal("true") ^^^ {

      BooleanExpression(true)
    }

    def falseExp = literal("false") ^^^ {
      BooleanExpression(false)
    }

    def rexp = aExp ~ """[<>]""".r ~ aExp ^^ {
      case op1 ~ op ~ op2 => BooleanExpression(op1, op2, op)
    }

    def bexp0: Parser[BooleanExpression] = {
      trueExp | falseExp | rexp
    }

    def bexp = bexp0 ~ rep("""(and)|(or)""".r ~ bexp0) ^^ {
      case op1 ~ rest => rest.foldLeft(op1) {
        case (op1, op ~ op2) => BooleanExpression(op1, op2, op)
      }
    }

    parens(bexp) | bexp
  }

  def parseStatement: Parser[Statement] = {
    val assignment = (ident <~ ":=") ~ aExp ^^ {
      case variable ~ value => Assignment(variable, value)
    }
    val ifStatement = ("if" ~> bExp) ~ ("then" ~> compound(parseStatement)) ~ ("else" ~> compound(parseStatement)) ^^ {
      case condition ~ thenPart ~ elsePart => Statement(condition, thenPart, elsePart)
    }
    val whileStatement = ("while" ~> bExp) ~ ("do" ~> compound(parseStatement)) ^^ {
      case condition ~ statement => Statement(condition, statement)
    }
    (ifStatement | whileStatement | assignment) ~ ((";" ~> parseStatement) ?) ^^ {
      case s1 ~ s2 => if (s2.nonEmpty) Statement(s1, s2.get) else s1
    }
  }

  def parseProgram: Statement = {
    val result = parse(parseStatement, program)
    result match {
      case Success(statement, _) => println(statement); statement
      case Failure(msg, _) => println("FAILURE: " + msg); throw new RuntimeException
      case Error(msg, _) => println("ERROR: " + msg); throw new RuntimeException
    }
  }
}

object WhileParser {
  type State = mutable.Map[String, Long]

  def main(args: Array[String]): Unit = {
    val in = new Scanner(System.in)
    val inputBuilder = new mutable.StringBuilder()
    while (in.hasNext) {
      inputBuilder.++=(in.nextLine() + "\n")
    }
    val program = inputBuilder.toString()
//    val program =
//      """
//        |fact := 1 ;
//        |val := 10000 ;
//        |cur := val ;
//        |mod := 1000000007 ;
//        |
//        |while ( cur > 1 )
//        |do
//        |{
//        |   fact := fact * cur ;
//        |   fact := fact - fact / mod * mod ;
//        |   cur := cur - 1
//        |} ;
//        |
//        |cur := 0
//        |""".stripMargin
    implicit val state: State = mutable.Map.empty[String, Long]
    new ProgramParser(program).parseProgram.execute()
    state.toList.sortBy(_._1).foreach(entry => println(s"${entry._1} ${entry._2}"))
  }

}