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

/**
  * Program is sequence of statements.
  */
trait Statement {
  def execute()(implicit state: State): Unit
}

object Statement {
  def apply(variable: String, value: ArithmeticExpression): Statement = Assignment(variable, value)

  def apply(condition: BooleanExpression, thenPart: Statement, elsePart: Statement): Statement = IfStatement(condition, thenPart, elsePart)

  def apply(condition: BooleanExpression, statements: Statement): Statement = WhileStatement(condition, statements)

  def apply(statement1: Statement, statement2: Statement): Statement = Statements(statement1, statement2)
}

/**
  * Combines two statements.
  *
  */
case class Statements(statement1: Statement, statement2: Statement) extends Statement {
  override def execute()(implicit state: State): Unit = {
    statement1.execute()
    statement2.execute()
  }
}

/**
  * Represents assignment statement in the program.
  * x := y
  * @param variable Variable to which value is to be assignment
  * @param value Expression which is to be assigned
  */
case class Assignment(variable: String, value: ArithmeticExpression) extends Statement {
  override def execute()(implicit state: State): Unit = state += (variable -> value.value)
}

/**
  * Represents `if then else` statement
  * @param condition Boolean expression for if statement
  * @param thenPart Statement to be evaluated if condition is true
  * @param elsePart Statement to be evaluated if condition is false
  */
case class IfStatement(condition: BooleanExpression, thenPart: Statement, elsePart: Statement) extends Statement {
  override def execute()(implicit state: State): Unit = if (condition.value) thenPart.execute() else elsePart.execute()
}

/**
  * Represents `while do` statement
  * @param condition Boolean expression for while
  * @param statements statements to be executed till condition is true
  */
case class WhileStatement(condition: BooleanExpression, statements: Statement) extends Statement {
  override def execute()(implicit state: State): Unit = {
    while (condition.value)
      statements.execute()
  }
}

/**
  * Represents a boolean expression. Evaluating it results is a boolean value
  */
trait BooleanExpression {
  def value(implicit state: State): Boolean
}

object BooleanExpression {
  def apply(value: Boolean): BooleanExpression = if (value) True() else False()

  def apply(op1: BooleanExpression, op2: BooleanExpression, operator: String): BooleanExpression = BExp(op1, op2, operator)

  def apply(op1: ArithmeticExpression, op2: ArithmeticExpression, operator: String): BooleanExpression = RExp(op1, op2, operator)
}

/**
  * Represent `True` value
  */
case class True() extends BooleanExpression {
  override def value(implicit state: State): Boolean = true
}

/**
  * Represent `False` value
  */
case class False() extends BooleanExpression {
  override def value(implicit state: State): Boolean = false
}

/**
  * Represents `and` or `or` of two boolean expressions
  * @param op1 First Operand
  * @param op2 Second Operand
  * @param operator Either string `and` or `or`
  */
case class BExp(op1: BooleanExpression, op2: BooleanExpression, operator: String) extends BooleanExpression {
  override def value(implicit state: State): Boolean = operator match {
    case "and" => op1.value && op2.value
    case "or" => op1.value || op2.value
  }
}

/**
  * Represents relational expression. Comparision of two numerical values
  * @param op1 First Expression
  * @param op2 Second Expression
  * @param operator Either string `<` or `>`
  */
case class RExp(op1: ArithmeticExpression, op2: ArithmeticExpression, operator: String) extends BooleanExpression {
  override def value(implicit state: State): Boolean = operator match {
    case "<" => op1.value < op2.value
    case ">" => op1.value > op2.value
  }
}

/**
  * Base trait for arithmetic expressions.
  */
trait ArithmeticExpression {
  def value(implicit state: State): Long
}

object ArithmeticExpression {
  def apply(name: String): ArithmeticExpression = Variable(name)

  def apply(n: Long): ArithmeticExpression = Number(n)

  def apply(op1: ArithmeticExpression, op2: ArithmeticExpression, operator: String): ArithmeticExpression = AExp(op1, op2, operator)
}

/**
  * Represents a variable in the program. Value of expression is value of variable at given program point.
  * @param name Name of the variable
  */
case class Variable(name: String) extends ArithmeticExpression {
  override def value(implicit state: State): Long = state(name)
}

/**
  * Numerical literal in the program
  * @param n Value of number
  */
case class Number(n: Long) extends ArithmeticExpression {
  override def value(implicit state: State): Long = n
}

/**
  * Represents arithmetic operation on two values
  * @param op1 First Operand
  * @param op2 Second Operand
  * @param operator Operator as string, must be one of +, -, *, /
  */
case class AExp(op1: ArithmeticExpression, op2: ArithmeticExpression, operator: String) extends ArithmeticExpression {
  override def value(implicit state: State): Long = operator match {
    case "+" => op1.value + op2.value
    case "-" => op1.value - op2.value
    case "*" => op1.value * op2.value
    case "/" => op1.value / op2.value
  }
}

/**
  * Parser for the while language
  * @param program Program to be parsed
  */
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
        case first ~ rest => rest.foldLeft(first) {
          case (op1, op ~ op2) => ArithmeticExpression(op1, op2, op)
        }
      }
      exp
    }

    def aExp2: Parser[ArithmeticExpression] = {
      lazy val exp = aExp1 ~ rep("""[+-]""".r ~ aExp1) ^^ {
        case first ~ rest => rest.foldLeft(first) {
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
      case first ~ rest => rest.foldLeft(first) {
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
      case s1 ~ s2 => s2.fold(s1)(s2 => Statement(s1, s2))
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
  /**
    * Represents the state of the program as map from variable to its value
    */
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