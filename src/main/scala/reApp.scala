package com.ledgerleopard.re

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.parsing.combinator._

object reApp extends App {

  abstract class RegexExpr
  case class Literal(c: Char) extends RegexExpr
  // a|b
  case class Or(expr1: RegexExpr, expr2: RegexExpr) extends RegexExpr
  // ab -> Concat(a,b); abc -> Concat(a, Concat(b, c))
  case class Concat(first: RegexExpr, second: RegexExpr) extends RegexExpr
  // a*
  case class Repeat(expr: RegexExpr) extends RegexExpr
  // a+
  case class Plus(expr: RegexExpr) extends RegexExpr

  object RegexParser extends RegexParsers {
    def charLit: Parser[RegexExpr] = ("""\w""".r | ".") ^^ {
      char => Literal(char.head)
    }

    def parenExpr: Parser[RegexExpr] = "(" ~> highExpr <~ ")"
    def highExpr: Parser[RegexExpr] = or | midExpr
    def or: Parser[RegexExpr] = midExpr ~ "|" ~ midExpr ^^ { case l ~ "|" ~ r => Or(l, r) }
    def concat: Parser[RegexExpr] = rep(lowExpr) ^^ (list => listToConcat(list))
    def midExpr: Parser[RegexExpr] = concat | lowExpr
    def lit: Parser[RegexExpr] = charLit | parenExpr
    def repeat: Parser[RegexExpr] = lit <~ "*" ^^ (l => Repeat(l))
    def plus: Parser[RegexExpr] = lit <~ "+" ^^ (p => Plus(p))
    def lowExpr: Parser[RegexExpr] = repeat | plus | lit
    def listToConcat(list: List[RegexExpr]): RegexExpr = list match {
      case head :: Nil => head
      case head :: rest => Concat(head, listToConcat(rest))
    }

    def apply(input: String): Option[RegexExpr] = {
      parseAll(highExpr, input) match {
        case Success(result, _) => Some(result)
        case failure : NoSuccess => None
      }
    }
  }

  abstract class State

  case class Consume(c: Char, out: State) extends State
  case class Split(out1: State, out2: State) extends State
  case class Match() extends State
  class Placeholder(var pointingTo: State) extends State

  object NFA {
    def regexToNFA(regex: RegexExpr): State =
      regexToNFA(regex, Match())

    private def regexToNFA(regex: RegexExpr,
                           andThen: State): State = {
      regex match {
        case Literal(c) =>
          Consume(c, andThen)
        case Concat(first, second) =>
          regexToNFA(first, regexToNFA(second, andThen))
        case Or(l, r) => Split(
          regexToNFA(l, andThen),
          regexToNFA(r, andThen)
        )

        case Repeat(r) =>
          val placeholder = new Placeholder(null)
          val split = Split(
            regexToNFA(r, placeholder),
            andThen
          )
          placeholder.pointingTo = split
          placeholder

        case Plus(r) =>
          regexToNFA(Concat(r, Repeat(r)), andThen)
      }
    }
  }

  object NFAEvaluator {
    def evaluate(nfa: State, input: String): Boolean =
      evaluate(Set(nfa), input)

    @tailrec
    def evaluate(nfas: Set[State], input: String): Boolean = {
      input match {
        case "" =>
          evaluateStates(nfas, None).contains(Match())
        case string =>
          evaluate(
            evaluateStates(nfas, input.headOption),
            string.tail
          )
      }
    }

    def evaluateStates(nfas: Set[State],
                       input: Option[Char]): Set[State] =
      nfas.flatMap(evaluateState(_, input, mutable.Set[State]()))

    def evaluateState(currentState: State, input: Option[Char],
                      visitedStates: mutable.Set[State]): Set[State] = {

      if (visitedStates contains currentState) {
        Set()
      } else {
        visitedStates.add(currentState)
        currentState match {

          case placeholder: Placeholder =>
            evaluateState(
              placeholder.pointingTo,
              input,
              visitedStates
            )

          case consume: Consume =>
            if (input.contains(consume.c) || consume.c == '.') {
              Set(consume.out)
            } else {
              Set()
            }

          case s: Split =>
            evaluateState(s.out1, input, visitedStates) ++
              evaluateState(s.out2, input, visitedStates)

          case _: Match =>
            if (input.isDefined) Set() else Set(Match())
        }
      }
    }
  }

  println(
    RegexParser("ab+cb")
      .map(NFA.regexToNFA)
      .map(NFAEvaluator.evaluate(_, "abbb"))
  )
}
