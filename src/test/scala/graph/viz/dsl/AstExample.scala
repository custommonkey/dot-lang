package graph.viz.dsl

import better.files.File
import graph.viz.dsl.DiGraph._

import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.meta._

object AstExample extends App {

  val ast: Term = "x * y + z + a".parse[Term].get

  val nodes = mutable.Buffer[Node]()

  def edges(i: Int)(term: Tree): Seq[Edge] = {

    val name = term match {
      case Term.ApplyInfix(m) => {
        val n = "infix" + i
        nodes += node(n, label := "infix")
        n
      }
      case x => x.toString()
    }

    val a: Seq[Edge] = term.children.flatMap {
      case Term.Name(child) => {
        val c = child match {
          case "+" => {
            val n = "plus" + i
            nodes += node(n, label := "+")
            n
          }
          case "*" => {
            val n = "mult" + i
            nodes += node(n, label := "*")
            n
          }
          case s => s
        }
        Some(edge(name, c))
      }
      case Term.ApplyInfix(m) =>
        Some(edge(name, "infix" + (i + 1)))
    }

    val b: Seq[Edge] = term.children.flatMap(edges(i + 1))

    a ++ b

  }

  val e = edges(0)(ast)

  val a: String = digraph(
    Seq(node('node, shape := box)) ++ nodes ++ e).toString

  println(a)

  File("ast.gv") < a


}
