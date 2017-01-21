package dot.lang

import better.files.File
import dot.lang.DiGraph._
import org.scalatest.FlatSpec

import scala.meta._

class AstSpec extends FlatSpec {

  behavior of "graph"

  it should "represent a basic scala ast" in {

    val ast: Term = "x + y".parse[Term].get

    def tree(term: Term) = {
      val name = term match {
        case Term.ApplyInfix(m) => "infix"
      }

      term.children.map {
        case Term.Name(child) => name -> child
      }
    }

    val a = digraph(tree(ast))

    println(a)

    File("ast.gv") < a.toString

    //val b = digraph(
    //'infix -> ('x, '+, 'y)
    //)

    //a shouldBe b

  }


}
