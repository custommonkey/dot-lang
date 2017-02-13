package graphviz.dsl

import graphviz.dsl.Container.Statements

object Container {

  type Statements = Iterable[Statement]

}

trait Container {

  def children: Statements

}

case class AttributeStatement(attr: Attribute[_]) extends Statement

protected abstract class AbstractGraph(statements: Statements)
  extends Container {
  val children: Statements = statements.flatMap {
    case StatementGroup(s) => s
    case s: Statement => Seq(s)
  }
}

class Inc extends (() => Int) {

  var c = 0

  override def apply(): Int = Function.const(c) {
    c = c + 1
  }

}

case class StatementGroup(statement: Statements) extends Statement

case class DiGraph(statements: Statements) extends AbstractGraph(statements)

case class Graph(statements: Statements) extends AbstractGraph(statements)

object DiGraph extends NodeWords with ClusterWords with GraphWords {

  val edgeSymbol = "->"

  def digraph(statements: Statements): DiGraph = new DiGraph(statements)

  def digraph(statements: Statement*): DiGraph = digraph(statements)

}

object Graph extends NodeWords with ClusterWords with GraphWords {

  val edgeSymbol = "--"

  def graph(statements: Statements): Graph = new Graph(statements)

  def graph(statements: Statement*): Graph = graph(statements)

}

trait ClusterWords {

  def cluster(statement: Statement, statements: Statement*)(implicit seq: () => Int) = Cluster(seq(), statement +: statements)

  def cluster(statements: Iterable[Statement])(implicit seq: () => Int) = Cluster(seq(), statements)

}

case class Cluster(i: Int, statements: Iterable[Statement])
  extends AbstractGraph(statements)
    with Statement

trait Statement

trait NodeWords {
  def node(id: Symbol, attrs: Attribute[_]*): Node =
    Node(id, attrs)

  def node(id: Symbol): Node = Node(id, Nil)

  def node(id: String, attrs: Attribute[_]*): Node = node(Symbol(id), attrs: _*)

  def edge(from: Symbol, to: Symbol): Edge = Edge(from, to)

  implicit def stringToSymbol(s: String): Symbol = Symbol(s)

  implicit def toNode(id: Symbol): Node = node(id)

  implicit def symbolToEdge(e: (Symbol, Symbol)): Edge = edge(e._1, e._2)

  implicit def stringToEdge(e: (String, String)): Edge = edge(Symbol(e._1), Symbol(e._2))

  implicit def toEdges(e: Iterable[(String, String)]): Iterable[Statement] = e.map(stringToEdge)

}

case class Node(id: Symbol, attrs: Seq[Attribute[_]]) extends Statement

case class Edge(from: Symbol, to: Symbol) extends Statement

case class Attribute[T](id: Symbol, value: T) extends Statement

trait Enom

trait Styles {

  sealed trait Style extends Enom

  case object solid extends Style

  case object dashed extends Style

  case object dotted extends Style

  case object bold extends Style

  case object rounded extends Style

  case object diagonals extends Style

  case object filled extends Style

  case object striped extends Style

  case object wedged extends Style

}

trait Shapes {

  sealed trait Shape extends Enom

  case object box extends Shape

  case object polygon extends Shape

  case object ellipse extends Shape

  case object oval extends Shape

  case object circle extends Shape

  case object point extends Shape

  case object egg extends Shape

  case object triangle extends Shape

  case object plaintext extends Shape

  case object plain extends Shape

  case object diamond extends Shape

  case object trapezium extends Shape

  case object parallelogram extends Shape

  case object house extends Shape

  case object pentagon extends Shape

  case object hexagon extends Shape

  case object septagon extends Shape

  case object octagon extends Shape

  case object doublecircle extends Shape

  case object doubleoctagon extends Shape

  case object tripleoctagon extends Shape

  case object invtriangle extends Shape

  case object invtrapezium extends Shape

  case object invhouse extends Shape

  case object Mdiamond extends Shape

  case object Msquare extends Shape

  case object Mcircle extends Shape

  case object rect extends Shape

  case object rectangle extends Shape

  case object square extends Shape

  case object star extends Shape

  case object none extends Shape

  case object underline extends Shape

  case object cylinder extends Shape

  case object note extends Shape

  case object tab extends Shape

  case object folder extends Shape

  case object box3d extends Shape

  case object component extends Shape

  case object promoter extends Shape

  case object cds extends Shape

  case object terminator extends Shape

  case object utr extends Shape

  case object primersite extends Shape

  case object restrictionsite extends Shape

  case object fivepoverhang extends Shape

  case object threepoverhang extends Shape

  case object noverhang extends Shape

  case object assembly extends Shape

  case object signature extends Shape

  case object insulator extends Shape

  case object ribosite extends Shape

  case object rnastab extends Shape

  case object proteasesite extends Shape

  case object proteinstab extends Shape

  case object rpromoter extends Shape

  case object rarrow extends Shape

  case object larrow extends Shape

  case object lpromoter extends Shape

}

class AnyRefAttributeBuilder(id: Symbol) {
  def :=(value: AnyRef): Attribute[AnyRef] = Attribute[AnyRef](id, value)
}

class EnumAttributeBuilder[T <: Enom](id: Symbol) {
  def :=(value: T): Attribute[T] = Attribute[T](id, value)
}

class SymbolAttributeBuilder(id: Symbol) {
  def :=(value: Symbol): Attribute[Symbol] = Attribute[Symbol](id, value)
}

class AnyValAttributeBuilder(id: Symbol) {
  def :=(value: AnyVal): Attribute[AnyVal] = Attribute[AnyVal](id, value)
}

trait AttributeNames extends Shapes with Styles {

  val style = new EnumAttributeBuilder[Style]('style)
  val shape = new EnumAttributeBuilder[Shape]('shape)
  val label = new AnyRefAttributeBuilder('label)
  val colour = new AnyRefAttributeBuilder('color)
  val fontcolour = new AnyRefAttributeBuilder('fontcolor)
  val bgcolour = new AnyRefAttributeBuilder('bgcolor)
  val fillcolour = new AnyRefAttributeBuilder('fillcolor)
  val layout = new AnyRefAttributeBuilder('layout)
  val fontsize = new AnyValAttributeBuilder('fontsize)

}

protected trait GraphWords extends AttributeNames {

  implicit val indentation: String = ""

  def edgeSymbol: String

  implicit class Ops(ref: Any) {
    def stringify(implicit indentation: String): String = apply(ref)
  }

  def apply[T](thing: Any)(implicit indentation: String): String = {

    def block(name: String, statements: Statements)(implicit indentation: String) =
      s"""$indentation$name {
         |${statements.stringify("  ")}
         |$indentation}""".stripMargin

    def attr(name: String, anyVal: String) = s"""$indentation$name = $anyVal"""

    thing match {
      case e: Seq[_] => e.map {
        case a: Cluster => a.stringify(indentation)
        case s => s.stringify(indentation) + ";"
      }.mkString("\n")
      case Attribute(Symbol(name), anyVal: Int) => attr(name, anyVal.toString)
      case Attribute(Symbol(name), enom: Enom) => attr(name, enom.toString)
      case Attribute(Symbol(name), anyRef: AnyRef) => attr(name, s""""$anyRef"""")
      case Attribute(Symbol(name), Symbol(symbol)) => attr(name, symbol)
      case Edge(Symbol(from), Symbol(to)) => s"$indentation$from $edgeSymbol $to"
      case Node(Symbol(name), attrs) => indentation + name + (if (attrs.nonEmpty) attrs.map(_.stringify).mkString(" [", ", ", "]") else "")
      case AttributeStatement(attr) => indentation + attr.stringify + ";"
      case DiGraph(statements) => block("digraph", statements)
      case Graph(statements) => block("graph", statements)
      case Cluster(i, statements) => block(s"subgraph cluster_$i", statements)
    }

  }

  implicit def toStatementGroup(statements: Seq[Statement]): StatementGroup = StatementGroup(statements)

  implicit def toStatementGroup(statements: Statements): StatementGroup = StatementGroup(statements)

  implicit def toStatement(attribute: Attribute[_]): AttributeStatement = AttributeStatement(attribute)

}
