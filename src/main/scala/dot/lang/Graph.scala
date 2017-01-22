package dot.lang

import dot.lang.Container.Statements

object Container {

  type Statements = Iterable[Statement]

}

abstract class Container(id: String) {

  protected def children: Statements

  override def toString: String = {
    val childString = if (children.nonEmpty) {
      children.mkString("\n  ", "\n  ", "\n")
    } else {
      ""
    }
    s"$id {$childString}"
  }

}

class AttributeStatement(attr: Attribute) extends Statement {
  override def toString: String = attr.toString + ";"
}

protected abstract class AbstractGraph(id: String,
  statements: Statements)
  extends Container(id) {
  protected val children: Statements = statements.flatMap {
    case StatementGroup(s) => s
    case s: Statement => Seq(s)
  }
}


class Inc extends Function0[Int] {

  var c = 0

  override def apply(): Int = Function.const(c) {
    c = c + 1
  }

}


object Graph extends NodeWords with ClusterWords with GraphWords {

  val edgeSymbol = "--"

  def graph(statements: Statements): Graph = new Graph(statements)

  def graph(statements: Statement*): Graph = graph(statements)

}

case class StatementGroup(statement: Statements) extends Statement

class Graph(statements: Statements)
  extends AbstractGraph("graph", statements)

object DiGraph extends NodeWords with ClusterWords with GraphWords {

  val edgeSymbol = "->"

  def digraph(statements: Statements)(implicit dummyImplicit: DummyImplicit): DiGraph = new DiGraph(statements)

  def digraph(statements: Statement*): DiGraph = digraph(statements)

}

class DiGraph(statements: Statements) extends AbstractGraph("digraph", statements)

trait ClusterWords {
  //def cluster(i: Int)(statements: Statement*)(implicit dummyImplicit: DummyImplicit) = new Cluster(i, statements)

  def cluster(statements: Iterable[Statement])(implicit seq: () => Int) = new Cluster(seq(), statements)

}

class Cluster(i: Int, statements: Iterable[Statement])
  extends AbstractGraph(s"subgraph cluster_$i", statements)
    with Statement

trait Statement

trait NodeWords {
  def node(id: Symbol, attrs: Attribute*): Node =
    new Node(id, attrs)

  def node(id: Symbol): Node = new Node(id, Nil)

  def node(id: String, attrs: Attribute*): Node = node(Symbol(id), attrs: _*)

  def edge(from: Symbol, to: Symbol): Edge = Edge(from, to, edgeSymbol)

  implicit def stringToSymbol(s: String) = Symbol(s)

  def edgeSymbol: String

  implicit def toNode(id: Symbol): Node = node(id)

  implicit def symbolToEdge(e: (Symbol, Symbol)): Edge = edge(e._1, e._2)

  implicit def stringToEdge(e: (String, String)): Edge = edge(Symbol(e._1), Symbol(e._2))

  implicit def toEdges(e: Iterable[(String, String)]): Iterable[Statement] = e.map(stringToEdge)

}

case class Node(id: Symbol, attrs: Seq[Attribute]) extends Statement {

  override def toString: String =
    id.name + (if (attrs.nonEmpty) attrs.mkString(" [", ", ", "]") else "") + ";"

}

case class Edge(from: Symbol, to: Symbol, edge: String) extends Statement {
  override def toString: String = s"${from.name} $edge ${to.name};"
}

trait Attribute

trait Styles {

  sealed trait Style

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

  sealed trait Shape

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
  def :=(value: AnyRef) = new AnyRefAttribute(id, value)
}

class EnumAttributeBuilder[T](id: Symbol) {
  def :=(value: T) = new EnumAttribute[T](id, value)
}

class SymbolAttributeBuilder(id: Symbol) {
  def :=(value: Symbol) = new SymbolAttribute(id, value)
}

class AnyValAttributeBuilder(id: Symbol) {
  def :=(value: AnyVal) = new AnyValAttribute(id, value)
}

class AnyValAttribute(id: Symbol, anyVal: AnyVal) extends Attribute {
  override def toString: String = s"""${id.name} = $anyVal"""
}

class SymbolAttribute(id: Symbol, symbol: Symbol) extends Attribute {
  override def toString: String = s"""${id.name} = ${symbol.name}"""
}

class EnumAttribute[T](id: Symbol, enum: T) extends Attribute {
  override def toString: String = s"""${id.name} = $enum"""
}

class AnyRefAttribute(id: Symbol, anyRef: AnyRef) extends Attribute {
  override def toString: String = s"""${id.name} = "$anyRef""""
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

  implicit def toStatementGroup(statements: Seq[Statement]): StatementGroup = StatementGroup(statements)

  implicit def toStatementGroup(statements: Statements): StatementGroup = StatementGroup(statements)


  implicit def toStatement(attribute: Attribute): AttributeStatement =
    new AttributeStatement(attribute)

}
