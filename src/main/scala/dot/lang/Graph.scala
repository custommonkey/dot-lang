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
  protected val children: Statements = statements
}

object Graph extends Node with ClusterWords with GraphWords {

  val edgeSymbol = "--"

  def graph(statements: Iterable[Statement]): Graph = new Graph(statements)

  def graph(statements: Statement*): Graph = graph(statements)

}

class Graph(statements: Statements)
  extends AbstractGraph("graph", statements)

object DiGraph extends Node with ClusterWords with GraphWords {

  val edgeSymbol = "->"

  def digraph(statements: Iterable[Statement]): DiGraph = new DiGraph(statements)

  def digraph(statements: Statement*): DiGraph = digraph(statements)

}

class DiGraph(statements: Statements) extends AbstractGraph("digraph", statements)

trait ClusterWords {
  def cluster(statements: Statement*) = new Cluster(statements)
}

class Cluster(statements: Iterable[Statement])
  extends AbstractGraph("subgraph cluster_0", statements)
    with Statement

trait Statement

trait Node {
  def node(id: Symbol, attrs: Attribute*): NodeStatement =
    new NodeStatement(id, attrs)

  def node(id: Symbol): NodeStatement = new NodeStatement(id, Nil)

  def edge(from: Symbol, to: Symbol): EdgeStatement = new EdgeStatement(from, to, edgeSymbol)

  def edgeSymbol: String

  implicit def toNode(id: Symbol): NodeStatement = node(id)

  implicit def toEdge(e: (Symbol, Symbol)): EdgeStatement = edge(e._1, e._2)
}

class NodeStatement(id: Symbol, attrs: Seq[Attribute]) extends Statement {

  override def toString: String =
    id.name + (if (attrs.nonEmpty) attrs.mkString(" [", ", ", "]") else "") + ";"

}

class EdgeStatement(from: Symbol, to: Symbol, edge: String) extends Statement {
  override def toString: String = s"${from.name} $edge ${to.name};"
}

trait Attribute

class AnyRefAttributeBuilder(id: Symbol) {
  def :=(value: AnyRef) = new AnyRefAttribute(id, value)
}

class AnyValAttributeBuilder(id: Symbol) {
  def :=(value: AnyVal) = new AnyValAttribute(id, value)
}

class AnyValAttribute(id: Symbol, anyVal: AnyVal) extends Attribute {
  override def toString: String = s"""${id.name} = $anyVal"""
}

class AnyRefAttribute(id: Symbol, anyRef: AnyRef) extends Attribute {
  override def toString: String = s"""${id.name} = "$anyRef""""
}

protected trait GraphWords {

  val label = new AnyRefAttributeBuilder('label)
  val colour = new AnyRefAttributeBuilder('color)
  val bgcolour = new AnyRefAttributeBuilder('bgcolor)
  val layout = new AnyRefAttributeBuilder('layout)
  val fontsize = new AnyValAttributeBuilder('fontsize)

  implicit def toStatement(attribute: Attribute): AttributeStatement =
    new AttributeStatement(attribute)

}
