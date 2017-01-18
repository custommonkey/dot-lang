class Graph(statements: Seq[Statement]) {

  override def toString: String = {
    s"""graph {
       |${statements.mkString("\n")}
       |}""".stripMargin
  }

}

class DiGraph(statements: Seq[Statement]) {

  override def toString: String = {
    s"""digraph {
       |${statements.mkString("\n")}
       |}""".stripMargin
  }

}

trait Statement

class NodeStatement(id: Symbol, attrs: Seq[Attribute]) extends Statement {

  override def toString: String =
    id.name + (if (attrs.nonEmpty) attrs.mkString(" [", ", ", "]") else "") + ";"

}

class EdgeStatement(from: Symbol, to: Symbol, edge: String) extends Statement {
  override def toString: String = s"${from.name} $edge ${to.name};"
}

trait Attribute

class IntAttribute(id: Symbol, i: Int) extends Attribute {
  override def toString: String = s"""${id.name} = $i"""
}

class StringAttribute(id: Symbol, s: String) extends Attribute {
  override def toString: String = s"""${id.name} = "$s""""
}

trait DiGraphWords extends GraphWords {
  override val edge = "->"

  def digraph(statements: Statement*): DiGraph =
    new DiGraph(statements)

}

trait GraphWords {

  implicit class SymbOps(symbol: Symbol) {

    def :=(i: Int) = new IntAttribute(symbol, i)

    def :=(s: String) = new StringAttribute(symbol, s)

  }

  val edge = "--"

  def graph(statements: Statement*): Graph = {
    new Graph(statements)
  }

  def node(id: Symbol, attrs: Attribute*): NodeStatement =
    new NodeStatement(id, attrs)

  def edge(from: Symbol, to: Symbol): EdgeStatement =
    new EdgeStatement(from, to, edge)

}
