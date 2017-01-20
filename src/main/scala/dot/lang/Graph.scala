package dot.lang

class Graph(statements: Seq[Statement]) {

  override def toString: String = {
    s"""graph {
       |${statements.mkString("\n")}
       |}""".stripMargin
  }

}

class DiGraph(attrs: Iterable[Attribute], statements: Iterable[Statement]) {

  override def toString: String = {
    s"""digraph {
       |  ${if(attrs.nonEmpty) attrs.mkString("", ";\n  ", ";") else ""}
       |  ${statements.mkString("\n  ")}
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

  def digraph(attr: Attribute*)(statements: Statement*): DiGraph =
    new DiGraph(attr, statements)

  def digraph(statements: Statement*): DiGraph =
    new DiGraph(Nil, statements)

  def digraph(attr: Iterable[Attribute], statements: Iterable[Statement]): DiGraph =
    new DiGraph(attr, statements)

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

  def node(id: Symbol): NodeStatement = new NodeStatement(id, Nil)

  def edge(from: Symbol, to: Symbol): EdgeStatement =
    new EdgeStatement(from, to, edge)

}
