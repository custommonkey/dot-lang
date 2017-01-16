
class Graph(statements: Seq[Statement]) {

  override def toString: String = {
    s"""digraph {
       |${statements.mkString("\n")}
       |}""".stripMargin
  }

}

trait Statement

class NodeStatement(id: Symbol, attrs: Seq[Attribute]) extends Statement {

  override def toString: String = id.name + (if (attrs.nonEmpty) attrs.mkString(" [", ", ", "]") else "") + ";"

}

class EdgeStatement(from: Symbol, to: Symbol) extends Statement {
  override def toString: String = s"${from.name} -> ${to.name};"
}

trait Attribute

class IntAttribute(id: Symbol, i: Int) extends Attribute {
  override def toString: String = s"""${id.name} = $i"""
}

class StringAttribute(id: Symbol, s: String) extends Attribute {
  override def toString: String = s"""${id.name} = "$s""""
}

trait GraphWords {

  implicit class SymbOps(symbol: Symbol) {

    def :=(i: Int) = new IntAttribute(symbol, i)

    def :=(s: String) = new StringAttribute(symbol, s)

  }

  def graph(statements: Statement*): Graph = new Graph(statements)

  def node(id: Symbol, attrs: Attribute*): NodeStatement = new NodeStatement(id, attrs)

  def edge(from: Symbol, to: Symbol): EdgeStatement = new EdgeStatement(from, to)

}


