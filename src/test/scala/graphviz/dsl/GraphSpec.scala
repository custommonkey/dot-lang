package graphviz.dsl

import org.scalatest.{FlatSpec, Matchers}

class GraphSpec extends FlatSpec with Matchers {

  behavior of "A Directed Graph"

  it should "generate a digraph string" in {
    import DiGraph._

    digraph(
      node('blah),
      edge('from, 'to)
    ).stringify shouldBe
      """digraph {
        |  blah;
        |  from -> to;
        |}""".stripMargin

  }

  it should "generate a digraph from a Seq" in {
    import DiGraph._

    val seq = Seq(node('blah), edge('from, 'to))

    digraph(seq).stringify shouldBe
      """digraph {
        |  blah;
        |  from -> to;
        |}""".stripMargin

  }

  it should "generate a graph from a Seq" in {
    import Graph._

    val seq = Seq(node('blah), edge('from, 'to))

    graph(seq).stringify shouldBe
      """graph {
        |  blah;
        |  from -- to;
        |}""".stripMargin

  }

  it should "generate attributes" in {

    import DiGraph._

    digraph(label := "two").stringify shouldBe
      """digraph {
        |  label = "two";
        |}""".stripMargin
  }

  it should "mix attributes and statements" in {

    import DiGraph._

    digraph(layout := "two", node('anode)).stringify shouldBe
      """digraph {
        |  layout = "two";
        |  anode;
        |}""".stripMargin
  }

  behavior of "A Graph"

  it should "generate a graph string" in {

    import Graph._

    implicit val inc = new Inc

    val g = graph(
      'blah,
      'from -> 'to,
      cluster(Nil)
    )

    g.stringify shouldBe
      """graph {
        |  blah;
        |  from -- to;
        |  subgraph cluster_0 {
        |
        |  }
        |}""".stripMargin

  }

  behavior of "A Node"

  it should "print it's name" in {
    import Graph._
    node('name).stringify shouldBe "name"
  }

  it should "print it's name and attribute" in {
    import Graph._
    node('name, fontsize := 1).stringify shouldBe "name [fontsize = 1]"
  }

  it should "print it's name and attributes" in {
    import Graph._
    node('name, label := "one", fontsize := 2).stringify shouldBe """name [label = "one", fontsize = 2]"""
  }

  behavior of "Attribute"

  it should "print name = value" in {
    import Graph._
    (fontsize := 1).stringify shouldBe "fontsize = 1"
  }

  it should """print name = "value"""" in {
    import Graph._
    (label := "two").stringify shouldBe """label = "two""""
  }

  it should """print enum values""" in {
    import Graph._
    (shape := box).stringify shouldBe "shape = box"
  }

  behavior of "Edge"

  it should "print an id" in {
    import Graph._
    edge('from, 'to).stringify shouldBe "from -- to"
  }

  it should "print a directed edge" in {
    import DiGraph._
    edge('from, 'to).stringify shouldBe "from -> to"
  }

  it should "create edge from strings" in {
    import DiGraph._

    val e: Edge = ("from", "to")

    e.stringify shouldBe "from -> to"

  }

  it should "create edge from seq of strings" in {
    import DiGraph._

    val a: Iterable[Statement] = Seq[(String, String)]("from" -> "to")

    a shouldBe Seq[Edge]('from -> 'to)

  }

  behavior of "Cluster"

  it should "print a cluster" in {
    import Graph._

    implicit val inc = new Inc

    val c0 = cluster(node('anode), node('bnode), edge('anode, 'bnode), label := "two")

    c0.stringify shouldBe
      """subgraph cluster_0 {
        |  anode;
        |  bnode;
        |  anode -- bnode;
        |  label = "two";
        |}""".stripMargin

    val c1 = cluster(Seq(node('anode), node('bnode), edge('anode, 'bnode), label := "two"))
    c1.stringify shouldBe
      """subgraph cluster_1 {
        |  anode;
        |  bnode;
        |  anode -- bnode;
        |  label = "two";
        |}""".stripMargin

  }

}
