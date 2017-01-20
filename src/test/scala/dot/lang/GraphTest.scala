package dot.lang

import org.scalatest.{FlatSpec, Matchers}

class GraphTest extends FlatSpec with Matchers {

  behavior of "A Directed Graph"

  it should "generate a digraph string" in {
    import DiGraph._

    digraph(
      node('blah),
      edge('from, 'to)
    ).toString shouldBe
      """digraph {
        |  blah;
        |  from -> to;
        |}""".stripMargin

  }

  it should "generate a digraph from a Seq" in {
    import DiGraph._

    val seq = Seq(node('blah), edge('from, 'to))

    digraph(seq).toString shouldBe
      """digraph {
        |  blah;
        |  from -> to;
        |}""".stripMargin

  }

  it should "generate a graph from a Seq" in {
    import Graph._

    val seq = Seq(node('blah), edge('from, 'to))

    graph(seq).toString shouldBe
      """graph {
        |  blah;
        |  from -- to;
        |}""".stripMargin

  }

  it should "generate attributes" in {

    import DiGraph._

    digraph(label := "two").toString shouldBe
      """digraph {
        |  label = "two";
        |}""".stripMargin
  }

  it should "mix attributes and statements" in {

    import DiGraph._

    digraph(layout := "two", node('anode)).toString shouldBe
      """digraph {
        |  layout = "two";
        |  anode;
        |}""".stripMargin
  }

  behavior of "A Graph"

  it should "generate a graph string" in {

    import Graph._

    val g = graph(
      'blah,
      'from -> 'to,
      cluster()
    )

    g.toString shouldBe
      """graph {
        |  blah;
        |  from -- to;
        |  subgraph cluster_0 {}
        |}""".stripMargin

  }

  behavior of "A Node"

  it should "print it's name" in {
    import Graph._
    node('name).toString shouldBe "name;"
  }

  it should "print it's name and attribute" in {
    import Graph._
    node('name, fontsize := 1).toString shouldBe "name [fontsize = 1];"
  }

  it should "print it's name and attributes" in {
    import Graph._
    node('name, label := "one", fontsize := 2).toString shouldBe """name [label = "one", fontsize = 2];"""
  }

  behavior of "Attribute"

  it should "print name = value" in {
    import Graph._
    (fontsize := 1).toString shouldBe "fontsize = 1"
  }

  it should """print name = "value"""" in {
    import Graph._
    (label := "two").toString shouldBe """label = "two""""
  }

  behavior of "EdgeStatement"

  it should "print an id" in {
    import Graph._
    edge('from, 'to).toString shouldBe "from -- to;"
  }

  it should "print a directed edge" in {
    import DiGraph._
    edge('from, 'to).toString shouldBe "from -> to;"
  }

  behavior of "Cluster"

  it should "print a cluster" in {
    import Graph._

    val c = cluster(node('anode), node('bnode), edge('anode, 'bnode), label := "two")

    c.toString shouldBe
      """subgraph cluster_0 {
        |  anode;
        |  bnode;
        |  anode -- bnode;
        |  label = "two";
        |}""".stripMargin

  }

}
