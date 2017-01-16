import org.scalatest.Matchers

class GraphTest extends org.scalatest.FlatSpec with Matchers with GraphWords {

  behavior of "A Graph"

  it should "generate a graph string" in {

    graph(
      node('blah),
      edge('from, 'to)
    ).toString shouldBe
      """digraph {
        |blah;
        |from -> to;
        |}""".stripMargin

  }

  behavior of "A Node"

  it should "print it's name" in {
    node('name).toString shouldBe "name;"
  }

  it should "print it's name and attribute" in {
    node('name, 'at := 1).toString shouldBe "name [at = 1];"
  }

  it should "print it's name and attributes" in {
    node('name, 'one := 1, 'two := 2).toString shouldBe "name [one = 1, two = 2];"
  }

  behavior of "Attribute"

  it should "print name = value" in {
    ('one := 1).toString shouldBe "one = 1"
  }

  it should """print name = "value"""" in {
    ('one := "two").toString shouldBe """one = "two""""
  }

  behavior of "EdgeStatement"

  it should "print an id" in {
    edge('from, 'to).toString shouldBe "from -> to;"
  }

}
