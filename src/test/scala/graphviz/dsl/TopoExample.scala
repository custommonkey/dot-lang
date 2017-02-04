package graphviz.dsl

import better.files.File
import graphviz.dsl.Graph._

import scala.collection.mutable

object TopoExample extends App {

  implicit val inc = new Inc

  val nodes = mutable.Buffer[Node]()

  def centre(name: String, apps: Statement*): Cluster =
    cluster(Seq[Statement](label := name) ++ apps)


  def app(centre: String, name: String): Symbol = {
    val n = node(centre + name, label := name)
    nodes += n
    n.id
  }

  def server(name: String): Symbol = {
    val n = node(name, label := name, shape := box3d)
    nodes += n
    n.id
  }

  case class Env(name: String, centres: Seq[Cluster] = Nil) {
    def make: Cluster =
      cluster(
        Seq[Statement](label := name) ++ centres
      )

    def withCentre(centre: Cluster): Env = copy(centres = centres :+ centre)
  }

  val dev = Env("Dev")
    .withCentre(
      centre("aaaa",
        app("aaaa", "one") -> server("two"),
        app("aaaa", "one") -> server("three")
      )
    )
    .withCentre(
      centre("aaab",
        app("aaab", "one") -> server("four"),
        app("aaab", "one") -> server("five")
      )
    ).make

  val test = Env("Test")
    .withCentre(
      centre("bbbb", app("bbbb", "c") -> server("d"))
    ).make

  val stage = Env("Stage")
    .withCentre(
      centre("cccc", 'e -> 'f)
    ).make

  val styles = Seq[Statement](
    bgcolour := "#0B486B",
    colour := "#79BD9A",
    fontcolour := "#CFF09E",
    node('node, style := filled, fillcolour := "#3B8686", fontcolour := "#CFF09E", colour := "#79BD9A"),
    node('edge, colour := "#79BD9A")
  )

  val dot = graph(
    styles,
    nodes,
    dev,
    test,
    stage
  ).toString

  println(dot)

  File("topo.gv") < dot

}
