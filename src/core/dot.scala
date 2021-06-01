package acyclicity

import contextual.*

import language.dynamics

enum Dot:
  case Graph(id: Option[Dot.Id], strict: Boolean, statements: Dot.Statement*)
  case Digraph(id: Option[Dot.Id], strict: Boolean, statements: Dot.Statement*)

  def serialize: String = Dot.serialize(Dot.tokenize(this))

object Dot:
  case class Target(directed: Boolean, dest: Ref | Statement.Subgraph, link: Option[Target])
  case class Attribute(key: String, value: String)
  case class Attachment(id: Id, compass: Option[CompassPoint] = None)

  case class Ref(id: Id, port: Option[Attachment] = None):
    def --(dest: Ref | Statement.Subgraph): Dot.Statement.Edge =
      Dot.Statement.Edge(this, Target(false, dest, None))
    
    def -->(dest: Ref | Statement.Subgraph): Dot.Statement.Edge =
      Dot.Statement.Edge(this, Target(true, dest, None))

  object Ref:
    def apply(key: String): Ref = Ref(Id(key))

  case class Id(key: String) extends Dynamic:
    def applyDynamicNamed(method: "apply")(attrs: (String, String)*) =
      Statement.Node(this, attrs.map { case (k, v) => Attribute(k, v) }*)
    
    def :=(id: Id): Statement.Assignment = Statement.Assignment(this, id)

  enum CompassPoint:
    case North, South, East, West, NorthEast, NorthWest, SouthEast, SouthWest
  
  enum Statement:
    case Node(id: Id, attrs: Attribute*)
    case Edge(id: Ref, rhs: Target, attrs: Attribute*)
    case Assignment(id: Id, id2: Id)
    case Subgraph(id: Option[Id], statements: Statement*)


  def serialize(tokens: Vector[String]): String =
    var buf: StringBuilder = StringBuilder()
    var level: Int = 0
    var end: Boolean = true

    def indent(): Unit = level += 1
    def outdent(): Unit = level -= 1
    def newline(): Unit = end = true

    def whitespace(): Unit =
      if end then
        buf.append("\n")
        buf.append("  "*level)
        end = false
      else buf.append(" ")

    tokens.foreach {
      case ""   => ()
      case ","  => buf.append(",")
      case "{"  => whitespace(); buf.append("{"); indent(); newline()
      case "}"  => outdent(); whitespace(); buf.append("}"); newline()
      case "["  => whitespace(); buf.append("[")
      case "]"  => whitespace(); buf.append("]"); newline()
      case ";"  => newline()
      case word => whitespace(); buf.append(word)
    }

    buf.toString

  def tokenize(graph: Ref | Dot | Target | Statement | Attribute): Vector[String] = graph match
    case Ref(id, port) =>
      Vector(port.fold(s""""${id.key}"""") { p => s""""${id.key}:$p""""" })
    
    case Attribute(key, value) =>
      Vector(s"""$key="$value"""")
    
    case Target(directed, dest, link) =>
      val op = if directed then "->" else "--"
      Vector(op) ++ tokenize(dest) ++ link.to(Vector).flatMap(tokenize(_)) ++ Vector(";")

    case Statement.Node(id, attrs*) =>
      Vector(s""""${id.key}"""") ++ (if attrs.isEmpty then Vector() else (Vector("[") ++
          attrs.to(Vector).flatMap(tokenize(_) :+ ",").init ++ Vector("]"))) ++ Vector(";")
    
    case Statement.Edge(id, rhs, attrs*) =>
      tokenize(id) ++ tokenize(rhs)
    
    case Statement.Assignment(id, id2) =>
      Vector(s""""${id.key}"""", "=", s""""${id2.key}"""", ";")
    
    case Statement.Subgraph(id, statements*) =>
      Vector("subgraph") ++ id.to(Vector).map(_.key) ++ Vector("{") ++ statements.flatMap(tokenize(_)) ++
          Vector("}")
    
    case Dot.Graph(id, strict, statements*) =>
      Vector(
        if strict then Vector("strict") else Vector(), Vector("graph"), id.to(Vector).map(_.key), Vector("{"),
            statements.flatMap(tokenize(_)), Vector("}")
      ).flatten
    
    case Dot.Digraph(id, strict, statements*) =>
      Vector(
        if strict then Vector("strict") else Vector(), Vector("digraph"), id.to(Vector).map(_.key), Vector("{"),
            statements.flatMap(tokenize(_)), Vector("}")
      ).flatten

object Digraph:
  def apply(id: Dot.Id, statements: Dot.Statement*): Dot = Dot.Digraph(Some(id), false, statements*)
  def apply(statements: Dot.Statement*): Dot = Dot.Digraph(None, false, statements*)
  def strict(id: Dot.Id, statements: Dot.Statement*): Dot = Dot.Digraph(Some(id), true, statements*)

object Graph:
  def apply(id: Dot.Id, statements: Dot.Statement*): Dot = Dot.Graph(Some(id), false, statements*)
  def strict(id: Dot.Id, statements: Dot.Statement*): Dot = Dot.Graph(Some(id), true, statements*)

object Subgraph:
  def apply(id: Dot.Id, statements: Dot.Statement*): Dot.Statement.Subgraph =
    Dot.Statement.Subgraph(Some(id), statements*)
  
  def apply(statements: Dot.Statement*): Dot.Statement.Subgraph = Dot.Statement.Subgraph(None, statements*)

object NodeParser extends Interpolator[Unit, Option[Dot.Ref], Dot.Ref]:
  def parse(state: Option[Dot.Ref], next: String): Some[Dot.Ref] = Some { next.split(":").to(List) match
    case List(id) =>
      Dot.Ref(Dot.Id(id))
    
    case List(id, port) =>
      Dot.Ref(Dot.Id(id), Some(Dot.Attachment(Dot.Id(port))))
    
    case List(id, port, point@("n" | "e" | "s" | "w" | "ne" | "nw" | "se" | "sw")) =>
      Dot.Ref(Dot.Id(id), Some(Dot.Attachment(Dot.Id(port), Some(Dot.CompassPoint.valueOf(point.capitalize)))))
    
    case _ =>
      throw ParseError("not a valid node ID")
  }
  
  def initial: Option[Dot.Ref] = None
  def complete(value: Option[Dot.Ref]): Dot.Ref = value.get
  def insert(state: Option[Dot.Ref], value: Option[Unit]): Option[Dot.Ref] = state