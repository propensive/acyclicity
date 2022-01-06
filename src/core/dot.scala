/*
    Acyclicity, version 0.5.0. Copyright 2021-22 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package acyclicity

import contextual.*
import rudiments.*
import gossamer.*

import language.dynamics

enum Dot:
  case Graph(id: Option[Dot.Id], strict: Boolean, statements: Dot.Statement*)
  case Digraph(id: Option[Dot.Id], strict: Boolean, statements: Dot.Statement*)

  def serialize: Text = Dot.serialize(Dot.tokenize(this))

  def add(newStatements: Dot.Statement*): Dot = this match
    case Dot.Graph(id, strict, statements*) =>
      Dot.Graph(id, strict, (statements ++ newStatements)*)
    
    case Dot.Digraph(id, strict, statements*) =>
      Dot.Digraph(id, strict, (statements ++ newStatements)*)

object Dot:
  case class Target(directed: Boolean, dest: Ref | Statement.Subgraph, link: Option[Target])
  case class Property(key: Text, value: Text)

  object Attachment:
    // FIXME: This needs to include the port
    given Show[Attachment] = _.id.key
  
  case class Attachment(id: Id, compass: Option[CompassPoint] = None)

  case class Ref(id: Id, port: Option[Attachment] = None):
    @targetName("joinTo")
    infix def --(dest: Ref | Statement.Subgraph): Dot.Statement.Edge =
      Dot.Statement.Edge(this, Target(false, dest, None))
    
    @targetName("mapTo")
    infix def -->(dest: Ref | Statement.Subgraph): Dot.Statement.Edge =
      Dot.Statement.Edge(this, Target(true, dest, None))

  object Ref:
    def apply(key: Text): Ref = Ref(Id(key))

  case class Id(key: Text) extends Dynamic:
    def applyDynamicNamed(method: "apply")(attrs: (String, Text)*) =
      Statement.Node(this, attrs.map { (k, v) => Property(k.show, v) }*)
    
    @targetName("assign")
    infix def :=(id: Id): Statement.Assignment = Statement.Assignment(this, id)

  enum CompassPoint:
    case North, South, East, West, NorthEast, NorthWest, SouthEast, SouthWest
  
  enum Statement:
    case Node(id: Id, attrs: Property*)
    case Edge(id: Ref, rhs: Target, attrs: Property*)
    case Assignment(id: Id, id2: Id)
    case Subgraph(id: Option[Id], statements: Statement*)

  def serialize(tokens: Vector[Text]): Text =
    val buf: StringBuilder = StringBuilder()
    var level: Int = 0
    var end: Boolean = true

    def indent(): Unit = level += 1
    def outdent(): Unit = level -= 1
    def newline(): Unit = end = true

    def whitespace(): Unit =
      if end then
        buf.add(t"\n")
        buf.add(t"  "*level)
        end = false
      else buf.add(t" ")

    tokens.foreach:
      case t""  => ()
      case t"," => buf.add(t",")
      case t"{" => whitespace(); buf.add(t"{"); indent(); newline()
      case t"}" => outdent(); whitespace(); buf.add(t"}"); newline()
      case t"[" => whitespace(); buf.add(t"[")
      case t"]" => whitespace(); buf.add(t"]"); newline()
      case t";" => newline()
      case word => whitespace(); buf.add(word)

    Showable(buf).show

  private def tokenize(graph: Ref | Dot | Target | Statement | Property): Vector[Text] = graph match
    case Ref(id, port) =>
      Vector(port.fold(t""""${id.key}"""") { p => t""""${id.key}:$p""""" })
    
    case Property(key, value) =>
      Vector(t"""$key="$value"""")
    
    case Target(directed, dest, link) =>
      val op = if directed then t"->" else t"--"
      Vector(op) ++ tokenize(dest) ++ link.to(Vector).flatMap(tokenize(_)) ++ Vector(t";")

    case Statement.Node(id, attrs*) =>
      Vector(t""""${id.key}"""") ++ (if attrs.isEmpty then Vector() else (Vector(t"[") ++
          attrs.to(Vector).flatMap(tokenize(_) :+ t",").init ++ Vector(t"]"))) ++ Vector(t";")
    
    case Statement.Edge(id, rhs, attrs*) =>
      tokenize(id) ++ tokenize(rhs)
    
    case Statement.Assignment(id, id2) =>
      Vector(t""""${id.key}"""", t"=", t""""${id2.key}"""", t";")
    
    case Statement.Subgraph(id, statements*) =>
      Vector(t"subgraph") ++ id.to(Vector).map(_.key) ++ Vector(t"{") ++
          statements.flatMap(tokenize(_)) ++ Vector(t"}")
    
    case Dot.Graph(id, strict, statements*) =>
      Vector(
        if strict then Vector(t"strict") else Vector(),
        Vector(t"graph"),
        id.to(Vector).map(_.key), Vector(t"{"),
        statements.flatMap(tokenize(_)), Vector(t"}")
      ).flatten
    
    case Dot.Digraph(id, strict, statements*) =>
      Vector(
        if strict then Vector(t"strict") else Vector(),
        Vector(t"digraph"),
        id.to(Vector).map(_.key),
        Vector(t"{"),
        statements.flatMap(tokenize(_)),
        Vector(t"}")
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
  
  def apply(statements: Dot.Statement*): Dot.Statement.Subgraph =
    Dot.Statement.Subgraph(None, statements*)

object NodeParser extends Interpolator[Unit, Option[Dot.Ref], Dot.Ref]:

  private val compassPoints: Set[Text] = Set(t"n", t"e", t"s", t"w", t"ne", t"nw", t"se", t"sw")
  def parse(state: Option[Dot.Ref], next: String): Some[Dot.Ref] =
    Some { next.show.cut(t":").to(List) match
      case List(id) =>
        Dot.Ref(Dot.Id(id))
      
      case List(id, port) =>
        Dot.Ref(Dot.Id(id), Some(Dot.Attachment(Dot.Id(port.show))))
      
      case List(id, port, point) if compassPoints.contains(point) =>
        Dot.Ref(Dot.Id(id), Some(Dot.Attachment(Dot.Id(port),
            Some(Dot.CompassPoint.valueOf(point.capitalize.s)))))
      
      case _ =>
        throw InterpolationError(t"not a valid node ID")
    }
  
  def initial: Option[Dot.Ref] = None
  def complete(value: Option[Dot.Ref]): Dot.Ref = value.get
  def skip(state: Option[Dot.Ref]): Option[Dot.Ref] = state
  def insert(state: Option[Dot.Ref], value: Unit): Option[Dot.Ref] = state
