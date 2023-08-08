[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/acyclicity/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/acyclicity/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Acyclicity

__Monadic directed acyclic graph datastructures__

Acyclicity provides a single data structure, `Dag[T]`, representing a graph of
nodes of type `T`, with monadic operations and several other utility methods,
plus the means to generate DOT for input to GraphViz.

## Features

- provides a simple immutable monadic implementation of a DAG
- implements `map`, `flatMap` and `filter` for `Dag`s
- can deduce a partial order on a graph
- generates `Dot` instances representing a DOT abstract syntax tree
- serializes `Dot` instances to `String`s, which can be rendered by GraphViz
- can find the transitive closure, transitive reduction and inverse of a graph
- methods for addition and subtraction of graph nodes


## Availability

Acyclicity has not yet been published as a binary.

## Getting Started

Acyclicity provides a monadic representation of a directed, acyclic graph (DAG) called `Dag`, and support for
generating [DOT](https://bit.ly/3vFumLW) files which can be rendered with tools such as
[GraphViz](https://graphviz.org/).

The `Dag[T]` type represents a mapping from nodes of type `T` to zero, one or many other nodes in the graph, and
can be constructed by providing the mapping from each node to its `Set` of dependent nodes, or by calling,
```scala
Dag.from(nodes)(fn)
```
where `nodes` is a `Set` of nodes, and `fn` is a function from each node to its dependencies.

For example,
```scala
val factors = Dag(
  30 -> Set(2, 3, 4, 5, 10, 15),
  15 -> Set(5, 3),
  10 -> Set(5, 2),
  5 -> Set(),
  3 -> Set(),
  2 -> Set()
)
```

### Monadic Operations

A `Dag[T]` may be mapped to a `Dag[S]` with a function `T => S`, like so:
```scala
factors.map(_*10)
```

Care should be taken when more than one node in the domain maps to a single node in the range, but both incoming
and outgoing edges will be merged in such cases.

It's also possible to `flatMap` with a function `T => Dag[S]`. This will replace every node of type `T` with a
subgraph, `Dag[S]` with incoming edges attached to all source nodes of the subgraph, and pre-existing outgoing
edges attached to all destination nodes of the subgraph.

A `Dag[T]` may also be filtered with a predicate, `T => Boolean`. The removal of a node during filtering will
reattach every incoming edge to every outgoing edge of that node.

### Other Operations

The method `Dag#reduction` will calculate the transitive reduction of the graph, removing any direct edge
between two nodes when transitive edges exist between those nodes.

The dual of this operation is the transitive closure, which adds direct edges between each pair of nodes between
which a transitive path exists. This is available with the `Dag#closure` method.

A list of nodes will be returned in topologically-sorted order by calling `Dag#sorted`.

### DOT output

An extension method, `dot`, on `Dag`s of `String`s will produce a `Dot` instance, an AST of the DOT code
necessary to render a graph. This can then be serialized to a `String` with the `serialize` method.

Typical usage would be to first convert a `Dag[T]` to a `Dag[String]`, then produce the `Dot` instance and
serialize it, for example:
```scala
println(dag.map(_.name).dot.serialize)
```

### Limitations

This library is incomplete, inadequately tested and subject to further development, and is recommended to be
used by developers who do not mind examining the source code to diagnose unexpected behavior.



## Status

Acyclicity is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Acyclicity is designed to be _small_. Its entire source code currently consists
of 234 lines of code.

## Building

Acyclicity can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Acyclicity are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/acyclicity/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Acyclicity easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Acyclicity was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

Acyclicity takes its name from the graphs it represents, which must not contain cycles.

In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meanings—since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## License

Acyclicity is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
