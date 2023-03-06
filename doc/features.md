- provides a simple immutable monadic implementation of a DAG
- implements `map`, `flatMap` and `filter` for `Dag`s
- can deduce a partial order on a graph
- generates `Dot` instances representing a DOT abstract syntax tree
- serializes `Dot` instances to `String`s, which can be rendered by GraphViz
- can find the transitive closure, transitive reduction and inverse of a graph
- methods for addition and subtraction of graph nodes