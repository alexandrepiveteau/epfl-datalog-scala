package io.github.alexandrepiveteau.datalog.core.interpreter.engine.staged

def prettyCode(code: String): String =
  code
    .replace("scala.", "")
    .replace("Predef.", "")
    .replace(".apply", "")
    .replace("io.github.alexandrepiveteau.datalog.core.", "")
    .replace("interpreter.ir.", "")
    .replace("interpreter.algebra.", "")
    .replace("interpreter.database.", "")
    .replace("rule.Predicate", "Predicate")
    .replace("[Int]", "")
    .replace("[Database]", "")
    .replace("[Column]", "")
    .replace("[TupleSet]", "")
    .replace("TupleSet.", "")
    .replace("collection.immutable.", "")
