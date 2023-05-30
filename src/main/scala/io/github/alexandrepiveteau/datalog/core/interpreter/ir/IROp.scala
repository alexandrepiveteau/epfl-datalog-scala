package io.github.alexandrepiveteau.datalog.core.interpreter.ir

import io.github.alexandrepiveteau.datalog.core.interpreter.algebra.*
import io.github.alexandrepiveteau.datalog.core.interpreter.database.{PredicateWithArity, StorageManager, nonEmpty}
import io.github.alexandrepiveteau.datalog.core.rule.Value
import io.github.alexandrepiveteau.datalog.core.{AggregationFunction, Domain}

// TODO : Document this.
sealed trait IROp[T]

// TODO : Document this.
case class IRSequence[T](ops: List[IROp[T]]) extends IROp[T]

// TODO : Document this.
case class IRStore[T](database: Database, predicate: PredicateWithArity, relation: IRRelOp[T]) extends IROp[T]

// TODO : Document this.
case class IRDoWhileNotEqual[T](op: IROp[T], first: Database, second: Database) extends IROp[T]

// TODO : Document this.
case class IRDoWhileNotEmpty[T](op: IROp[T], database: Database) extends IROp[T]

// TODO : Document this.
case class IRMergeAndClear[T]() extends IROp[T]

// TODO : Document this.
sealed trait IRRelOp[T] extends IROp[T]:
  val arity: Int

// TODO : Document this.
case class IRRelEmpty[T](arity: Int) extends IRRelOp[T]

// TODO : Document this.
case class IRRelDomain[T](arity: Int, values: Iterable[Value[T]]) extends IRRelOp[T]

// TODO : Document this.
case class IRRelScan[T](database: Database, predicate: PredicateWithArity) extends IRRelOp[T]:
  override val arity: Int = predicate.arity

// TODO : Document this.
case class IRRelSelect[T](relation: IRRelOp[T], selection: Set[Set[Column[T]]]) extends IRRelOp[T]:
  override val arity: Int = relation.arity

// TODO : Document this.
case class IRRelJoin[T](relations: List[IRRelOp[T]]) extends IRRelOp[T]:
  override val arity: Int = relations.map(_.arity).sum

// TODO : Document this.
case class IRRelUnion[T](first: IRRelOp[T], second: IRRelOp[T]) extends IRRelOp[T]:
  override val arity: Int = first.arity

// TODO : Document this.
case class IRRelDistinct[T](relation: IRRelOp[T]) extends IRRelOp[T]:
  override val arity: Int = relation.arity

// TODO : Document this.
case class IRRelMinus[T](relation: IRRelOp[T], removed: IRRelOp[T]) extends IRRelOp[T]:
  override val arity: Int = relation.arity

// TODO : Document this.
case class IRRelProject[T](relation: IRRelOp[T], columns: List[Column[T]]) extends IRRelOp[T]:
  override val arity: Int = columns.size

// TODO : Document this.
case class IRRelAggregate[T](relation: IRRelOp[T],
                             projection: List[AggregationColumn[T]],
                             same: Set[Index],
                             domain: Domain[T],
                             aggregate: AggregationFunction,
                             indices: Set[Index],
                            ) extends IRRelOp[T]:
  override val arity: Int = projection.size

// INTERPRETATION OF THE IR

// TODO : Document this.
extension[T] (op: IROp[T])

  // TODO : Document this.
  def compute(storage: StorageManager[T]): Unit =
    op match
      case IRSequence(ops) => ops.foreach(_.compute(storage))
      case IRStore(database, predicate, relation) =>
        storage
          .database(database)
          .update(predicate, relation.rel(using storage))
      case IRDoWhileNotEqual(op, first, second) =>
        while
          op.compute(storage)
          storage.database(first) == storage.database(second)
        do ()
      case IRDoWhileNotEmpty(op, database) =>
        while
          op.compute(storage)
          storage.database(database).nonEmpty
        do ()
      case IRMergeAndClear() =>
        storage.database(Database.Base) += storage.database(Database.Result)
        storage.removeAll(Set(Database.Base))
      case _: IRRelOp[_] => ()

// TODO : Document this.
extension[T] (op: IRRelOp[T])

  // TODO : Document this.
  def rel(using storage: StorageManager[T]): Relation[T] =
    op match
      case IRRelAggregate(relation, projection, same, domain, aggregate, indices) =>
        relation.rel.aggregate(projection, same, aggregate, indices)(using domain)
      case IRRelDistinct(relation) => relation.rel.distinct()
      case IRRelDomain(arity, values) => Relation.domain(arity, values.toSet)
      case IRRelEmpty(arity) => Relation.empty(arity)
      case IRRelJoin(relations) => relations.map(_.rel).join()
      case IRRelMinus(relation, removed) => relation.rel.minus(removed.rel)
      case IRRelProject(relation, columns) => relation.rel.project(columns)
      case IRRelScan(database, predicate) => storage.database(database)(predicate)
      case IRRelSelect(relation, selection) => relation.rel.select(selection)
      case IRRelUnion(first, second) => first.rel.union(second.rel)
