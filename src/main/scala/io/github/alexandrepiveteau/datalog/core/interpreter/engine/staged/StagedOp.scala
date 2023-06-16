package io.github.alexandrepiveteau.datalog.core.interpreter.engine.staged

import io.github.alexandrepiveteau.datalog.core.*
import io.github.alexandrepiveteau.datalog.core.interpreter.algebra.*
import io.github.alexandrepiveteau.datalog.core.interpreter.database.{PredicateWithArity, StorageManager, nonEmpty}
import io.github.alexandrepiveteau.datalog.core.interpreter.ir.{Database, IROp}
import io.github.alexandrepiveteau.datalog.core.rule.{Predicate, Value}

import scala.quoted.*

/**
 * A [[StagedOp]] is an intermediate representation of an operation which will be compiled through multi-stage
 * execution. It is a tree of operations, which can be traversed and compiled to perform multi-stage execution.
 *
 * @tparam C the type of the constants.
 * @tparam O the type of the output for this operation.
 */
sealed trait StagedOp[C, O, R[_]]

case class SequenceOp[C, R[_]](ops: List[StagedOp[C, Unit, R]]) extends StagedOp[C, Unit, R]

case class ScanOp[C, R[_]](database: Database,
                     predicate: PredicateWithArity) extends StagedOp[C, R[C], R]

case class StoreOp[C, R[_]](database: Database,
                      predicate: PredicateWithArity,
                      relation: StagedOp[C, R[C], R]) extends StagedOp[C, Unit, R]

case class DoWhileNotEqualOp[C, R[_]](op: StagedOp[C, Unit, R],
                                first: Database,
                                second: Database) extends StagedOp[C, Unit, R]

case class DoWhileNonEmptyOp[C, R[_]](op: StagedOp[C, Unit, R],
                                database: Database) extends StagedOp[C, Unit, R]

case class MergeAndClearOp[C, R[_]]() extends StagedOp[C, Unit, R]

case class EmptyOp[C, R[_]](arity: Int) extends StagedOp[C, R[C], R]

case class DomainOp[C, R[_]](arity: Int, values: Set[Value[C]]) extends StagedOp[C, R[C], R]

case class JoinOp[C, R[_]](relations: List[StagedOp[C, R[C], R]]) extends StagedOp[C, R[C], R]

case class UnionOp[C, R[_]](relations: Set[StagedOp[C, R[C], R]]) extends StagedOp[C, R[C], R]

case class ArityOp[C, R[_]](relation: StagedOp[C, R[C], R]) extends StagedOp[C, Int, R]

case class AggregateOp[C, R[_]](relation: StagedOp[C, R[C], R],
                          projection: List[AggregationColumn[C]],
                          same: Set[Index],
                          aggregate: AggregationFunction,
                          indices: Set[Index],
                         )(using val domain: Domain[C]) extends StagedOp[C, R[C], R]

case class MinusOp[C, R[_]](left: StagedOp[C, R[C], R], right: StagedOp[C, R[C], R]) extends StagedOp[C, R[C], R]

case class DistinctOp[C, R[_]](relation: StagedOp[C, R[C], R]) extends StagedOp[C, R[C], R]

case class ProjectOp[C, R[_]](relation: StagedOp[C, R[C], R], projection: List[Column[C]]) extends StagedOp[C, R[C], R]

case class SelectOp[C, R[_]](relation: StagedOp[C, R[C], R], selection: Set[Set[Column[C]]]) extends StagedOp[C, R[C], R]

/**
 * An implementation of [[IROp]] which constructs a tree of [[StagedOp]]s. The tree can then be traversed and compiled
 * to perform multi-stage execution.
 */
given StagedIROp[C]: IROp[C, [R] =>> StagedOp[C, R, TupleSet], TupleSet] with

  override def sequence(ops: List[StagedOp[C, Unit, TupleSet]]): StagedOp[C, Unit, TupleSet] =
    SequenceOp(ops)

  override def scan(database: Database,
                    predicate: PredicateWithArity): StagedOp[C, TupleSet[C], TupleSet] =
    ScanOp(database, predicate)

  override def store(database: Database,
                     predicate: PredicateWithArity,
                     relation: StagedOp[C, TupleSet[C], TupleSet]): StagedOp[C, Unit, TupleSet] =
    StoreOp(database, predicate, relation)

  override def doWhileNotEqual(op: StagedOp[C, Unit, TupleSet],
                               first: Database,
                               second: Database): StagedOp[C, Unit, TupleSet] =
    DoWhileNotEqualOp(op, first, second)

  override def doWhileNonEmpty(op: StagedOp[C, Unit, TupleSet],
                               database: Database): StagedOp[C, Unit, TupleSet] =
    DoWhileNonEmptyOp(op, database)

  override def mergeAndClear(): StagedOp[C, Unit, TupleSet] =
    MergeAndClearOp()

  override def empty(arity: Int): StagedOp[C, TupleSet[C], TupleSet] =
    EmptyOp(arity)

  override def domain(arity: Int, values: Set[Value[C]]): StagedOp[C, TupleSet[C], TupleSet] =
    DomainOp(arity, values)

  override def join(relations: List[StagedOp[C, TupleSet[C], TupleSet]]): StagedOp[C, TupleSet[C], TupleSet] =
    JoinOp(relations)

  override def union(relations: Set[StagedOp[C, TupleSet[C], TupleSet]]): StagedOp[C, TupleSet[C], TupleSet] =
    UnionOp(relations)

  extension (relation: StagedOp[C, TupleSet[C], TupleSet])

    override def arity: StagedOp[C, Int, TupleSet] =
      ArityOp(relation)

    override def aggregate(projection: List[AggregationColumn[C]],
                           same: Set[Index],
                           aggregate: AggregationFunction,
                           indices: Set[Index],
                          )(using domain: Domain[C]): StagedOp[C, TupleSet[C], TupleSet] =
      AggregateOp(relation, projection, same, aggregate, indices)

    override def minus(other: StagedOp[C, TupleSet[C], TupleSet]): StagedOp[C, TupleSet[C], TupleSet] =
      MinusOp(relation, other)

    override def distinct(): StagedOp[C, TupleSet[C], TupleSet] =
      DistinctOp(relation)

    override def project(projection: List[Column[C]]): StagedOp[C, TupleSet[C], TupleSet] =
      ProjectOp(relation, projection)

    override def select(selection: Set[Set[Column[C]]]): StagedOp[C, TupleSet[C], TupleSet] =
      SelectOp(relation, selection)

// COMPILATION OF THE IR TO AN EXPRESSION

def compile[C: Type : ToExpr, O: Type : ToExpr](op: StagedOp[C, O, TupleSet])
                                               (using quotes: Quotes, s: Expr[StorageManager[C]], d: Expr[Domain[C]]): Expr[O] =
  op match
    case SequenceOp(ops) => Expr.block[Unit](ops.map(compile(_)), '{ () })
    case ScanOp(database, predicate) =>
      '{ ${ s }.database(${ Expr(database) }).apply(${ Expr(predicate) }) }
    case StoreOp(database, predicate, relation) =>
      '{ ${ s }.database(${ Expr(database) }).update(${ Expr(predicate) }, ${ compile(relation) }) }
    case DoWhileNotEqualOp(op, first, second) =>
      '{
        while
          ${ compile(op) }
          $s.database(${ Expr(first) }) != $s.database(${ Expr(second) })
        do ()
      }
    case DoWhileNonEmptyOp(op, database) =>
      '{
        while
          ${ compile(op) }
          $s.database(${ Expr(database) }).nonEmpty
        do ()
      }
    case MergeAndClearOp() => '{
      $s.database(Database.Base) += $s.database(Database.Result)
      $s.removeAll(Set(Database.Base))
    }
    case EmptyOp(arity) => '{
      TupleSet.empty[C](${ Expr(arity) })
    }
    case DomainOp(arity, values) => '{
      TupleSet.domain[C](${ Expr(arity) }, ${ Expr(values) })
    }
    case JoinOp(relations) =>
      '{ TupleSet.join[C](${ Expr.ofList(relations.map(compile(_))) }) }
    case UnionOp(relations) =>
      '{ TupleSet.union[C](${ Expr.ofSet(relations.map(compile(_))) }) }
    case ArityOp(relation) =>
      '{ TupleSet.arity(${ compile(relation) }) }
    case AggregateOp(relation, projection, same, aggregate, indices) =>
      '{
        val r = ${ compile(relation) }
        TupleSet.aggregate(r,
          ${ Expr(projection) },
          ${ Expr(same) },
          ${ Expr(aggregate) },
          ${ Expr(indices) },
        )(using $d)
      }
    case MinusOp(first, second) =>
      '{
        TupleSet.minus(
          ${ compile(first) },
          ${ compile(second) },
        )
      }
    case DistinctOp(relation) => '{ TupleSet.distinct(${ compile(relation) }) }
    case ProjectOp(relation, projection) =>
      '{
        TupleSet.project(
          ${ compile(relation) },
          ${ Expr(projection) },
        )
      }
    case SelectOp(relation, selection) =>
      '{
        TupleSet.select(
          ${ compile(relation) },
          ${ Expr(selection) },
        )
      }

given ValueToExpr[T: Type : ToExpr]: ToExpr[Value[T]] with
  override def apply(x: Value[T])(using Quotes): Expr[Value[T]] =
    '{ Value[T](${ Expr(x.value) }) }

given TupleSetToExpr[T: Type : ToExpr]: ToExpr[TupleSet[T]] with
  override def apply(x: TupleSet[T])(using Quotes): Expr[TupleSet[T]] =
    '{ TupleSet[T](${ Expr(x.arity) }, ${ Expr(x.tuples) }) }

given DatabaseToExpr: ToExpr[Database] with
  override def apply(x: Database)(using Quotes): Expr[Database] =
    '{ Database(${ Expr(x.backing) }) }

given AggregationColumnToExpr[T: Type : ToExpr]: ToExpr[AggregationColumn[T]] with
  override def apply(x: AggregationColumn[T])(using Quotes): Expr[AggregationColumn[T]] = x match
    case AggregationColumnAggregate =>
      '{ AggregationColumnAggregate }
    case AggregationColumnColumn(column) =>
      '{ AggregationColumnColumn(${ Expr(column) }) }

given ColumnToExpr[T: Type : ToExpr]: ToExpr[Column[T]] with
  override def apply(x: Column[T])(using Quotes): Expr[Column[T]] = x match
    case Constant(value) => '{ Constant(${ Expr(value) }) }
    case Index(index) => '{ Index(${ Expr(index) }) }

given AggregationFunctionToExpr: ToExpr[AggregationFunction] with
  override def apply(x: AggregationFunction)(using Quotes): Expr[AggregationFunction] = x match
    case Count => '{ Count }
    case Sum => '{ Sum }
    case Min => '{ Min }
    case Max => '{ Max }

given IndexToExpr: ToExpr[Index] with
  override def apply(x: Index)(using Quotes): Expr[Index] =
    '{ Index(${ Expr(x.index) }) }

given PredicateToExpr: ToExpr[Predicate] with
  override def apply(x: Predicate)(using Quotes): Expr[Predicate] =
    '{ Predicate(${ Expr(x.value) }) }

given PredicateWithArityToExpr: ToExpr[PredicateWithArity] with
  override def apply(x: PredicateWithArity)(using Quotes): Expr[PredicateWithArity] =
    '{ PredicateWithArity(${ Expr(x.predicate) }, ${ Expr(x.arity) }) }

given StorageManagerToExpr[T: Type : ToExpr]: ToExpr[StorageManager[T]] with
  override def apply(x: StorageManager[T])(using Quotes): Expr[StorageManager[T]] =
    '{ StorageManager[T]() }

extension (e: Expr.type)
  def ofSet[T](xs: Set[Expr[T]])(using Type[T])(using Quotes): Expr[Set[T]] =
    '{ Set(${ Varargs(xs.toSeq) }: _*) }
