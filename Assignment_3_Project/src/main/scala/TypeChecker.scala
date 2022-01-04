package de.uni_saarland.cs.se

import cafesat.api.Formulas.Formula
import cafesat.api.{Formulas, Solver}

/**
  * Type context as in lecture slides 8/73.
  */
class TypeContext private(val mapping: Map[Id, VType]) {
  def this() = {
    this(Map())
  }

  /**
    * Create a extended copy of this type context that sets the type for the
    * given variable.
    */
  def withVar(id: Id, value: VType): TypeContext = {
    new TypeContext(mapping updated(id, value))
  }

  /**
    * Get the type for a given variable.
    */
  def typeForVar(id: Id): Option[VType] = mapping.get(id)

  override def equals(obj: Any): Boolean = {
    obj match {
      case other: TypeContext => mapping == other.mapping
      case _ => false
    }
  }
}

object TypeContext {
  /**
    * Creates an empty type context.
    */
  def emptyContext(): TypeContext = new TypeContext(Map())
}

/**
  * Variability context as in lecture slides 8/73.
  */
class VariabilityContext(val featureModel: Formula) {
  override def equals(obj: Any): Boolean = {
    obj match {
      case other: VariabilityContext => featureModel == other.featureModel
      case _ => false
    }
  }
}

object VariabilityContext {
  /**
    * Creates an empty variability context.
    */
  def emptyContext(): VariabilityContext = new VariabilityContext(Formulas.True)
}

/**
  * This exception must be thrown if a type error is detected.
  *
  * @param expr the (sub) expression that is currently checked
  * @param variabilityContext the current variability context
  * @param typeContext the current type context
  * @param message a message describing the type error
  */
class TypeCheckingError(
  val expr: Expression,
  val variabilityContext: VariabilityContext,
  var typeContext: TypeContext,
  message: String
) extends Exception(message)


object TypeChecker {
  /**
    * Determine the type of an expression given some type and variability
    * context.
    *
    * This function handles the dispatch for the different expression types.
    * You only have to implement the individual type rules as indicated by
    * the TODOs below.
    */
  def checkType(
                 expr: Expression,
                 variabilityContext: VariabilityContext,
                 typeContext: TypeContext
               ): VType = {
    expr match {
      case expr: Const => expr.c match {
        case True => checkTTrue(variabilityContext, typeContext);
        case False => checkTFalse(variabilityContext, typeContext);
        case const: Num => checkTNum(const, variabilityContext, typeContext);
      };
      case expr: Id => checkTId(expr, variabilityContext, typeContext);
      case expr: Smaller => checkTSmaller(expr, variabilityContext, typeContext);
      case expr: If => checkTIf(expr, variabilityContext, typeContext);
      case expr: Let => checkTLet(expr, variabilityContext, typeContext);
      case expr: Choice => checkTChoice(expr, variabilityContext, typeContext);
    }
  }

  def checkTTrue(
                  variabilityContext: VariabilityContext,
                  typeContext: TypeContext
                ): VType = {
    // TODO: Implement T-True
  }

  def checkTFalse(
                   variabilityContext: VariabilityContext,
                   typeContext: TypeContext
                 ): VType = {
    // TODO: Implement T-False
  }

  def checkTNum(
                 const: Num,
                 variabilityContext: VariabilityContext,
                 typeContext: TypeContext
               ): VType = {
    // TODO: Implement T-Num
  }

  def checkTId(
                expr: Id,
                variabilityContext: VariabilityContext,
                typeContext: TypeContext
              ): VType = {
    // TODO: Implement T-Id
  }

  def checkTSmaller(
                     expr: Smaller,
                     variabilityContext: VariabilityContext,
                     typeContext: TypeContext
                   ): VType = {
    // TODO: Implement T-Smaller
  }

  def checkTIf(
                expr: If,
                variabilityContext: VariabilityContext,
                typeContext: TypeContext
              ): VType = {
    // TODO: Implement T-If
  }

  def checkTLet(
                 expr: Let,
                 variabilityContext: VariabilityContext,
                 typeContext: TypeContext
               ): VType = {
    // TODO: Implement T-Let
  }

  def checkTChoice(
                    expr: Choice,
                    variabilityContext: VariabilityContext,
                    typeContext: TypeContext
                  ): VType = {
    // TODO: Implement T-Choice
  }
}
