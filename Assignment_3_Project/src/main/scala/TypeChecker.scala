package de.uni_saarland.cs.se

import cafesat.api
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
    new VType (Map(BoolTy -> variabilityContext.featureModel))
   }

  def checkTFalse(
                   variabilityContext: VariabilityContext,
                   typeContext: TypeContext
                 ): VType = {

    new VType (Map(BoolTy -> variabilityContext.featureModel))
  }

  def checkTNum(
                 const: Num,
                 variabilityContext: VariabilityContext,
                 typeContext: TypeContext
               ): VType = {
    new VType (Map(NumTy -> variabilityContext.featureModel))
  }

  def checkTId(
                expr: Id,
                variabilityContext: VariabilityContext,
                typeContext: TypeContext
              ): VType = {
    if(typeContext.typeForVar(expr).isEmpty){
      throw new TypeCheckingError(expr = expr, variabilityContext = variabilityContext, typeContext = typeContext,
        message = "Error: Id not present in the context");
    }

    var formula_map : Map[Type, Formula] = Map()
    var formula_ : Formula = null
    for(type_ <- typeContext.typeForVar(expr).get.dom()){
      formula_ = formula_ || typeContext.typeForVar(expr).get.formulaForType(type_).get
      formula_map += (type_ -> (typeContext.typeForVar(expr).get.formulaForType(type_).get && variabilityContext.featureModel ))
    }
    Solver.solveForSatisfiability(!variabilityContext.featureModel || formula_) match {
      case None => {
        println("Error: ")
        throw new TypeCheckingError(expr = expr, variabilityContext = variabilityContext, typeContext = typeContext, message = "Error:");
      }
      case Some(_) => println("success")
    }
    new VType (formula_map)
  }

  def checkTSmaller(
                     expr: Smaller,
                     variabilityContext: VariabilityContext,
                     typeContext: TypeContext
                   ): VType = {

    if(checkType(expr.lhs, variabilityContext, typeContext).equals(
        new VType(Map(NumTy -> variabilityContext.featureModel)))){
      // using sat solver for si implies phi for e1
      Solver.solveForSatisfiability(!variabilityContext.featureModel ||
        checkType(expr.lhs, variabilityContext, typeContext).formulaForType(NumTy).get) match {
        case None => {
          println("Error: psi NOT implies phi")
          throw new TypeCheckingError(expr = expr, variabilityContext = variabilityContext, typeContext = typeContext,
            message = "Error: psi NOT implies phi");
        }
        case Some(_) => if (checkType(expr.rhs, variabilityContext, typeContext).equals(
          new VType(Map(NumTy -> variabilityContext.featureModel)))) {
          // using sat solver for si implies phi for e2
          Solver.solveForSatisfiability(!variabilityContext.featureModel ||
            checkType(expr.rhs, variabilityContext, typeContext).formulaForType(NumTy).get) match {
            case None => {
              println("Error: psi NOT implies phi")
              throw new TypeCheckingError(expr = expr, variabilityContext = variabilityContext, typeContext = typeContext, message = "Error: psi NOT implies phi");
            }
            case Some(_) => {
              println("Success")
              new VType(Map(BoolTy -> variabilityContext.featureModel));
            }
          }
        }else{
            println("Error: Expression RHS not NumTy")
            throw new TypeCheckingError(expr = expr,variabilityContext = variabilityContext, typeContext = typeContext, message = "Error: Expression RHS not NumTy")
          };
      }
    }
    else{
      println("Error: Expression LHS not NumTy")
      throw new TypeCheckingError(expr = expr,variabilityContext = variabilityContext, typeContext = typeContext, message = "Error: Expression LHS not NumTy")
    }
    new VType(Map(BoolTy -> variabilityContext.featureModel))
  }

  def checkTIf(
                expr: If,
                variabilityContext: VariabilityContext,
                typeContext: TypeContext
              ): VType = {

    if(! checkType(expr.condition, variabilityContext, typeContext).equals(
      new VType(Map(BoolTy -> variabilityContext.featureModel)))){
      throw new TypeCheckingError(expr = expr, variabilityContext = variabilityContext, typeContext = typeContext,
        message = "Error: BoolTy is not the variable");
    }
//    Solver.solveForSatisfiability(!variabilityContext.featureModel) match {
//      case None => {
//        println("Error: psi NOT implies phi")
//        throw new TypeCheckingError(expr = expr, variabilityContext = variabilityContext, typeContext = typeContext, message = "Error: psi NOT implies phi");
//      }
//      case Some(_) => {
//        println("Success")
//        new VType(Map(BoolTy -> variabilityContext.featureModel));
//      }
//    }
    new VType (Map(BoolTy -> variabilityContext.featureModel))
  }

  def checkTLet(
                 expr: Let,
                 variabilityContext: VariabilityContext,
                 typeContext: TypeContext
               ): VType = {
    if(!typeContext.typeForVar(expr.varName).isEmpty){
      throw new TypeCheckingError(expr = expr, variabilityContext = variabilityContext, typeContext = typeContext,
        message = "Error: Id not present in the context");
    }
    val t_1 = checkType(expr.varValue,variabilityContext,typeContext)
    var formula_1 : Formula = null
    for(type_ <- t_1.dom()){
      formula_1 = formula_1 || t_1.formulaForType(type_).get
    }
    Solver.solveForSatisfiability(!variabilityContext.featureModel || formula_1) match {
      case None => {
        println("Error: ")
        throw new TypeCheckingError(expr = expr, variabilityContext = variabilityContext, typeContext = typeContext, message = "Error:");
      }
      case Some(_) => println("OK")
    }
    val t_2 = checkType(expr.inExpr,variabilityContext,typeContext.withVar(expr.varName,t_1))
    var formula_2 : Formula = null
    for(type_ <- t_2.dom()){
      formula_2 = formula_2 || t_2.formulaForType(type_).get
    }
    Solver.solveForSatisfiability(!variabilityContext.featureModel || formula_2) match {
      case None => {
        println("Error: ")
        throw new TypeCheckingError(expr = expr, variabilityContext = variabilityContext, typeContext = typeContext, message = "Error:");
      }
      case Some(_) => println("OK")
    }
    t_2
  }

  def checkTChoice(
                    expr: Choice,
                    variabilityContext: VariabilityContext,
                    typeContext: TypeContext
                  ): VType = {
    // create Thau 1 and Thau 2
    val t1_type = checkType(expr.trueChoice,new VariabilityContext(variabilityContext.featureModel && expr.presenceCondition),
        typeContext)
    val t2_type = checkType(expr.falseChoice,new VariabilityContext(variabilityContext.featureModel && ! expr.presenceCondition),
        typeContext)

    var formula_map : Map[Type, Formula] = Map()
    for(type_ <- t1_type.dom() diff t2_type.dom()){
      formula_map += (type_ -> t1_type.formulaForType(type_).get)
    }
    for(type_ <- t2_type.dom() diff t1_type.dom()){
      formula_map += (type_ -> t2_type.formulaForType(type_).get)
    }
    for(type_ <- t1_type.dom() intersect t2_type.dom()){
      formula_map += (type_ -> (t1_type.formulaForType(type_).get || t2_type.formulaForType(type_).get))
    }
    new VType(formula_map)
  }
}
