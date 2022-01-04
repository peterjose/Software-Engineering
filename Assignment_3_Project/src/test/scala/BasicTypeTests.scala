package de.uni_saarland.cs.se

import Expression.{_if, _let}
import ExpressionConversions.{constantToExpr, intToConst, stringToId}

import cafesat.api.{FormulaBuilder, Formulas}
import org.scalatest.flatspec.AnyFlatSpec

class BasicTypeTests extends AnyFlatSpec {
  "The expression 'Const(True)'" should "have the type 'BoolTy'" in {
    val testExpr: Expression = Const(True)
    val t = TypeChecker.checkType(
        testExpr,
        VariabilityContext.emptyContext(),
        TypeContext.emptyContext()
      )
    assert(t == new VType(Map(BoolTy -> Formulas.True)))
  }

  "The expression 'Const(False)'" should "have the type 'BoolTy'" in {
    val testExpr: Expression = Const(False)
    val t = TypeChecker.checkType(
      testExpr,
      VariabilityContext.emptyContext(),
      TypeContext.emptyContext()
    )
    assert(t == new VType(Map(BoolTy -> Formulas.True)))
  }

  "The expression 'Const(Num(42))'" should "have the type 'NumTy'" in {
    val testExpr: Expression = Const(Num(42))
    val t = TypeChecker.checkType(
      testExpr,
      VariabilityContext.emptyContext(),
      TypeContext.emptyContext()
    )
    assert(t == new VType(Map(NumTy -> Formulas.True)))
  }

  "The variable in 'x'" must "be in the context" in {
    val testExpr: Expression = Id("x")
    val error = intercept[TypeCheckingError](TypeChecker.checkType(
      testExpr,
      VariabilityContext.emptyContext(),
      TypeContext.emptyContext()
    ))
    assert(error.expr == testExpr)
    assert(error.typeContext == TypeContext.emptyContext())
    assert(error.variabilityContext == VariabilityContext.emptyContext())
  }

  "The arguments to 'Smaller'" must "have the type 'Num'" in {
    val testExpr: Expression = 5 _lt False
    val error = intercept[TypeCheckingError](TypeChecker.checkType(
      testExpr,
      VariabilityContext.emptyContext(),
      TypeContext.emptyContext()
    ))
    assert(error.expr == testExpr)
    assert(error.typeContext == TypeContext.emptyContext())
    assert(error.variabilityContext == VariabilityContext.emptyContext())
  }

  "The condition in 'If'" must "have the type 'BoolTy'" in {
    val testExpr: Expression = _if(3) _then True _else False
    val error = intercept[TypeCheckingError](TypeChecker.checkType(
      testExpr,
      VariabilityContext.emptyContext(),
      TypeContext.emptyContext()
    ))
    assert(error.expr == testExpr)
    assert(error.typeContext == TypeContext.emptyContext())
    assert(error.variabilityContext == VariabilityContext.emptyContext())
  }

  "The 'Id' in 'Let'" must "not be in the context already" in {
    val testExpr: Expression = _let("x") _is 5 _in ("x" _lt 4)
    val typeContext = TypeContext.emptyContext()
      .withVar("x", new VType(Map(NumTy -> Formulas.True)))
    val error = intercept[TypeCheckingError](TypeChecker.checkType(
      testExpr,
      VariabilityContext.emptyContext(),
      typeContext
    ))
    assert(error.expr == testExpr)
    assert(error.typeContext == typeContext)
    assert(error.variabilityContext == VariabilityContext.emptyContext())
  }

  "The expression 'Choice(A || B, True, 5)'" should "have a VType with two entries" in {
    val A = FormulaBuilder.propVar("A")
    val B = FormulaBuilder.propVar("B")
    val testExpr: Expression = Choice(A || B, True, 5)
    val t = TypeChecker.checkType(
      testExpr,
      VariabilityContext.emptyContext(),
      TypeContext.emptyContext()
    )
    assert(t == new VType(Map(
      BoolTy -> (Formulas.True && (A || B)),
      NumTy -> (Formulas.True && !(A || B))
    )))
  }
}
