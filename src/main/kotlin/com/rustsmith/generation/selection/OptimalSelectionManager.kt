package com.rustsmith.generation.selection

import com.rustsmith.ast.*
import com.rustsmith.generation.Context
import com.rustsmith.logging.Logger
import kotlin.reflect.KClass

open class OptimalSelectionManager : BaseSelectionManager() {

    override val config: MutableMap<KClass<out ASTNode>, Int> = mutableMapOf(
        FunctionCallExpression::class to 2,
        RecursiveExpression::class to 5,
        StructType::class to 5,
        TupleType::class to 5,
        Variable::class to 1,
        ArrayLiteral::class to 5
    ).withDefault { Int.MAX_VALUE }

    override fun choiceGenerateNewStatementWeightings(ctx: Context): Map<Boolean, Double> {
        val newStatementWeightings = super.choiceGenerateNewStatementWeightings(ctx).toMutableMap()
        newStatementWeightings[true] = 30.0 / (ctx.statementsPerScope.last().size + 1)
        newStatementWeightings[false] = 1 - newStatementWeightings[true]!!
        return newStatementWeightings
    }

    override fun choiceGenerateNewStructWeightings(ctx: Context): Map<Boolean, Double> {
        val createNewStruct = 1.0 / (ctx.numberOfStructsDefined.value + 1)
        return mapOf(true to createNewStruct, false to 1.0 - createNewStruct)
    }

    override fun choiceGenerateNewTupleWeightings(ctx: Context): Map<Boolean, Double> {
        val createNewTuple = 1.0 / (ctx.numberOfTuplesDefined.value + 1)
        return mapOf(true to createNewTuple, false to 1 - createNewTuple)
    }

    override fun choiceGenerateNewFunctionWeightings(ctx: Context): Map<Boolean, Double> {
        val createNewFunction = 1.0 / (ctx.numberOfFunctionsDefined.value + 1)
        return mapOf(true to createNewFunction, false to 1 - createNewFunction)
    }

    override fun choiceGenerateNewCLIArgumentWeightings(ctx: Context): Map<Boolean, Double> =
        mapOf(true to 0.1, false to 0.9)

    override fun availableExpressionsWeightings(ctx: Context, type: Type): NodeSelectionWeighting<Expression> {
        val expressionWeightings = super.availableExpressionsWeightings(ctx, type)
        val currentRecursiveExpressions = ctx.statementsPerScope.last()
            .count { it is ExpressionStatement && it.expression is RecursiveExpression } + 1
        expressionWeightings.updateWeighting(
            RecursiveExpression::class,
            1.0 / (ctx.getDepth(RecursiveExpression::class) * 4 + currentRecursiveExpressions)
        )
        expressionWeightings.updateWeighting(
            ArrayLengthExpression::class,
            1.0 / (ctx.getDepth(ArrayLengthExpression::class) + 1)
        )
        expressionWeightings.updateWeighting(
            ArrayLiteral::class,
            1.0 / (ctx.getDepth(ArrayLiteral::class) + 1)
        )
        expressionWeightings.updateWeighting(
            FunctionCallExpression::class,
            1.0 / (ctx.getDepth(FunctionCallExpression::class) * 4 + (1 + ctx.numberOfFunctionsDefined.value))
        )
        Logger.logText("Weightings for expressions ${expressionWeightings.weightings}", ctx)
        return expressionWeightings
    }

    override fun availableStatementsWeightings(ctx: Context): NodeSelectionWeighting<Statement> {
        val statementWeightings = super.availableStatementsWeightings(ctx)
        statementWeightings.updateWeighting(ExpressionStatement::class, 2.0)
        if (ctx.statementsPerScope.last().count { it is ReturnStatement } > 0) {
            statementWeightings.updateWeighting(ReturnStatement::class, 0.0)
        } else {
            statementWeightings.updateWeighting(ReturnStatement::class, 0.3)
        }

        if (ctx.statementsPerScope.last().count { it is BreakStatement } > 0) {
            statementWeightings.updateWeighting(BreakStatement::class, 0.0)
        } else {
            statementWeightings.updateWeighting(BreakStatement::class, 0.3)
        }
        Logger.logText("Weightings for statements ${statementWeightings.weightings}", ctx)
        return statementWeightings
    }

    override fun availableTypesWeightings(ctx: Context): NodeSelectionWeighting<Type> {
        val typeWeightings = super.availableTypesWeightings(ctx)
        typeWeightings.updateWeighting(ContainerType::class, 2.0 / (ctx.getDepth(ContainerType::class) + 1))
        typeWeightings.updateWeighting(TypeAliasType::class, 1.0 / (ctx.getDepth(RecursiveType::class) + 5))
        typeWeightings.updateWeighting(ReferenceType::class, 1.0 / (ctx.getDepth(ReferenceType::class) + 1))
        if (ctx.previousIncrement == ExpressionStatement::class) {
            typeWeightings.updateWeighting(VoidType::class, 2.0)
        } else {
            typeWeightings.updateWeighting(VoidType::class, 0.0)
        }
        Logger.logText("Weightings for types ${typeWeightings.weightings}", ctx)
        return typeWeightings
    }

    override fun choiceGenerateNewTypeAliasWeightings(ctx: Context): Map<Boolean, Double> {
        val createNewAlias = 1.0 / (ctx.numberOfTypeAliasesDefined.value + 1)
        return mapOf(true to createNewAlias, false to 1.0 - createNewAlias)
    }
}
