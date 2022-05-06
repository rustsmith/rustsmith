package com.rustsmith.generation.selection

import com.rustsmith.ast.*
import com.rustsmith.generation.Context
import kotlin.reflect.KClass

open class OptimalSelectionManager(expressionConfiguration: List<KClass<out Expression>>) :
    SwarmBasedSelectionManager(expressionConfiguration) {

    override val config: Map<KClass<out ASTNode>, Int> = mapOf(
        RecursiveExpression::class to 5,
        FunctionCallExpression::class to 5,
        StructType::class to 5,
        TupleType::class to 5,
    ).withDefault { Int.MAX_VALUE }

    override fun choiceGenerateNewStatementWeightings(ctx: Context): Map<Boolean, Double> {
        val newStatementWeightings = super.choiceGenerateNewStatementWeightings(ctx).toMutableMap()
        newStatementWeightings[true] = 10.0 / (ctx.statementsPerScope.last().size + 1)
        newStatementWeightings[false] = 1 - newStatementWeightings[true]!!
        return newStatementWeightings
    }

    override fun choiceGenerateNewStructWeightings(ctx: Context): Map<Boolean, Double> {
        val createNewStruct = 1.0 / (ctx.numberOfStructsDefined.value + 1)
        return mapOf(true to createNewStruct, false to 1 - createNewStruct)
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
            .count { it is ExpressionStatement && it.expression is RecursiveExpression } * 2 + 1
        expressionWeightings.updateWeighting(
            RecursiveExpression::class,
            1.0 / (ctx.getDepth(RecursiveExpression::class) * 8 + currentRecursiveExpressions)
        )
        expressionWeightings.updateWeighting(
            FunctionCallExpression::class,
            1.0 / (ctx.getDepth(FunctionCallExpression::class) * 8 + 1)
        )
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
        return statementWeightings
    }

    override fun availableTypesWeightings(ctx: Context): NodeSelectionWeighting<Type> {
        val typeWeightings = super.availableTypesWeightings(ctx)
        typeWeightings.updateWeighting(StructType::class, 1.0 / (ctx.getDepth(StructType::class) + 1))
        typeWeightings.updateWeighting(TupleType::class, 1.0 / (ctx.getDepth(TupleType::class) + 1))
        if (ctx.previousIncrement == ExpressionStatement::class) {
            typeWeightings.updateWeighting(VoidType::class, 2.0)
        } else {
            typeWeightings.updateWeighting(VoidType::class, 0.0)
        }
        return typeWeightings
    }
}
