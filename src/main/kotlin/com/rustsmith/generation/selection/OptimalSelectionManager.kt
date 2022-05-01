package com.rustsmith.generation.selection

import com.rustsmith.ast.*
import com.rustsmith.generation.Context
import kotlin.reflect.KClass

class OptimalSelectionManager : BaseSelectionManager() {

    override val config: Map<KClass<out ASTNode>, Int> = mapOf(
        RecursiveExpression::class to 100,
        FunctionCallExpression::class to 300,
        StructType::class to 300,
        TupleType::class to 300,
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

    override fun availableExpressionsWeightings(ctx: Context, type: Type): NodeSelectionWeighting<Expression> {
        val expressionWeightings = super.availableExpressionsWeightings(ctx, type)
        expressionWeightings.updateWeighting(
            RecursiveExpression::class,
            1.0 / (ctx.getDepth(RecursiveExpression::class) * 8 + 5)
        )
        expressionWeightings.updateWeighting(
            FunctionCallExpression::class,
            1.0 / (ctx.getDepth(FunctionCallExpression::class) * 4 + 10)
        )
        return expressionWeightings
    }

    override fun availableStatementsWeightings(ctx: Context): NodeSelectionWeighting<Statement> {
        val statementWeightings = super.availableStatementsWeightings(ctx)
        statementWeightings.updateWeighting(ExpressionStatement::class, 2.0)
        if (ctx.statementsPerScope.last().contains(ReturnStatement::class)) {
            statementWeightings.updateWeighting(ReturnStatement::class, 0.0)
        } else {
            statementWeightings.updateWeighting(ReturnStatement::class, 0.3)
        }

        if (ctx.statementsPerScope.last().contains(BreakStatement::class)) {
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
