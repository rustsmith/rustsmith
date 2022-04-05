package com.rustsmith.generation.selection

import com.rustsmith.ast.*
import com.rustsmith.generation.Context
import kotlin.reflect.KClass

class OptimalSelectionManager : BaseSelectionManager() {

    override val config: Map<KClass<out ASTNode>, Int> = mapOf(
        RecursiveExpression::class to 100,
        FunctionCallExpression::class to 3,
        TupleType::class to 3
    ).withDefault { Int.MAX_VALUE }

    override fun createNewStatementWeightings(ctx: Context): Map<Boolean, Double> {
        val newStatementWeightings = super.createNewStatementWeightings(ctx).toMutableMap()
        newStatementWeightings[true] = 30.0 / (ctx.numberOfDeclarationsInScope.value + 1)
        return newStatementWeightings
    }

    override fun availableExpressionsWeightings(ctx: Context, type: Type): NodeSelectionWeighting<Expression> {
        val expressionWeightings = super.availableExpressionsWeightings(ctx, type)
        expressionWeightings.updateWeighting(
            RecursiveExpression::class,
            1.0 / (ctx.getDepth(RecursiveExpression::class) + 100)
        )
        return expressionWeightings
    }
}
