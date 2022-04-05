package com.rustsmith.generation.selection

import com.rustsmith.ast.*
import com.rustsmith.generation.Context
import kotlin.reflect.KClass

class OptimalSelectionManager : BaseSelectionManager() {

    override val config: Map<KClass<out ASTNode>, Int> = mapOf(
        RecursiveExpression::class to 100,
        FunctionCallExpression::class to 300,
        StructType::class to 300,
        TupleType::class to 3
    ).withDefault { Int.MAX_VALUE }

    override fun createNewStatementWeightings(ctx: Context): Map<Boolean, Double> {
        val newStatementWeightings = super.createNewStatementWeightings(ctx).toMutableMap()
        newStatementWeightings[true] = 50.0 / (ctx.numberOfDeclarationsInScope.value + 1)
        return newStatementWeightings
    }

    override fun availableExpressionsWeightings(ctx: Context, type: Type): NodeSelectionWeighting<Expression> {
        val expressionWeightings = super.availableExpressionsWeightings(ctx, type)
        expressionWeightings.updateWeighting(
            RecursiveExpression::class,
            1.0 / (ctx.getDepth(RecursiveExpression::class) + 100)
        )
        expressionWeightings.updateWeighting(
            FunctionCallExpression::class,
            1.0 / (ctx.getDepth(FunctionCallExpression::class) + 10)
        )
        return expressionWeightings
    }

    override fun availableTypesWeightings(ctx: Context): NodeSelectionWeighting<Type> {
        val typeWeightings = super.availableTypesWeightings(ctx)
        typeWeightings.updateWeighting(StructType::class, 1.0 / (ctx.getDepth(StructType::class) + 1))
        return typeWeightings
    }
}
