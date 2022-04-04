package com.rustsmith.generation.selection

import com.rustsmith.ast.AddExpression
import com.rustsmith.ast.Expression
import com.rustsmith.ast.Type
import com.rustsmith.generation.Context
import kotlin.reflect.KClass

class OptimalSelectionManager : BaseSelectionManager() {

    override fun createNewStatementWeightings(ctx: Context): Map<Boolean, Double> {
        val newStatementWeightings = super.createNewStatementWeightings(ctx).toMutableMap()
        newStatementWeightings[true] = 10.0 / (ctx.numberOfDeclarationsInScope.value + 1)
        return newStatementWeightings
    }

    override fun availableExpressionsWeightings(ctx: Context, type: Type): SelectionWeighting<KClass<out Expression>> {
        val expressionWeightings = super.availableExpressionsWeightings(ctx, type).toMutableMap()
        if (AddExpression::class in expressionWeightings) {
            expressionWeightings[AddExpression::class] =
                1.0 / (ctx.currentState.getValue(AddExpression::class) + 1)
        }
        return expressionWeightings
    }
}
