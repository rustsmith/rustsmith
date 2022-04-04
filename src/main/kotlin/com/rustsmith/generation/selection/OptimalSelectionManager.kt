package com.rustsmith.generation.selection

import com.rustsmith.ast.AddExpression
import com.rustsmith.ast.Expression
import com.rustsmith.ast.Type
import com.rustsmith.generation.Context
import kotlin.reflect.KClass

class OptimalSelectionManager: BaseSelectionManager() {

    override fun availableExpressionsWeightings(ctx: Context, type: Type): SelectionWeighting<KClass<out Expression>> {
        val expressionWeightings = super.availableExpressionsWeightings(ctx, type).toMutableMap()
        expressionWeightings[AddExpression::class] = 1.0/(ctx.currentState.getValue(AddExpression::class))
        return expressionWeightings
    }

}