package com.rustsmith.generation.selection

import com.rustsmith.ast.Expression
import com.rustsmith.ast.Statement
import com.rustsmith.ast.Type
import com.rustsmith.generation.Context
import kotlin.reflect.KClass

open class SwarmBasedSelectionManager(private val expressionConfiguration: List<KClass<out Expression>>) : BaseSelectionManager() {

    override fun availableExpressionsWeightings(ctx: Context, type: Type): NodeSelectionWeighting<Expression> {
        val expressionWeightings = super.availableExpressionsWeightings(ctx, type)
        /* Turn off certain expressions given in configuration */
        expressionConfiguration.forEach {
            expressionWeightings.removeWeighting(it)
        }
        return expressionWeightings
    }

    override fun availableStatementsWeightings(ctx: Context): NodeSelectionWeighting<Statement> {
        val statementWeightings = super.availableStatementsWeightings(ctx)

        return statementWeightings
    }

    override fun availableTypesWeightings(ctx: Context): NodeSelectionWeighting<Type> {
        val typeWeightings = super.availableTypesWeightings(ctx)

        return typeWeightings
    }
}
