package com.rustsmith.generation.selection

import com.rustsmith.CustomRandom
import com.rustsmith.ast.*
import com.rustsmith.generation.Context
import kotlin.reflect.KClass
import kotlin.reflect.full.findAnnotation
import kotlin.reflect.full.isSubclassOf

open class AggressiveSelectionManager(
    private val aggressiveNode: KClass<out ASTNode>,
    private val weighting: Double = 20.0
) : OptimalSelectionManager() {

    private val randomType: KClass<out Type>? = if (aggressiveNode.isSubclassOf(Expression::class)) {
        aggressiveNode.findAnnotation<ExpressionGenNode>()!!.compatibleType.genSubClasses().random(CustomRandom)
    } else {
        null
    }

    override fun availableStatementsWeightings(ctx: Context): NodeSelectionWeighting<Statement> {
        val weightings = super.availableStatementsWeightings(ctx)
        if (aggressiveNode.isSubclassOf(Statement::class)) {
            weightings.updateWeighting(
                aggressiveNode as KClass<out Statement>,
                weighting * (weightings.weightings[aggressiveNode] ?: 1.0)
            )
        }
        return weightings
    }

    override fun availableTypesWeightings(ctx: Context): NodeSelectionWeighting<Type> {
        val weightings = super.availableTypesWeightings(ctx)
        if (aggressiveNode.isSubclassOf(Type::class) && ctx.getDepthLast(RecursiveExpression::class) > 0) {
            weightings.updateWeighting(
                aggressiveNode as KClass<out Type>,
                weighting * (weightings.weightings[aggressiveNode] ?: 1.0)
            )
        }

        if (randomType != null) {
            weightings.updateWeighting(randomType, weighting * (weightings.weightings[randomType] ?: 1.0))
        }
        return weightings
    }

    override fun availableExpressionsWeightings(ctx: Context, type: Type): NodeSelectionWeighting<Expression> {
        val weightings = super.availableExpressionsWeightings(ctx, type)
        if (aggressiveNode.isSubclassOf(Expression::class)) {
            weightings.updateWeighting(
                aggressiveNode as KClass<out Expression>,
                weighting * (weightings.weightings[aggressiveNode] ?: 1.0)
            )
        }
        return weightings
    }
}
