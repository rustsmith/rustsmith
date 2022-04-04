package com.rustsmith.generation.selection

import com.rustsmith.ast.*
import com.rustsmith.generation.Context
import kotlin.reflect.KClass
import kotlin.reflect.full.findAnnotation
import kotlin.reflect.full.hasAnnotation
import kotlin.reflect.full.isSubclassOf

/**
 * Base Selection Manager where each option is associated with an equal weighting to choose from
 *
 */
open class BaseSelectionManager : SelectionManager {

    // Config describes what limit each specific ASTNode should have in terms of depth
    override val config: Map<KClass<out ASTNode>, Int> = mapOf(
        RecursiveExpression::class to 2,
        FunctionCallExpression::class to 3,
        TupleType::class to 3
    ).withDefault { Int.MAX_VALUE }

    /* Block size for statement blocks */
    override fun createNewStatementWeightings(ctx: Context) = mapOf(true to 0.7, false to 0.3)

    override fun availableStatementsWeightings(ctx: Context): SelectionWeighting<KClass<out Statement>> {
        val allStatements = Statement::class.subclasses().filter { it.hasAnnotation<GenNode>() }
        return filterNodes(allStatements.toMutableList(), ctx).associateWith { 1.0 }
    }

    override fun availableExpressionsWeightings(ctx: Context, type: Type): SelectionWeighting<KClass<out Expression>> {
        val allExpressions =
            Expression::class.subclasses().filter { it.hasAnnotation<ExpressionGenNode>() }.filter {
                it.findAnnotation<ExpressionGenNode>()?.compatibleType?.genSubClasses()
                    ?.contains(type::class) ?: false
            }
        return filterNodes(allExpressions.toMutableList(), ctx).associateWith { 1.0 }
    }

    override fun availableTypesWeightings(ctx: Context): Map<KClass<out Type>, Double> {
        return filterNodes(Type::class.genSubClasses().toMutableList(), ctx).associateWith { 1.0 }
    }

    private fun <T : ASTNode> filterNodes(
        nodes: MutableList<KClass<out T>>,
        ctx: Context
    ): List<KClass<out T>> {
        config.filterKeys { !it.isFinal }.forEach { entry ->
            val totalForSubclasses = entry.key.subclasses().sumOf { ctx.currentState.getValue(it) }
            if (totalForSubclasses >= entry.value) {
                val elements = nodes.filter { it.isSubclassOf(entry.key) }.toSet()
                nodes.removeAll(elements)
            }
        }
        nodes.removeIf { ctx.currentState.getValue(it) >= config.getValue(it) }
        return nodes
    }
}
