package com.rustsmith.generation.selection

import com.rustsmith.ast.*
import com.rustsmith.generation.Context
import com.rustsmith.subclasses
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
    override fun choiceGenerateNewStatementWeightings(ctx: Context) = mapOf(true to 0.7, false to 0.3)

    /* Both false as default. Eg: never create a new struct or function if a struct already exists or a function
       with the return type required already exists
     */
    override fun choiceGenerateNewStructWeightings(ctx: Context): Map<Boolean, Double> = mapOf(false to 1.0)

    override fun choiceGenerateNewTupleWeightings(ctx: Context): Map<Boolean, Double> = mapOf(false to 1.0)

    override fun choiceGenerateNewFunctionWeightings(ctx: Context): Map<Boolean, Double> = mapOf(false to 0.5, true to 0.5)

    override fun choiceGenerateNewCLIArgumentWeightings(ctx: Context): Map<Boolean, Double> = mapOf(false to 1.0)

    override fun availableStatementsWeightings(ctx: Context): NodeSelectionWeighting<Statement> {
        val allStatements = Statement::class.subclasses().filter { it.hasAnnotation<GenNode>() }
        val filteredStatements = filterNodes(allStatements.toMutableList(), ctx).associateWith { 1.0 }.toMutableMap()
        if (ctx.returnExpressionType == null) {
            filteredStatements.remove(ReturnStatement::class)
        }

        if (ctx.returnLoopType == null) {
            filteredStatements.remove(BreakStatement::class)
        }
        return NodeSelectionWeighting(filteredStatements)
    }

    override fun availableExpressionsWeightings(ctx: Context, type: Type): NodeSelectionWeighting<Expression> {
        val allExpressions =
            Expression::class.subclasses().filter { it.hasAnnotation<ExpressionGenNode>() }.filter {
                it.findAnnotation<ExpressionGenNode>()?.compatibleType?.genSubClasses()
                    ?.contains(type::class) ?: false
            }
        val filteredExpressions = filterNodes(allExpressions.toMutableList(), ctx).associateWith { 1.0 }.toMutableMap()
        if (ctx.currentFunctionName != "main") {
            filteredExpressions.remove(CLIArgumentAccessExpression::class)
        }
        return NodeSelectionWeighting(filteredExpressions)
    }

    override fun availableTypesWeightings(ctx: Context): NodeSelectionWeighting<Type> {
        val allTypes =
            filterNodes(Type::class.genSubClasses().toMutableList(), ctx).associateWith { 1.0 }.toMutableMap()
        return NodeSelectionWeighting(allTypes)
    }

    private fun <T : ASTNode> filterNodes(
        nodes: MutableList<KClass<out T>>,
        ctx: Context
    ): List<KClass<out T>> {
        config.filterKeys { !it.isFinal }.forEach { entry ->
            val totalForSubclasses = entry.key.subclasses().sumOf { ctx.getDepth(it) }
            if (totalForSubclasses >= entry.value) {
                val elements = nodes.filter { it.isSubclassOf(entry.key) }.toSet()
                nodes.removeAll(elements)
            }
        }
        nodes.removeIf { ctx.getDepth(it) >= config.getValue(it) }
        return nodes
    }
}
