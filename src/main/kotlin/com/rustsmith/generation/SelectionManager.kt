package com.rustsmith.generation

import com.rustsmith.ast.*
import kotlin.reflect.KClass
import kotlin.reflect.full.findAnnotation
import kotlin.reflect.full.hasAnnotation
import kotlin.reflect.full.isSubclassOf

// Config describes what limit each specific ASTNode should have in terms of depth
val config: Map<KClass<out ASTNode>, Int> =
    mapOf<KClass<out ASTNode>, Int>(RecursiveExpression::class to 2, FunctionCallExpression::class to 1, TupleType::class to 3).withDefault { Int.MAX_VALUE }

class SelectionManager(state: Map<KClass<out ASTNode>, Int>) {

    private val currentState: Map<KClass<out ASTNode>, Int>

    init {
        currentState = state.toMutableMap().withDefault { 0 }
    }

    fun incrementCount(kClass: KClass<out ASTNode>): SelectionManager {
        val stateCopy = currentState.toMutableMap().withDefault { 0 }
        stateCopy[kClass] = stateCopy.getValue(kClass) + 1
        return SelectionManager(stateCopy)
    }

    private fun <T : ASTNode> filterNodes(nodes: MutableList<KClass<out T>>): List<KClass<out T>> {
        config.filterKeys { !it.isFinal }.forEach { entry ->
            val totalForSubclasses = entry.key.subclasses().sumOf { currentState.getValue(it) }
            if (totalForSubclasses >= entry.value) {
                val elements = nodes.filter { it.isSubclassOf(entry.key) }.toSet()
                nodes.removeAll(elements)
            }
        }
        nodes.removeIf { currentState.getValue(it) >= config.getValue(it) }
        return nodes
    }

    fun availableTypes() = Type::class.genSubClasses()

    fun availableStatements(): List<KClass<out Statement>> {
        val allStatements = Statement::class.subclasses().filter { it.hasAnnotation<GenNode>() } +
            ExpressionAndStatement::class.subclasses().filter { it.hasAnnotation<ExpressionGenNode>() }
        return filterNodes(allStatements.toMutableList())
    }

    fun availableExpressions(type: Type): List<KClass<out Expression>> {
        val allExpressions = Expression::class.subclasses().filter { it.hasAnnotation<ExpressionGenNode>() }.filter {
            it.findAnnotation<ExpressionGenNode>()?.compatibleType?.genSubClasses()?.contains(type::class) ?: false
        }
        return filterNodes(allExpressions.toMutableList())
    }
}
