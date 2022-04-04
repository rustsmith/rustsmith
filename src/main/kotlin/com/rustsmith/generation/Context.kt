package com.rustsmith.generation

import com.rustsmith.ast.ASTNode
import com.rustsmith.ast.SymbolTable
import kotlin.reflect.KClass

data class Context(
    val state: Map<KClass<out ASTNode>, Int>,
    val statementDepth: List<Int>,
    val symbolTable: SymbolTable
) {
    val numberOfDeclarationsLocal = lazy { symbolTable.getLocalVariables().size }
    val numberOfDeclarationsInScope = lazy { symbolTable.getCurrentVariables().size }

    val currentState: Map<KClass<out ASTNode>, Int> = state.toMutableMap().withDefault { 0 }

    fun withSymbolTable(symbolTable: SymbolTable): Context {
        val stateCopy = currentState.toMutableMap().withDefault { 0 }
        return Context(stateCopy, statementDepth.toMutableList(), symbolTable)
    }

    fun incrementCount(kClass: KClass<out ASTNode>): Context {
        val stateCopy = currentState.toMutableMap().withDefault { 0 }
        stateCopy[kClass] = stateCopy.getValue(kClass) + 1
        return Context(stateCopy, statementDepth.toMutableList(), symbolTable)
    }

    fun incrementStatementCount(): Context {
        val stateCopy = currentState.toMutableMap().withDefault { 0 }
        val statementDepthCopy = statementDepth.toMutableList()
        statementDepthCopy[statementDepthCopy.lastIndex]++
        return Context(stateCopy, statementDepthCopy, symbolTable)
    }

    fun enterScope(): Context {
        val stateCopy = currentState.toMutableMap().withDefault { 0 }
        return Context(stateCopy, listOf(*statementDepth.toTypedArray(), 0), symbolTable)
    }
}
