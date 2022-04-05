package com.rustsmith.generation

import com.rustsmith.ast.ASTNode
import com.rustsmith.ast.SymbolTable
import com.rustsmith.subclasses
import kotlin.reflect.KClass

data class Context(
    private val nodeDepthState: Map<KClass<out ASTNode>, Int>,
    val statementsPerScope: List<Int>,
    val symbolTable: SymbolTable
) {
    val numberOfDeclarationsLocal = lazy { symbolTable.getLocalVariables().size }
    val numberOfDeclarationsInScope = lazy { symbolTable.getCurrentVariables().size }

    fun getDepth(kClass: KClass<out ASTNode>): Int {
        return kClass.subclasses().sumOf { nodeDepthState[it] ?: 0 }
    }

    fun withSymbolTable(symbolTable: SymbolTable): Context {
        val stateCopy = nodeDepthState.toMutableMap().withDefault { 0 }
        return Context(stateCopy, statementsPerScope.toMutableList(), symbolTable)
    }

    fun incrementCount(kClass: KClass<out ASTNode>): Context {
        val stateCopy = nodeDepthState.toMutableMap().withDefault { 0 }
        stateCopy[kClass] = stateCopy.getValue(kClass) + 1
        return Context(stateCopy, statementsPerScope.toMutableList(), symbolTable)
    }

    fun incrementStatementCount(): Context {
        val stateCopy = nodeDepthState.toMutableMap().withDefault { 0 }
        val statementDepthCopy = statementsPerScope.toMutableList()
        statementDepthCopy[statementDepthCopy.lastIndex]++
        return Context(stateCopy, statementDepthCopy, symbolTable)
    }

    fun enterScope(): Context {
        val stateCopy = nodeDepthState.toMutableMap().withDefault { 0 }
        return Context(stateCopy, listOf(*statementsPerScope.toTypedArray(), 0), symbolTable)
    }
}
