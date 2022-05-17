package com.rustsmith.generation

import com.rustsmith.ast.*
import com.rustsmith.subclasses
import kotlin.reflect.KClass

data class Context(
    val nodeDepthState: List<Map<KClass<out ASTNode>, Int>>,
    val currentFunctionName: String,
    val statementsPerScope: List<List<Statement>>,
    val symbolTable: SymbolTable,
    val requiredType: Type? = null,
    val returnExpressionType: Type? = null,
    val returnLoopType: Type? = null,
    val previousIncrement: KClass<out ASTNode>? = null,
    val assignmentRootNode: List<Variable>? = null,
    val currentLifetimeParameterNumber: Int? = null
) {
    val numberOfDeclarationsLocal = lazy { symbolTable.getLocalVariables().size }
    val numberOfDeclarationsInScope = lazy { symbolTable.getCurrentVariables().size }
    val numberOfFunctionsDefined = lazy { symbolTable.functionSymbolTable.functions.size }
    val numberOfStructsDefined = lazy { symbolTable.globalSymbolTable.structs.size }
    val numberOfTuplesDefined = lazy { symbolTable.globalSymbolTable.tupleTypes.size }

    fun incrementLifetimeParameterNumber(): Context {
        return this.copy(currentLifetimeParameterNumber = currentLifetimeParameterNumber?.inc() ?: 1)
    }

    fun withAssignmentNode(variable: Variable?): Context {
        if (variable != null) {
            return this.copy(assignmentRootNode = listOf(*(assignmentRootNode?.toTypedArray() ?: arrayOf()), variable))
        }
        return this
    }

    fun setRequiredType(type: Type): Context {
        return this.copy(requiredType = type)
    }

    fun setReturnExpressionType(type: Type): Context {
        return this.copy(returnExpressionType = type)
    }

    fun setLoopReturnType(type: Type): Context {
        return this.copy(returnLoopType = type)
    }

    fun getDepth(kClass: KClass<out ASTNode>): Int {
        return kClass.subclasses().sumOf { nodeDepthState.sumOf { map -> map[it] ?: 0 } }
    }

    fun getDepthLast(kClass: KClass<out ASTNode>): Int {
        return kClass.subclasses().sumOf { nodeDepthState.last()[it] ?: 0 }
    }

    fun withSymbolTable(symbolTable: SymbolTable): Context {
        val stateCopy = nodeDepthState.toMutableList().map { it.toMutableMap().withDefault { 0 } }
        return this.copy(
            nodeDepthState = stateCopy,
            statementsPerScope = statementsPerScope.toMutableList(),
            symbolTable = symbolTable
        )
    }

    fun incrementCount(kClass: KClass<out ASTNode>): Context {
        val stateCopy = nodeDepthState.toMutableList().map { it.toMutableMap().withDefault { 0 } }
        stateCopy.last()[kClass] = stateCopy.last().getValue(kClass) + 1
        return this.copy(
            nodeDepthState = stateCopy,
            statementsPerScope = statementsPerScope.toMutableList(),
            previousIncrement = kClass
        )
    }

    fun incrementStatementCount(statement: Statement): Context {
        val stateCopy = nodeDepthState.toMutableList().map { it.toMutableMap().withDefault { 0 } }
        val statementDepthCopy = statementsPerScope.toMutableList()
        statementDepthCopy[statementDepthCopy.lastIndex] =
            listOf(*statementDepthCopy[statementDepthCopy.lastIndex].toTypedArray(), statement)
        return this.copy(nodeDepthState = stateCopy, statementsPerScope = statementDepthCopy, requiredType = null)
    }

    fun enterScope(): Context {
        val stateCopy = nodeDepthState.toMutableList().map { it.toMutableMap().withDefault { 0 } }.toMutableList()
        stateCopy.add(mutableMapOf())
        return this.copy(
            nodeDepthState = stateCopy,
            statementsPerScope = listOf(*statementsPerScope.toTypedArray(), listOf())
        )
    }

    fun resetContextForFunction(): Context {
        return this.copy(statementsPerScope = listOf(), requiredType = null, returnLoopType = null)
    }

    fun forNewStatement(): Context {
        val stateCopy = nodeDepthState.toMutableList().map { it.toMutableMap().withDefault { 0 } }.toMutableList()
        stateCopy.add(mutableMapOf())
        return this.copy(nodeDepthState = stateCopy, requiredType = null)
    }

    fun forDependantDeclaration(): Context {
        val stateCopy = nodeDepthState.toMutableList().map { it.toMutableMap().withDefault { 0 } }.toMutableList()
        stateCopy.removeLast()
        stateCopy.add(mutableMapOf())
        return this.copy(nodeDepthState = stateCopy)
    }

    fun withFunctionName(functionName: String): Context {
        return this.copy(currentFunctionName = functionName)
    }
}
