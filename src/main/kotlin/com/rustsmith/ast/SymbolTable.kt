package com.rustsmith.ast

import com.rustsmith.Random

data class IdentifierData(val type: Type, val virtual: Boolean = false)

class SymbolTableIterator(private val symbolTable: SymbolTable) : Iterator<SymbolTable> {
    var current: SymbolTable? = null

    override fun hasNext(): Boolean = current == null || current?.parent != null

    override fun next(): SymbolTable {
        if (hasNext()) {
            current = if (current == null) symbolTable else current?.parent!!
            return current!!
        }
        throw Exception("No parent for symbol table")
    }
}

data class SymbolTable(val parent: SymbolTable?) : Iterable<SymbolTable> {
    private val symbolMap = mutableMapOf<String, IdentifierData>()

    operator fun get(key: String): IdentifierData? {
        for (table in iterator()) {
            if (table.symbolMap.containsKey(key)) {
                return table.symbolMap[key]
            }
        }
        return null
    }

    operator fun set(key: String, value: IdentifierData) {
        symbolMap[key] = value
    }

    private fun setVirtualVariable(value: Type): Pair<String, IdentifierData> {
        val variableName = VariableGenerator.generateVariable()
        val identifierData = IdentifierData(value, true)
        symbolMap[variableName] = identifierData
        return variableName to identifierData
    }

    fun getCurrentVariables(): Set<String> {
        val currentVariables = mutableSetOf<String>()
        for (table in iterator()) {
            currentVariables.addAll(table.symbolMap.keys)
        }
        return currentVariables
    }

    fun getRandomVariable(): Pair<String, IdentifierData> {
        return symbolMap.toList().randomOrNull(Random) ?: setVirtualVariable(Type::class.genSubClasses().random(Random).objectInstance!!)
    }

    fun getRandomVariableOfType(type: Type): Pair<String, IdentifierData> {
        val overallMap = mutableMapOf<String, IdentifierData>()
        for (table in iterator()) {
            table.symbolMap.forEach { overallMap.putIfAbsent(it.key, it.value) }
        }
        return overallMap.toList().filter { it.second.type == type }.randomOrNull(Random) ?: setVirtualVariable(type)
    }

    private fun cleanupCurrentVirtualVariables() {
        symbolMap.filter { it.value.virtual }.forEach { symbolMap[it.key] = it.value.copy(virtual = false) }
    }

    private fun getCurrentVirtualVariables(): List<Pair<String, IdentifierData>> {
        return symbolMap.filter { it.value.virtual }.toList()
    }

    fun enterScope(): SymbolTable {
        return SymbolTable(this)
    }

    fun exitScope(body: Statement): Statement {
        val declarations = getCurrentVirtualVariables().map {
            Declaration(
                it.second.type, it.first, it.second.type.generateLiteral(this), this
            )
        }
        this.cleanupCurrentVirtualVariables()
        return if (declarations.isEmpty()) body else ChainedStatement(ChainedStatement.createFromList(declarations, this), body, this)
    }

    override fun iterator() = SymbolTableIterator(this)
}
