package com.rustsmith.ast

import com.rustsmith.Random

data class IdentifierData(val type: Type, val mutable: Boolean, val valid: Boolean = true)

class SymbolTableIterator(private val symbolTable: SymbolTable) : Iterator<SymbolTable> {
    private var current: SymbolTable? = null

    override fun hasNext(): Boolean = current == null || current?.parent != null

    override fun next(): SymbolTable {
        if (hasNext()) {
            current = if (current == null) symbolTable else current?.parent!!
            return current!!
        }
        throw Exception("No parent for symbol table")
    }
}

class FunctionSymbolTable {
    private val symbolMap = mutableMapOf<String, IdentifierData>()
    val functions = mutableListOf<FunctionDefinition>()

    fun getRandomFunctionOfType(type: Type): Pair<String, IdentifierData>? {
        return symbolMap.toList().filter { (it.second.type as FunctionType).returnType == type }
            .randomOrNull(Random)
    }

    operator fun get(key: String): IdentifierData? {
        return symbolMap[key]
    }

    operator fun set(key: String, value: IdentifierData) {
        symbolMap[key] = value
    }

    fun addFunction(functionDefinition: FunctionDefinition) {
        functions.add(functionDefinition)
    }
}

class StructSymbolTable {
    private val symbolMap = mutableMapOf<String, IdentifierData>()
    val structs = mutableListOf<StructDefinition>()

    fun getRandomStruct(): Pair<String, IdentifierData>? {
        return symbolMap.toList().randomOrNull(Random)
    }

    operator fun get(key: String): IdentifierData? {
        return symbolMap[key]
    }

    operator fun set(key: String, value: IdentifierData) {
        symbolMap[key] = value
    }

    fun addStruct(structDefinition: StructDefinition) {
        structs.add(structDefinition)
    }
}

data class SymbolTable(
    val parent: SymbolTable?,
    val functionSymbolTable: FunctionSymbolTable,
    val structSymbolTable: StructSymbolTable
) :
    Iterable<SymbolTable> {
    private val symbolMap = mutableMapOf<String, IdentifierData>()

    operator fun get(key: String): IdentifierData? {
        for (table in iterator()) {
            if (table.symbolMap.containsKey(key)) {
                return table.symbolMap[key]
            }
        }
        return functionSymbolTable[key]
    }

    fun removeVariableOwnership(key: String) {
        for (table in iterator()) {
            if (table.symbolMap.containsKey(key)) {
                table.symbolMap[key] = table.symbolMap[key]!!.copy(valid = false)
                return
            }
        }
    }

    operator fun set(key: String, value: IdentifierData) {
        symbolMap[key] = value
    }

    /* Not affected by ownership of variables as it is a statistic used by weighting strategy */
    fun getLocalVariables(): Set<String> {
        return symbolMap.keys
    }

    fun getCurrentVariables(): Set<String> {
        val currentVariables = mutableSetOf<String>()
        for (table in iterator()) {
            currentVariables.addAll(table.symbolMap.keys)
        }
        return currentVariables
    }

    fun getOwnedVariables(): Set<String> {
        val overallMap = mutableMapOf<String, IdentifierData>()
        for (table in iterator()) {
            table.symbolMap.forEach { overallMap.putIfAbsent(it.key, it.value) }
        }
        return overallMap.toList().filter { it.second.valid }.map { it.first }.toSet()
    }

    fun getRandomMutableVariable(): Pair<String, IdentifierData>? {
        val overallMap = mutableMapOf<String, IdentifierData>()
        for (table in iterator()) {
            table.symbolMap.forEach { overallMap.putIfAbsent(it.key, it.value) }
        }
        return overallMap.toList().filter { it.second.mutable }.filter { it.second.valid }.randomOrNull(Random)
    }

    fun getRandomVariableOfType(type: Type): Pair<String, IdentifierData>? {
        val overallMap = mutableMapOf<String, IdentifierData>()
        for (table in iterator()) {
            table.symbolMap.forEach { overallMap.putIfAbsent(it.key, it.value) }
        }
        return overallMap.toList().filter { it.second.type == type }.filter { it.second.valid }
            .randomOrNull(Random)
    }

    fun enterScope(): SymbolTable {
        return SymbolTable(this, functionSymbolTable, structSymbolTable)
    }

    override fun iterator() = SymbolTableIterator(this)
}
