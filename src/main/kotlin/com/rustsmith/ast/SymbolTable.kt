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

class GlobalSymbolTable {
    private val symbolMap = mutableMapOf<String, IdentifierData>()
    val structs = mutableListOf<StructDefinition>()
    val tupleTypes = mutableListOf<TupleType>()

    operator fun get(key: String): IdentifierData? {
        return symbolMap[key]
    }

    operator fun set(key: String, value: IdentifierData) {
        symbolMap[key] = value
    }

    /* Struct methods */

    fun addStruct(structDefinition: StructDefinition) = structs.add(structDefinition)

    fun getRandomStruct(): Pair<String, IdentifierData>? = symbolMap.toList().randomOrNull(Random)

    fun findStructWithType(type: Type): StructType? {
        val structDefinition =
            structs.filter { structDef -> structDef.arguments.any { it.second == type } }.randomOrNull()
        return symbolMap[structDefinition?.structName]?.type as StructType?
    }

    /* Tuple methods */

    fun addTupleType(type: TupleType) = tupleTypes.add(type)

    fun getRandomTuple(): TupleType? = tupleTypes.randomOrNull(Random)

    fun findTupleWithType(type: Type): TupleType? {
        return tupleTypes.filter { it.types.contains(type to true) }.randomOrNull()
    }
}

data class SymbolTable(
    val parent: SymbolTable?,
    val functionSymbolTable: FunctionSymbolTable,
    val globalSymbolTable: GlobalSymbolTable
) : Iterable<SymbolTable> {
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

    fun getRandomTupleAccessOfType(type: Type): Pair<String, Pair<Type, Int>>? {
        val overallMap = mutableMapOf<String, Pair<Type, Int>>()
        for (table in iterator()) {
            table.symbolMap.forEach {
                if (it.value.valid && it.value.type is TupleType && (it.value.type as TupleType).types.contains(type to true)) {
                    val indexOfTupleAccess = (it.value.type as TupleType).types.indexOf(type to true)
//                    val typesList = (it.value.type as TupleType).types.toMutableList()
//                    typesList[indexOfTupleAccess] = typesList[indexOfTupleAccess].copy(second = false)
//                    table.symbolMap[it.key] =  table.symbolMap[it.key]!!.copy(type = (it.value.type as TupleType).copy(types = typesList), valid = false)
                    overallMap.putIfAbsent(it.key, it.value.type to indexOfTupleAccess)
                }
            }
        }
        return overallMap.toList().randomOrNull(Random)
    }

    fun enterScope(): SymbolTable {
        return SymbolTable(this, functionSymbolTable, globalSymbolTable)
    }

    override fun iterator() = SymbolTableIterator(this)
}
