package com.rustsmith.ast

import com.rustsmith.CustomRandom

enum class OwnershipState {
    VALID,
    PARTIALLY_VALID,
    INVALID
}

data class IdentifierData(val type: Type, val mutable: Boolean, val validity: OwnershipState)

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
            .randomOrNull(CustomRandom)
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

    fun getRandomStruct(): Pair<String, IdentifierData>? = symbolMap.toList().randomOrNull(CustomRandom)

    fun findStructWithType(type: Type): StructType? {
        val structDefinition =
            structs.filter { structDef -> structDef.arguments.any { it.second == type } }.randomOrNull()
        return symbolMap[structDefinition?.structName]?.type as StructType?
    }

    /* Tuple methods */

    fun addTupleType(type: TupleType) = tupleTypes.add(type)

    fun getRandomTuple(): TupleType? = tupleTypes.randomOrNull(CustomRandom)

    fun findTupleWithType(type: Type): TupleType? {
        return tupleTypes.filter { it.types.contains(type) }.randomOrNull()
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

    fun setVariableOwnershipState(key: String, ownershipState: OwnershipState) {
        for (table in iterator()) {
            if (table.symbolMap.containsKey(key)) {
                table.symbolMap[key] = table.symbolMap[key]!!.copy(validity = ownershipState)
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
        return overallMap.toList().filter { it.second.validity == OwnershipState.VALID }.map { it.first }.toSet()
    }

    fun getRandomMutableVariable(): Pair<String, IdentifierData>? {
        val overallMap = mutableMapOf<String, IdentifierData>()
        for (table in iterator()) {
            table.symbolMap.forEach { overallMap.putIfAbsent(it.key, it.value) }
        }
        return overallMap.toList().filter { it.second.mutable }.filter { it.second.validity == OwnershipState.VALID }
            .randomOrNull(CustomRandom)
    }

    fun getRandomVariableOfType(type: Type, requiredType: Type?): Pair<String, IdentifierData>? {
        val overallMap = mutableMapOf<String, IdentifierData>()
        for (table in iterator()) {
            table.symbolMap.forEach { overallMap.putIfAbsent(it.key, it.value) }
        }
        if (requiredType != null && type is RecursiveType) {
            val partiallyOrCompletelyValidVariables = overallMap.toList().filter { it.second.type == type }
                .filter { it.second.validity != OwnershipState.INVALID }
            if ("var137" in partiallyOrCompletelyValidVariables.map { it.first }) {
                println("HERE2")
            }
            return partiallyOrCompletelyValidVariables.filter { variable ->
                (variable.second.type as RecursiveType).argumentsToOwnershipMap.any { it == requiredType to OwnershipState.VALID } ||
                    (variable.second.type as RecursiveType).argumentsToOwnershipMap.any { it == requiredType to OwnershipState.PARTIALLY_VALID }
            }.randomOrNull()
        }
        return overallMap.toList().filter { it.second.type == type }
            .filter { it.second.validity == OwnershipState.VALID }
            .randomOrNull(CustomRandom)
    }

    fun enterScope(): SymbolTable {
        return SymbolTable(this, functionSymbolTable, globalSymbolTable)
    }

    override fun iterator() = SymbolTableIterator(this)
}
