package com.rustsmith.ast

import com.rustsmith.CustomRandom
import com.rustsmith.generation.Context
import com.rustsmith.subclasses

enum class OwnershipState {
    VALID, BORROWED, MUTABLY_BORROWED, PARTIALLY_VALID, INVALID;
    fun borrowable() = this == VALID || this == BORROWED
    fun movable() = this == VALID
    fun assignable() = this == VALID || this == PARTIALLY_VALID
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
    val commandLineTypes = mutableListOf<CLIInputType>()

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
            structs.filter { structDef -> structDef.arguments.any { it.second == type } }.randomOrNull(CustomRandom)
        return symbolMap[structDefinition?.structName]?.type as StructType?
    }

    /* Tuple methods */

    fun addTupleType(type: TupleType) = tupleTypes.add(type)

    fun getRandomTuple(): TupleType? = tupleTypes.randomOrNull(CustomRandom)

    fun findTupleWithType(type: Type): TupleType? {
        return tupleTypes.filter { it.types.contains(type) }.randomOrNull(CustomRandom)
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

    private fun findMutableSubExpressions(expression: LHSAssignmentNode): List<LHSAssignmentNode> {
        return when (val type = expression.toType()) {
            is ContainerType -> {
                when (type) {
                    is StructType -> type.argumentsToOwnershipMap.mapIndexed { index, pair ->
                        StructElementAccessExpression(
                            expression, type.types[index].first, this
                        ) to pair
                    }.filter { it.second.second.assignable() }.flatMap { findMutableSubExpressions(it.first) }
                    is TupleType -> type.argumentsToOwnershipMap.mapIndexed { index, pair ->
                        TupleElementAccessExpression(
                            expression, index, this
                        ) to pair
                    }.filter { it.second.second.assignable() }.flatMap { findMutableSubExpressions(it.first) }
                }
            }
            else -> listOf(expression)
        }
    }

    fun getRandomMutableVariable(ctx: Context): LHSAssignmentNode? {
        val overallMap = mutableMapOf<String, IdentifierData>()
        var i = 0
        for (table in iterator()) {
            if (i != 0) {
                // Stops dangling references
                table.symbolMap.filter {
                    !it.value.type.memberTypes().map { t -> t::class }.any { k -> k in ReferencingTypes::class.subclasses() }
                }.forEach { overallMap.putIfAbsent(it.key, it.value) }
            } else {
                table.symbolMap.forEach { overallMap.putIfAbsent(it.key, it.value) }
            }
            i++
        }
        return overallMap.toList().filter { it.second.mutable }.filter { it.second.validity.assignable() }.filter {
            if (ctx.assignmentRootNode == null) true else (
                !ctx.assignmentRootNode.map { variable -> variable.value }
                    .contains(it.first)
                )
        }.flatMap { findMutableSubExpressions(Variable(it.first, this)) }.randomOrNull(CustomRandom)
    }

    fun getRandomVariableOfType(type: Type, requiredType: Type?, ctx: Context, mutableRequired: Boolean): Pair<String, IdentifierData>? {
        var overallMap = mutableMapOf<String, IdentifierData>()
        if (ctx.getDepth(LoopExpression::class) > 0) {
            this.symbolMap.forEach { overallMap.putIfAbsent(it.key, it.value) }
        } else {
            for (table in iterator()) {
                table.symbolMap.forEach { overallMap.putIfAbsent(it.key, it.value) }
            }
        }

        overallMap = overallMap.filter {
            ctx.assignmentRootNode?.map { variable -> variable.value }?.contains(it.key) == false
        }.toMutableMap()

        if (requiredType != null && type is RecursiveType && ctx.previousIncrement in PartialMoveExpression::class.subclasses()) {
            val partiallyOrCompletelyValidVariables = overallMap.toList().filter { it.second.type == type }
                .filter { it.second.validity != OwnershipState.INVALID }
            return partiallyOrCompletelyValidVariables.filter { variable ->
                (variable.second.type as RecursiveType).argumentsToOwnershipMap.any { it == requiredType to OwnershipState.VALID } ||
                    if (ctx.getDepthLast(PartialMoveExpression::class) > 1)
                        (variable.second.type as RecursiveType).argumentsToOwnershipMap.any { it == requiredType to OwnershipState.PARTIALLY_VALID }
                    else false
            }.filter { it.second.mutable == mutableRequired }.randomOrNull(CustomRandom)
        }
        return overallMap.toList().filter { it.second.type == type }.filter {
            if (ctx.getDepthLast(ReferenceExpression::class) > 0) it.second.validity.borrowable() else it.second.validity.movable()
        }.filter { it.second.mutable == mutableRequired }.randomOrNull(CustomRandom)
    }

    fun enterScope(): SymbolTable {
        return SymbolTable(this, functionSymbolTable, globalSymbolTable)
    }

    override fun iterator() = SymbolTableIterator(this)
}
