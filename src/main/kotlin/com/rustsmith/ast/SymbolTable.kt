package com.rustsmith.ast

import com.rustsmith.CustomRandom
import com.rustsmith.generation.Context
import com.rustsmith.subclasses

enum class OwnershipState {
    VALID, BORROWED, MUTABLY_BORROWED, PARTIALLY_VALID, INVALID;

    fun borrowable() = this == VALID || this == BORROWED
    fun movable() = this == VALID
    fun assignable() = this == VALID || this == PARTIALLY_VALID

    fun overridingState() = this in listOf(INVALID, PARTIALLY_VALID)
}

data class IdentifierData(
    val type: Type,
    val mutable: Boolean,
    val validity: OwnershipState,
    val depth: Int,
    val constant: Boolean = false
) {
    fun clone(): IdentifierData {
        return this.copy(type = type.clone())
    }
}

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
    val structs = mutableSetOf<StructDefinition>()
    val tupleTypes = mutableSetOf<TupleType>()
    val vectorTypes = mutableSetOf<Type>()
    val optionTypes = mutableSetOf<Type>()
    val arrayTypes = mutableSetOf<StaticSizedArrayType>()
    val boxTypes = mutableSetOf<Type>()
    val typeAliases = mutableSetOf<TypeAliasDefinition>()
    val commandLineTypes = mutableSetOf<LiteralType>()

    operator fun get(key: String): IdentifierData? {
        return symbolMap[key]
    }

    operator fun set(key: String, value: IdentifierData) {
        symbolMap[key] = value
    }

    /* Type aliases methods */

    fun addTypeAlias(type: TypeAliasDefinition) = typeAliases.add(type)

    fun getRandomTypeAlias(): TypeAliasType? = typeAliases.randomOrNull(CustomRandom)?.aliasType?.type

    /* Box methods */

    fun addBoxType(type: Type) = boxTypes.add(type)

    fun getRandomBoxType(): Type? = boxTypes.randomOrNull(CustomRandom)

    /* Array methods */

    fun addVectorType(type: Type) = vectorTypes.add(type)

    fun addArrayType(type: Type, index: UInt) = arrayTypes.add(StaticSizedArrayType(type, index))

    fun getRandomVectorType(): Type? = vectorTypes.randomOrNull(CustomRandom)

    fun getRandomArrayType(): StaticSizedArrayType? = arrayTypes.randomOrNull(CustomRandom)

    /* Option methods */

    fun addOptionType(type: Type) = optionTypes.add(type)

    fun getRandomOptionType(): Type? = optionTypes.randomOrNull(CustomRandom)

    /* Struct methods */

    fun addStruct(structDefinition: StructDefinition) = structs.add(structDefinition)

    fun addMethod(structType: StructType, method: FunctionDefinition) {
        structs.find { it.structType.type.structName == structType.structName }!!.methods.add(method)
    }

    fun getRandomMethodOfType(type: Type): Pair<StructType, FunctionDefinition>? {
        return structs.flatMap { struct -> struct.methods.map { struct.structType.type to it } }
            .filter { it.second.returnType == type }.randomOrNull(CustomRandom)
    }

    fun getRandomStruct(ctx: Context): Pair<String, IdentifierData>? {
        if (ctx.getDepth(OptionType::class) > 0) {
            return symbolMap.toList().filter { it.second.type.memberTypes().find { kClass -> kClass::class == MutableReferenceType::class } == null }
                .filter { it.second.type.memberTypes().find { kClass -> kClass::class == MutableReferenceType::class } == null }
                .filter { it.second.type.memberTypes().find { kClass -> kClass::class == ReferenceType::class } == null }
                .filter { it.second.type.memberTypes().find { kClass -> kClass::class == BoxType::class } == null }
                .randomOrNull(CustomRandom)
        }
        return symbolMap.toList().filter { it.second.type is StructType }.randomOrNull(CustomRandom)
    }

    fun findStructWithType(type: Type): StructType? {
        val structDefinition =
            structs.filter { structDef -> structDef.structType.type.types.any { it.second == type } }
                .randomOrNull(CustomRandom)
        return (symbolMap[structDefinition?.structType?.type?.structName]?.type as StructType?)
    }

    /* Tuple methods */

    fun addTupleType(type: TupleType) = tupleTypes.add(type.clone())

    fun getRandomTuple(ctx: Context): TupleType? {
        if (ctx.getDepth(OptionType::class) > 0) {
            return tupleTypes.filter { it.memberTypes().find { kClass -> kClass::class == MutableReferenceType::class } == null }
                .filter { it.memberTypes().find { kClass -> kClass::class == MutableReferenceType::class } == null }
                .filter { it.memberTypes().find { kClass -> kClass::class == ReferenceType::class } == null }
                .filter { it.memberTypes().find { kClass -> kClass::class == BoxType::class } == null }
                .randomOrNull(CustomRandom)
        }
        return tupleTypes.randomOrNull(CustomRandom)
    }
}

data class SymbolTable(
    val parent: SymbolTable?,
    val functionSymbolTable: FunctionSymbolTable,
    val globalSymbolTable: GlobalSymbolTable,
    val symbolMap: MutableMap<String, IdentifierData> = mutableMapOf()
) : Iterable<SymbolTable> {
    val depth = lazy {
        var count = 0
        for (table in iterator()) {
            count++
        }
        count
    }

    fun root(): SymbolTable {
        var current = this
        for (table in this.iterator()) {
            current = table
        }
        return current
    }

    fun snapshot(): SymbolTable {
        return SymbolTable(
            parent?.snapshot(),
            functionSymbolTable,
            globalSymbolTable,
            symbolMap.mapValues { it.value.clone().copy(type = it.value.type.snapshot()) }.toMutableMap()
        )
    }

    fun mergeSnapshot(symbolTable: SymbolTable) {
        val i = this.iterator()
        for (table in symbolTable.iterator()) {
            val iSymbolTable = i.next()
            iSymbolTable.symbolMap.clear()
            iSymbolTable.symbolMap.putAll(table.symbolMap)
        }
    }

    operator fun get(key: String): IdentifierData? {
        for (table in iterator()) {
            if (table.symbolMap.containsKey(key)) {
                return table.symbolMap[key]
            }
        }
        return functionSymbolTable[key]
    }

    fun setVariableOwnershipState(key: String, ownershipState: OwnershipState, depth: Int?) {
        if (ownershipState.overridingState()) {
            for (table in iterator()) {
                if (table.symbolMap.containsKey(key)) {
                    table.symbolMap[key] = table.symbolMap[key]!!.copy(validity = ownershipState)
                }
            }
        } else if (depth != null) {
            for (table in iterator()) {
                if (table.depth.value == depth) {
                    table.symbolMap[key] = get(key)!!.copy(validity = ownershipState)
                }
            }
        } else {
            symbolMap[key] = get(key)!!.copy(validity = ownershipState)
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

    fun getAllVariables(): MutableMap<String, IdentifierData> {
        val overallMap = mutableMapOf<String, IdentifierData>()
        for (table in iterator()) {
            table.symbolMap.forEach { overallMap.putIfAbsent(it.key, it.value) }
        }
        return overallMap
    }

    fun getOwnedVariables(allowConstants: Boolean = true): Set<String> {
        val overallMap = mutableMapOf<String, IdentifierData>()
        for (table in iterator()) {
            table.symbolMap.forEach { overallMap.putIfAbsent(it.key, it.value) }
        }
        return overallMap.toList().filter { it.second.validity == OwnershipState.VALID }
            .filter { if (allowConstants) true else !it.second.constant }
            .map { it.first }.toSet()
    }

    private fun findMutableSubExpressions(expression: LHSAssignmentNode): List<LHSAssignmentNode> {
        return listOf(expression) + when (val type = expression.toType()) {
            is ContainerType -> {
                when (type) {
                    is StructType -> type.argumentsToOwnershipMap.mapIndexed { index, pair ->
                        StructElementAccessExpression(
                            expression, type.types[index].first, this
                        ) to pair
                    }.filter { it.second.second.assignable() }
                        .flatMap { findMutableSubExpressions(it.first) }
                    is TupleType -> type.argumentsToOwnershipMap.mapIndexed { index, pair ->
                        TupleElementAccessExpression(
                            expression, index, this
                        ) to pair
                    }.filter { it.second.second.assignable() }
                        .flatMap { findMutableSubExpressions(it.first) }
                    // TODO: Add array access
                    is VectorType -> listOf()
                    is BoxType -> listOf()
                    is StaticSizedArrayType -> listOf()
                }
            }
            else -> listOf()
        }
    }

    data class MutableVariableResult(val node: LHSAssignmentNode, val identifierData: IdentifierData)

    fun getRandomMutableVariable(ctx: Context): MutableVariableResult? {
        val overallMap = mutableMapOf<String, IdentifierData>()
        for (table in iterator()) {
            table.symbolMap.forEach { overallMap.putIfAbsent(it.key, it.value) }
        }
        return overallMap.toList().filter { it.second.validity.assignable() }.filter {
            if (ctx.assignmentRootNode == null) true else (
                !ctx.assignmentRootNode.map { variable -> variable.value }
                    .contains(it.first)
                )
        }.flatMap {
            findMutableSubExpressions(Variable(it.first, this)).map { exp ->
                MutableVariableResult(
                    exp,
                    it.second
                )
            }
        }
            .filter { it.identifierData.mutable || it.node.toType() is MutableReferenceType }.randomOrNull(CustomRandom)
    }

    fun getRandomVariableOfType(
        type: Type,
        requiredType: Type?,
        ctx: Context,
        mutableRequired: Boolean
    ): List<String> {
        var overallMap = mutableMapOf<String, IdentifierData>()
        if (ctx.getDepth(LoopExpression::class) > 0) {
            if (ctx.lifetimeRequirement == null ||
                (
                    type.memberTypes()
                        .count { it is ReferencingTypes } == 0 && ctx.getDepth(ReferencingExpressions::class) == 0
                    ) ||
                this.depth.value <= ctx.lifetimeRequirement
            ) {
                this.symbolMap.forEach { overallMap.putIfAbsent(it.key, it.value) }
            }
        } else {
            for (table in iterator()) {
                table.symbolMap.forEach { overallMap.putIfAbsent(it.key, it.value) }
            }
        }
        if ((
            type.memberTypes()
                .count { it is ReferencingTypes } > 0 || ctx.getDepth(ReferencingExpressions::class) > 0
            ) &&
            ctx.lifetimeRequirement != null
        ) {
            overallMap = overallMap.filter {
                it.value.depth <= ctx.lifetimeRequirement
            }.toMutableMap()
        }

        overallMap = overallMap.filter {
            ctx.assignmentRootNode?.map { variable -> variable.value }?.contains(it.key) == false
        }.toMutableMap()

        if (requiredType != null && type is RecursiveType && ctx.previousIncrement in PartialMoveExpression::class.subclasses()) {
            val partiallyOrCompletelyValidVariables = overallMap.toList().filter { it.second.type == type }
                .filter { it.second.validity != OwnershipState.INVALID }
                .filter { if (ctx.getDepthLast(ReferenceExpression::class) > 0) it.second.validity != OwnershipState.PARTIALLY_VALID else true }
            return partiallyOrCompletelyValidVariables.filter { variable ->
                (variable.second.type as RecursiveType).argumentsToOwnershipMap.any {
                    it.first == requiredType && it.second == OwnershipState.VALID
                } ||
                    if (ctx.getDepthLast(PartialMoveExpression::class) > 1)
                        (variable.second.type as RecursiveType).argumentsToOwnershipMap.any {
                            it.first == requiredType && it.second == OwnershipState.PARTIALLY_VALID
                        }
                    else false
            }.filter { it.second.type == type }.filter {
                if (ctx.getDepthLast(ReferenceExpression::class) > 0) it.second.validity
                    .borrowable() else it.second.validity.movable()
            }.filter { it.second.mutable == mutableRequired }.map { it.first }
        }
        return overallMap.toList().filter { it.second.type == type }.filter {
            if (ctx.getDepthLast(ReferenceExpression::class) > 0) it.second.validity
                .borrowable() else it.second.validity.movable()
        }.filter { it.second.mutable == mutableRequired }.map { it.first }
    }

    fun enterScope(): SymbolTable {
        return SymbolTable(this, functionSymbolTable, globalSymbolTable)
    }

    override fun iterator() = SymbolTableIterator(this)
}
