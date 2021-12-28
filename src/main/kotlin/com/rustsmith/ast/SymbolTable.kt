package com.rustsmith

import com.rustsmith.ast.Type
import com.rustsmith.ast.varCount

data class IdentifierData(val type: Type, val virtual: Boolean = false)

class SymbolTable {
    private val symbolMap = mutableMapOf<String, IdentifierData>()

    operator fun get(key: String): IdentifierData? {
        return symbolMap[key]
    }

    operator fun set(key: String, value: Type) {
        symbolMap[key] = IdentifierData(value)
    }

    fun setVirtualVariable(value: Type): Pair<String, IdentifierData> {
        val variableName = "var${varCount++}"
        val identifierData = IdentifierData(value, true)
        symbolMap[variableName] = identifierData
        return variableName to identifierData
    }

    fun getCurrentVariables(): Set<String> {
        return symbolMap.keys
    }

    fun getRandomVariable(): Pair<String, IdentifierData>? {
        return symbolMap.toList().randomOrNull(Random)
    }

    fun getRandomVariableOfType(type: Type): Pair<String, IdentifierData> {
        return symbolMap.toList().filter { it.second.type == type }.randomOrNull(Random) ?: setVirtualVariable(type)
    }

    fun cleanupVirtualVariables() {
        symbolMap.filter { it.value.virtual }.forEach { symbolMap[it.key] = it.value.copy(virtual = false) }
    }

    fun getVirtualVariables(): List<Pair<String, IdentifierData>> {
        return symbolMap.filter { it.value.virtual }.toList()
    }
}
