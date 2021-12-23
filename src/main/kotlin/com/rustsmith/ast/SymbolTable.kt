package com.rustsmith

import com.rustsmith.ast.Type
import kotlin.reflect.KClass

data class IdentifierData(val type: Type)

class SymbolTable {
    val symbolMap = mutableMapOf<String, IdentifierData>()

    operator fun get(key: String): IdentifierData? {
        return symbolMap[key]
    }

    operator fun set(key: String, value: Type) {
        symbolMap[key] = IdentifierData(value)
    }

    fun getCurrentVariables(): Set<String> {
        return symbolMap.keys
    }

    fun getRandomVariable(): Pair<String, IdentifierData>? {
        return symbolMap.toList().randomOrNull(Random)
    }

    fun findVariableWithType(type: KClass<out Type>): Pair<String, IdentifierData>? {
        return symbolMap.toList().find { it.second.type::class == type }
    }
}
