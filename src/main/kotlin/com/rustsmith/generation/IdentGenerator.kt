package com.rustsmith.generation

const val VARIABLE_PREFIX = "var"
const val FUNCTION_PREFIX = "fun"
const val STRUCT_PREFIX = "Struct"

object IdentGenerator {
    private val prefixMap = mutableMapOf<String, Int>().withDefault { 0 }

    fun generateVariable(): String = generate(VARIABLE_PREFIX)

    fun generateFunctionName(): String = generate(FUNCTION_PREFIX)

    fun generateStructName(): String = generate(STRUCT_PREFIX)

    private fun generate(prefix: String): String {
        prefixMap[prefix] = prefixMap.getValue(prefix) + 1
        return "$prefix${prefixMap[prefix]}"
    }

    fun reset() {
        prefixMap.clear()
    }
}
