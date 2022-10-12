package com.rustsmith.generation

const val TYPE_PREFIX = "Type"
const val CONST_PREFIX = "CONST"
const val VARIABLE_PREFIX = "var"
const val FUNCTION_PREFIX = "fun"
const val STRUCT_PREFIX = "Struct"

class IdentGenerator {
    private val prefixMap = mutableMapOf<String, Int>().withDefault { 0 }

    fun generateVariable(): String = generate(VARIABLE_PREFIX)

    fun generateConst(): String = generate(CONST_PREFIX)

    fun generateTypeAlias(): String = generate(TYPE_PREFIX)

    fun generateFunctionName(): String = generate(FUNCTION_PREFIX)

    fun generateStructName(): String = generate(STRUCT_PREFIX)

    private fun generate(prefix: String): String {
        prefixMap[prefix] = prefixMap.getValue(prefix) + 1
        return "$prefix${prefixMap[prefix]}"
    }
}
