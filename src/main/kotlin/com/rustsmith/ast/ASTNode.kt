package com.rustsmith.ast

import com.fasterxml.jackson.annotation.JsonTypeInfo
import com.rustsmith.generation.ASTGenerator
import com.rustsmith.generation.SelectionManager
import kotlin.reflect.KClass

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "@type")
sealed interface ASTNode {
    fun toRust(): String
}

data class FunctionDefinition(
    val returnType: Type = VoidType,
    val functionName: String,
    val arguments: Map<String, Type>,
    val body: Statement
) : ASTNode {
    override fun toRust(): String {
        return "fn $functionName() -> ${returnType.toRust()} {\n${body.toRust()}\n}\n"
    }
}

data class Program(val seed: Long, val structs: List<Any> = emptyList(), val functions: List<FunctionDefinition>) :
    ASTNode {
    override fun toRust(): String {
        return "#![allow(warnings, unused, unconditional_panic)]\n${functions.joinToString("\n") { it.toRust() }}"
    }
}

fun generateMain(programSeed: Long): FunctionDefinition {
    val symbolTable = SymbolTable(null)
    val body = ChainedStatement(ASTGenerator(symbolTable)(SelectionManager(mapOf())), Output(symbolTable, programSeed), symbolTable)
    return FunctionDefinition(
        functionName = "main",
        arguments = emptyMap(),
        body = body
    )
}

fun <T : Any> KClass<T>.subclasses(): Set<KClass<out T>> {
    if (this.isFinal) {
        return setOf(this)
    }
    val nonFinalClasses = this.sealedSubclasses.filter { !it.isFinal }
    val result = mutableSetOf<KClass<out T>>()
    for (nonFinalClass in nonFinalClasses) {
        result.addAll(nonFinalClass.subclasses())
    }
    return this.sealedSubclasses.filter { it.isFinal }.toSet() + result
}
