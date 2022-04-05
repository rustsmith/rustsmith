package com.rustsmith.ast

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import com.fasterxml.jackson.annotation.JsonTypeInfo
import com.rustsmith.generation.ASTGenerator
import com.rustsmith.generation.Context

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "@type")
@JsonIgnoreProperties(value = ["symbolTable"])
sealed interface ASTNode {
    fun toRust(): String
}

data class FunctionDefinition(
    val returnType: Type = VoidType,
    val functionName: String,
    val arguments: Map<String, Type>,
    val body: StatementBlock
) : ASTNode {
    override fun toRust(): String {
        return "fn $functionName(${
            arguments.map { "${it.key}: ${it.value.toRust()}" }.joinToString(", ")
        }) -> ${returnType.toRust()} {\n${body.toRust()}\n}\n"
    }
}

data class Program(
    val seed: Long,
    val structs: List<Any> = emptyList(),
    val functions: List<FunctionDefinition>
) :
    ASTNode {
    override fun toRust(): String {
        return "#![allow(warnings, unused, unconditional_panic)]\n${functions.joinToString("\n") { it.toRust() }}"
    }
}

fun generateProgram(programSeed: Long): Program {
    val functionSymbolTable = FunctionSymbolTable()
    val symbolTable = SymbolTable(null, functionSymbolTable)
    val body = ASTGenerator(symbolTable)(Context(mapOf(), listOf(), symbolTable))
    val bodyWithOutput =
        StatementBlock(body.statements + Output(symbolTable, programSeed), symbolTable)
    val mainFunction = FunctionDefinition(
        functionName = "main",
        arguments = emptyMap(),
        body = bodyWithOutput
    )
    return Program(programSeed, emptyList(), functionSymbolTable.functions + mainFunction)
}