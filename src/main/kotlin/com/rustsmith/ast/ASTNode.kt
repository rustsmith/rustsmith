package com.rustsmith.ast

import com.rustsmith.generation.ASTGenerator
import com.rustsmith.generation.Context
import com.rustsmith.recondition.Macros

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

data class StructDefinition(val structType: LifetimeParameterizedType<StructType>) : ASTNode {
    override fun toRust(): String {
        val traits = "#[derive(Debug)]\n"
        val parameterizedSyntax = if (structType.lifetimeParameters().isNotEmpty()) "<${structType.lifetimeParameters().toSet().joinToString(",") { "'a$it" }}>" else ""
        return "${traits}struct ${structType.type.structName}$parameterizedSyntax {\n${structType.type.types.joinToString("\n") { "${it.first}: ${it.second.toRust()}," }}\n}\n"
    }
}

data class Program(
    val seed: Long,
    val macros: Set<Macros>,
    val structs: List<StructDefinition> = emptyList(),
    val functions: List<FunctionDefinition>
) :
    ASTNode {
    override fun toRust(): String {
        return "#![allow(warnings, unused, unconditional_panic)]\n${macros.joinToString("\n") { it.toRust() }}\n${structs.joinToString("\n") { it.toRust() }}\n${
        functions.joinToString(
            "\n"
        ) { it.toRust() }
        }"
    }
}

fun generateProgram(programSeed: Long, failFast: Boolean): Pair<Program, List<String>> {
    val functionSymbolTable = FunctionSymbolTable()
    val globalSymbolTable = GlobalSymbolTable()
    val symbolTable = SymbolTable(null, functionSymbolTable, globalSymbolTable)
    val astGenerator = ASTGenerator(symbolTable, failFast)
    val mainFunctionContext = Context(listOf(mapOf()), "main", listOf(), symbolTable)
    val body = astGenerator(mainFunctionContext)
    val bodyWithOutput =
        StatementBlock(listOf(FetchCLIArgs(symbolTable)) + body.statements + Output(symbolTable, programSeed), symbolTable)
    val mainFunction = FunctionDefinition(
        functionName = "main",
        arguments = emptyMap(),
        body = bodyWithOutput
    )
    val cliArguments = symbolTable.globalSymbolTable.commandLineTypes.map { astGenerator.generateCLIArgumentsForLiteralType(it, mainFunctionContext) }
    return Program(programSeed, setOf(), globalSymbolTable.structs.toList(), functionSymbolTable.functions + mainFunction) to cliArguments
}
