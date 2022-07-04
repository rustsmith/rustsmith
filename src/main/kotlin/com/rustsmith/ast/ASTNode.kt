package com.rustsmith.ast

import com.rustsmith.CustomRandom
import com.rustsmith.generation.ASTGenerator
import com.rustsmith.generation.Context
import com.rustsmith.generation.IdentGenerator
import com.rustsmith.recondition.Macros

sealed interface ASTNode {
    fun toRust(): String
}

data class FunctionDefinition(
    val returnType: Type = VoidType,
    val functionName: String,
    val arguments: Map<String, Type>,
    val body: StatementBlock,
    val forceNoInline: Boolean,
    val addSelfVariable: Boolean
) : ASTNode {
    override fun toRust(): String {
        val inline = if (forceNoInline) "#[inline(never)]" else ""
        val self = if (addSelfVariable) "self," else ""
        return "$inline\nfn $functionName($self ${
        arguments.map { "${it.key}: ${it.value.toRust()}" }.joinToString(", ")
        }) -> ${returnType.toRust()} {\n${body.toRust()}\n}\n"
    }
}

data class StructDefinition(val structType: LifetimeParameterizedType<StructType>, val methods: MutableList<FunctionDefinition> = mutableListOf()) : ASTNode {
    override fun toRust(): String {
        val traits = "#[derive(Debug)]\n"
        val parameterizedSyntax = if (structType.lifetimeParameters().isNotEmpty()) "<${structType.lifetimeParameters().toSet().joinToString(",") { "'a$it" }}>" else ""
        val structDef = "${traits}struct ${structType.type.structName}$parameterizedSyntax {\n${structType.type.types.joinToString("\n") { "${it.first}: ${it.second.toRust()}," }}\n}\n"
        val implDef = "\nimpl$parameterizedSyntax ${structType.type.structName}$parameterizedSyntax {\n ${methods.joinToString("\n") { it.toRust() }} \n}"
        return structDef + implDef
    }
}

data class Program(
    val seed: Long,
    val macros: Set<Macros>,
    val constants: List<ConstDeclaration>,
    val structs: List<StructDefinition> = emptyList(),
    val functions: List<FunctionDefinition>
) :
    ASTNode {
    override fun toRust(): String {
        return "#![allow(warnings, unused, unconditional_panic)]\nuse std::env;\nuse std::collections::hash_map::DefaultHasher;\nuse std::hash::{Hash, Hasher};\n${constants.joinToString("\n") { it.toRust() }}\n${macros.joinToString("\n") { it.toRust() }}\n${structs.joinToString("\n") { it.toRust() }}\n${
        functions.joinToString(
            "\n"
        ) { it.toRust() }
        }"
    }
}

fun generateProgram(programSeed: Long, identGenerator: IdentGenerator, failFast: Boolean): Pair<Program, List<String>> {
    val functionSymbolTable = FunctionSymbolTable()
    val globalSymbolTable = GlobalSymbolTable()
    val symbolTable = SymbolTable(SymbolTable(null, functionSymbolTable, globalSymbolTable), functionSymbolTable, globalSymbolTable)
    val astGenerator = ASTGenerator(symbolTable, failFast, identGenerator)
    val mainFunctionContext = Context(listOf(mapOf()), "main", listOf(), symbolTable)
    val numberOfConstants = CustomRandom.nextInt(10)
    val constantDeclarations = (0..numberOfConstants).map { astGenerator.generateConstantDeclaration(mainFunctionContext) }
    val body = astGenerator(mainFunctionContext)
    val bodyWithOutput =
        StatementBlock(listOf(FetchCLIArgs(symbolTable)) + body.statements + Output(symbolTable, programSeed), symbolTable)
    val mainFunction = FunctionDefinition(
        functionName = "main",
        arguments = emptyMap(),
        body = bodyWithOutput,
        forceNoInline = false,
        addSelfVariable = false
    )
    val cliArguments = symbolTable.globalSymbolTable.commandLineTypes.map { astGenerator.generateCLIArgumentsForLiteralType(it, mainFunctionContext) }
    return Program(programSeed, setOf(), constantDeclarations, globalSymbolTable.structs.toList(), functionSymbolTable.functions + mainFunction) to cliArguments
}
