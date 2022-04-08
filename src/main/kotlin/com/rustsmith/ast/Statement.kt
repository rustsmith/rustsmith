package com.rustsmith.ast

import com.fasterxml.jackson.annotation.JsonIgnoreProperties

annotation class GenNode

@JsonIgnoreProperties(value = ["symbolTable"])
sealed interface Statement : ASTNode {
    val symbolTable: SymbolTable
}

@GenNode
data class ExpressionStatement(
    val expression: Expression,
    val addSemiColon: Boolean,
    override val symbolTable: SymbolTable
) : Statement {
    override fun toRust(): String {
        return "${expression.toRust()}${if (addSemiColon) ";" else ""}"
    }
}

@GenNode
data class Declaration(
    val mutable: Boolean,
    val type: Type,
    val variableName: String,
    val value: Expression,
    override val symbolTable: SymbolTable
) : Statement {

    override fun toRust(): String {
        return "let ${if (mutable) "mut " else ""}$variableName: ${type.toRust()} = ${value.toRust()};"
    }
}

@GenNode
data class Assignment(val variableName: String, val value: Expression, override val symbolTable: SymbolTable) :
    Statement {

    override fun toRust(): String {
        return "$variableName = ${value.toRust()};"
    }
}

@GenNode
data class StatementBlock(val statements: List<Statement>, val symbolTable: SymbolTable) : ASTNode {

    override fun toRust(): String {
        return statements.joinToString("\n") { it.toRust() }
    }
}

data class Output(override val symbolTable: SymbolTable, val programSeed: Long) : Statement {

    override fun toRust(): String {
        val hashString = mutableListOf<String>()
        hashString.add("println!(\"Program Seed: {:?}\", ${programSeed}i64);")
        symbolTable.getOwnedVariables().sorted().forEach {
            hashString.add("println!(\"{:?}\", ${"\"$it\"" to it});")
        }
        return hashString.joinToString("\n")
    }
}
