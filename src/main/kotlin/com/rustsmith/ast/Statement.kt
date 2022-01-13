package com.rustsmith.ast

import com.fasterxml.jackson.annotation.JsonIgnoreProperties

annotation class GenNode(val weight: Int = 1)

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
        return "let ${if (mutable) "mut" else ""} $variableName: ${type.toRust()} = ${value.toRust()};"
    }
}

@GenNode
data class Assignment(val variableName: String, val value: Expression, override val symbolTable: SymbolTable) :
    Statement {

    override fun toRust(): String {
        return "$variableName = ${value.toRust()};"
    }
}

@GenNode(1)
data class ChainedStatement(val s1: Statement, val s2: Statement, override val symbolTable: SymbolTable) : Statement {

    companion object {
        fun createFromList(statements: List<Statement>, symbolTable: SymbolTable): Statement {
            if (statements.size == 1) {
                return statements.first()
            }
            return ChainedStatement(statements.first(), createFromList(statements.drop(1), symbolTable), symbolTable)
        }
    }

    override fun toRust(): String {
        return "${s1.toRust()}\n${s2.toRust()}"
    }
}

data class Output(override val symbolTable: SymbolTable, val programSeed: Long) : Statement {

    override fun toRust(): String {
        val hashString = mutableListOf<String>()
        hashString.add("println!(\"Program Seed: {:?}\", ${programSeed}i64);")
        symbolTable.getCurrentVariables().sorted().forEach {
            hashString.add("println!(\"{:?}\", ${"\"$it\"" to it});")
        }
        return hashString.joinToString("\n")
    }
}
