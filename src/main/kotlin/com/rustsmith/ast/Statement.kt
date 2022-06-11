package com.rustsmith.ast

annotation class GenNode

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

data class ConstDeclaration(
    val type: Type,
    val variableName: String,
    val value: LiteralExpression,
    override val symbolTable: SymbolTable
) : Statement {

    override fun toRust(): String {
        return "const $variableName: ${type.toRust()} = ${value.toRust()};"
    }
}

@GenNode
data class Assignment(val lhs: LHSAssignmentNode, val value: Expression, override val symbolTable: SymbolTable) :
    Statement {

    override fun toRust(): String {
        return "${lhs.toRust()} = ${value.toRust()};"
    }
}

sealed interface BlockEndingStatement : Statement

@GenNode
data class ReturnStatement(
    val expression: Expression,
    override val symbolTable: SymbolTable
) : BlockEndingStatement {
    override fun toRust(): String {
        return "return ${expression.toRust()};"
    }
}

// TODO: Change this to have expressions with break
@GenNode
data class BreakStatement(
    override val symbolTable: SymbolTable
) : BlockEndingStatement {
    override fun toRust(): String {
        return "break;"
    }
}

@GenNode
data class PrintElementStatement(
    val variableName: String,
    override val symbolTable: SymbolTable
) : Statement {
    override fun toRust(): String {
        return "println!(\"{:?}\", (\"$variableName\", $variableName));"
    }
}

data class StatementBlock(val statements: List<Statement>, val symbolTable: SymbolTable) : ASTNode {

    override fun toRust(): String {
        return statements.joinToString("\n") { it.toRust() }
    }
}

data class FetchCLIArgs(override val symbolTable: SymbolTable) : Statement {

    override fun toRust(): String {
        return "use std::env;\nlet cli_args: Vec<String> = env::args().collect();"
    }
}

data class Output(override val symbolTable: SymbolTable, val programSeed: Long?) : Statement {

    override fun toRust(): String {
        val hashString = mutableListOf<String>()
        if (programSeed != null) hashString.add("println!(\"Program Seed: {:?}\", ${programSeed}i64);")
        symbolTable.getOwnedVariables().sorted().forEach {
            hashString.add("println!(\"{:?}\", ${"\"$it\"" to it});")
        }
        return hashString.joinToString("\n")
    }
}
