package com.rustsmith.ast

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import com.rustsmith.SymbolTable

var varCount = 0

@JsonIgnoreProperties(value = ["symbolTable"])
sealed interface Statement : ASTNode {
    val symbolTable: SymbolTable
}

@GenNode
data class Declaration(val type: Type, val variableName: String, val value: Expression, override val symbolTable: SymbolTable) : Statement {
    companion object : Randomizeable<Declaration> {
        override fun createRandom(symbolTable: SymbolTable): Declaration {
            val variableName = "var${varCount++}"
            val expression = generateASTNode<Expression>(symbolTable)
            symbolTable[variableName] = expression.toType()
            return Declaration(
                type = expression.toType(),
                variableName = variableName,
                value = expression,
                symbolTable
            )
        }
    }

    override fun toRust(): String {
        return "let mut $variableName: ${type.toRust()} = ${value.toRust()};"
    }
}

@GenNode
data class Assignment(val variableName: String, val value: Expression, override val symbolTable: SymbolTable) : Statement {
    companion object : Randomizeable<Assignment> {
        override fun createRandom(symbolTable: SymbolTable): Assignment? {
            val (variableName, identifierData) = symbolTable.getRandomVariable() ?: return null
            val expression = generateASTNode<Expression>(symbolTable, { it.toType() == identifierData.type })
            return Assignment(
                variableName = variableName,
                value = expression,
                symbolTable
            )
        }
    }

    override fun toRust(): String {
        return "$variableName = ${value.toRust()};"
    }
}

@GenNode(1)
data class ChainedStatement(val s1: Statement, val s2: Statement, override val symbolTable: SymbolTable) : Statement {

    companion object : Randomizeable<ChainedStatement> {
        override fun createRandom(symbolTable: SymbolTable): ChainedStatement {
            return ChainedStatement(generateASTNode(symbolTable), generateASTNode(symbolTable), symbolTable)
        }
    }

    override fun toRust(): String {
        return "${s1.toRust()}\n${s2.toRust()}"
    }
}

data class Output(override val symbolTable: SymbolTable) : Statement {

    override fun toRust(): String {
        val hashString = mutableListOf<String>()
        symbolTable.getCurrentVariables().sorted().forEach {
            hashString.add("println!(\"{:?}\", ${"\"$it\"" to it});")
        }
        return hashString.joinToString("\n")
    }
}
