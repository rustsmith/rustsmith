package com.rustsmith.ast

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import com.rustsmith.Random
import com.rustsmith.SymbolTable
import kotlin.reflect.KClass
import kotlin.reflect.full.hasAnnotation

var varCount = 0

@JsonIgnoreProperties(value = ["symbolTable"])
sealed interface Statement : ASTNode {
    val symbolTable: SymbolTable
}

interface RandomStatFactory<T> {
    fun createRandom(symbolTable: SymbolTable): T
}

annotation class GenNode

data class ExpressionStatement(val expression: Expression, override val symbolTable: SymbolTable) : Statement {
    override fun toRust(): String {
        return "${expression.toRust()};"
    }
}

@GenNode
data class Declaration(val type: Type, val variableName: String, val value: Expression, override val symbolTable: SymbolTable) : Statement {
    companion object : RandomStatFactory<Declaration> {
        override fun createRandom(symbolTable: SymbolTable): Declaration {
            val variableName = "var${varCount++}"
            val declarationType = Type::class.genSubClasses().random(Random).objectInstance!!
            val expression = generateExpression(symbolTable, declarationType)
            symbolTable[variableName] = expression.toType()
            return Declaration(
                type = declarationType,
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
    companion object : RandomStatFactory<Assignment> {
        override fun createRandom(symbolTable: SymbolTable): Assignment {
            val (variableName, identifierData) = symbolTable.getRandomVariable() ?: symbolTable.setVirtualVariable(Type::class.genSubClasses().random(Random).objectInstance!!)
            val expression = generateExpression(symbolTable, identifierData.type)
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

@GenNode
data class ChainedStatement(val s1: Statement, val s2: Statement, override val symbolTable: SymbolTable) : Statement {

    companion object : RandomStatFactory<ChainedStatement> {
        override fun createRandom(symbolTable: SymbolTable): ChainedStatement {
            return ChainedStatement(generateStatement(symbolTable), generateStatement(symbolTable), symbolTable)
        }

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

data class Output(override val symbolTable: SymbolTable) : Statement {

    override fun toRust(): String {
        val hashString = mutableListOf<String>()
        symbolTable.getCurrentVariables().sorted().forEach {
            hashString.add("println!(\"{:?}\", ${"\"$it\"" to it});")
        }
        return hashString.joinToString("\n")
    }
}

fun KClass<out Statement>.genSubClasses(): List<KClass<out Statement>> {
    return this.subclasses().filter { it.hasAnnotation<GenNode>() }
}
