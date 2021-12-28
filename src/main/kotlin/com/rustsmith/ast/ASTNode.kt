package com.rustsmith.ast

import com.fasterxml.jackson.annotation.JsonTypeInfo
import com.rustsmith.Random
import com.rustsmith.SymbolTable
import kotlin.reflect.KClass
import kotlin.reflect.full.companionObjectInstance
import kotlin.reflect.full.findAnnotation
import kotlin.reflect.full.isSubclassOf

interface Randomizeable<T> {
    fun createRandom(symbolTable: SymbolTable, type: Type): T?
}

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
        return functions.joinToString("\\n") { it.toRust() }
    }
}

fun generateMain(): FunctionDefinition {
    val symbolTable = SymbolTable()
    val body = ChainedStatement(generateStatement(symbolTable), Output(symbolTable), symbolTable)
    val emptySymbolTable = SymbolTable()
    val declarations = body.symbolTable.getVirtualVariables().map {
        Declaration(
            it.second.type, it.first, it.second.type.generateLiteral(symbolTable), emptySymbolTable
        )
    }
    symbolTable.cleanupVirtualVariables()
    val finalBody = if (declarations.isEmpty()) {
        body
    } else ChainedStatement(ChainedStatement.createFromList(declarations, emptySymbolTable), body, symbolTable)
    return FunctionDefinition(
        functionName = "main",
        arguments = emptyMap(),
        body = finalBody
    )
}

fun <T : Any> KClass<T>.subclasses(): List<KClass<out T>> {
    if (this.isFinal) {
        return listOf(this)
    }
    val nonFinalClasses = this.sealedSubclasses.filter { !it.isFinal }
    val result = mutableListOf<KClass<out T>>()
    for (nonFinalClass in nonFinalClasses) {
        result.addAll(nonFinalClass.subclasses())
    }
    return this.sealedSubclasses.filter { it.isFinal } + result
}

fun generateExpression(symbolTable: SymbolTable, type: Type): Expression {
    val classes = Expression::class.genSubClasses().filter {
        it.findAnnotation<ExpressionGenNode>()?.compatibleType?.genSubClasses()?.contains(type::class) ?: false
    }
    var node: Expression?
    do {
        val chosenClass = classes.random(Random)
        node = (chosenClass.companionObjectInstance as Randomizeable<*>).createRandom(symbolTable, type) as Expression?
    } while (node == null)
    return node
}

fun generateStatement(symbolTable: SymbolTable): Statement {
    val classes = Statement::class.genSubClasses() + Expression::class.genSubClasses()
    val chosenClass = classes.random(Random)
    if (chosenClass.isSubclassOf(Expression::class)) {
        val expressionType = chosenClass.findAnnotation<ExpressionGenNode>()!!.compatibleType.genSubClasses().random(
            Random
        ).objectInstance!!
        return ExpressionStatement(
            (chosenClass.companionObjectInstance as Randomizeable<*>).createRandom(symbolTable, expressionType) as Expression, symbolTable
        )
    }
    return (chosenClass.companionObjectInstance as RandomStatFactory<*>).createRandom(symbolTable) as Statement
}
