package com.rustsmith.ast

import com.fasterxml.jackson.annotation.JsonTypeInfo
import com.rustsmith.Random
import com.rustsmith.SymbolTable
import kotlin.reflect.KClass
import kotlin.reflect.full.companionObjectInstance
import kotlin.reflect.full.findAnnotation
import kotlin.reflect.full.hasAnnotation

annotation class GenNode(val weight: Int = 1)

interface Randomizeable<T> {
    fun createRandom(symbolTable: SymbolTable): ASTNode?
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

data class Program(val seed: Long, val structs: List<Any> = emptyList(), val functions: List<FunctionDefinition>) : ASTNode {
    override fun toRust(): String {
        return functions.joinToString("\\n") { it.toRust() }
    }
}

fun generateMain(): FunctionDefinition {
    val symbolTable = SymbolTable()
    val body = generateASTNode<Statement>(symbolTable)
    return FunctionDefinition(functionName = "main", arguments = emptyMap(), body = ChainedStatement(body, Output(symbolTable), symbolTable))
}

fun <T : ASTNode> generateSubClassList(kClass: KClass<out T>): List<KClass<out T>> {
    val nonFinalClasses = kClass.sealedSubclasses.filter { !it.isFinal }
    val result = mutableListOf<KClass<out T>>()
    for (nonFinalClass in nonFinalClasses) {
        result.addAll(generateSubClassList(nonFinalClass))
    }
    return kClass.sealedSubclasses.filter { it.isFinal && it.hasAnnotation<GenNode>() } + result
}

inline fun <reified T : ASTNode> generateASTNode(symbolTable: SymbolTable, predicate: (t: T) -> Boolean = { true }, kClassPredicate: (t: KClass<out T>) -> Boolean = { true }): T {
    val classes = generateSubClassList(T::class).flatMap { kClass -> List(kClass.findAnnotation<GenNode>()!!.weight) { kClass } }
    var node: T? = null
    do {
        val kClass = classes.random(Random)
        if (!kClassPredicate(kClass)) continue
        node = (kClass.companionObjectInstance as Randomizeable<*>).createRandom(symbolTable) as T?
    } while (node == null || !predicate(node))
    return node
}
