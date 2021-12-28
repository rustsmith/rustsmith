package com.rustsmith.ast

import com.rustsmith.SymbolTable
import java.math.BigInteger
import kotlin.reflect.KClass
import kotlin.reflect.full.hasAnnotation

sealed interface Type : ASTNode {
    fun generateLiteral(symbolTable: SymbolTable): Expression
}

sealed interface NumberType : Type

sealed interface IntType : NumberType

@GenNode
object I8Type : IntType {
    override fun generateLiteral(symbolTable: SymbolTable): Expression {
        return Int8Literal.createRandom(symbolTable, this)
    }

    override fun toRust(): String {
        return "i8"
    }
}

@GenNode
object I16Type : IntType {
    override fun generateLiteral(symbolTable: SymbolTable): Expression {
        return Int16Literal.createRandom(symbolTable, this)
    }

    override fun toRust(): String {
        return "i16"
    }
}

@GenNode
object I32Type : IntType {
    override fun generateLiteral(symbolTable: SymbolTable): Expression {
        return Int32Literal.createRandom(symbolTable, this)
    }

    override fun toRust(): String {
        return "i32"
    }
}

@GenNode
object I64Type : IntType {
    override fun generateLiteral(symbolTable: SymbolTable): Expression {
        return Int64Literal.createRandom(symbolTable, this)
    }

    override fun toRust(): String {
        return "i64"
    }
}

@GenNode
object I128Type : IntType {
    override fun generateLiteral(symbolTable: SymbolTable): Expression {
        return Int128Literal.createRandom(symbolTable, this)
    }

    override fun toRust(): String {
        return "i128"
    }
}

sealed interface FloatType : NumberType

@GenNode
object F32Type : FloatType {
    override fun generateLiteral(symbolTable: SymbolTable): Expression {
        return Float32Literal.createRandom(symbolTable, this)
    }

    override fun toRust(): String {
        return "f32"
    }
}

@GenNode
object F64Type : FloatType {
    override fun generateLiteral(symbolTable: SymbolTable): Expression {
        return Float64Literal.createRandom(symbolTable, this)
    }

    override fun toRust(): String {
        return "f64"
    }
}

@GenNode
object StringType : Type {
    override fun generateLiteral(symbolTable: SymbolTable): Expression {
        return StringLiteral.createRandom(symbolTable, this)
    }

    override fun toRust(): String {
        return "&str"
    }
}

@GenNode
object BoolType : Type {
    override fun generateLiteral(symbolTable: SymbolTable): Expression {
        return BooleanLiteral.createRandom(symbolTable, this)
    }

    override fun toRust(): String {
        return "bool"
    }
}

object VoidType : Type {
    override fun generateLiteral(symbolTable: SymbolTable): Expression {
        TODO("Not yet implemented")
    }

    override fun toRust(): String {
        return "()"
    }
}

fun NumberType.zero(symbolTable: SymbolTable): Expression {
    return when (this) {
        F32Type -> Float32Literal(0.0.toFloat(), symbolTable)
        F64Type -> Float64Literal(0.0, symbolTable)
        I8Type -> Int8Literal(0, symbolTable = symbolTable)
        I16Type -> Int16Literal(0, symbolTable = symbolTable)
        I32Type -> Int32Literal(0, symbolTable = symbolTable)
        I64Type -> Int64Literal(0, symbolTable = symbolTable)
        I128Type -> Int128Literal(BigInteger.ZERO, symbolTable = symbolTable)
    }
}

fun KClass<out Type>.genSubClasses(): List<KClass<out Type>> {
    return this.subclasses().filter { it.hasAnnotation<GenNode>() }
}
