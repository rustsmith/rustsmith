package com.rustsmith.ast

import com.rustsmith.SymbolTable
import java.math.BigInteger

sealed interface Type : ASTNode

sealed interface Number : Type

sealed interface IntType : Number

@GenNode(10)
object I8Type : IntType {
    override fun toRust(): String {
        return "i8"
    }
}

@GenNode
object I16Type : IntType {
    override fun toRust(): String {
        return "i16"
    }
}

@GenNode
object I32Type : IntType {
    override fun toRust(): String {
        return "i32"
    }
}

@GenNode
object I64Type : IntType {
    override fun toRust(): String {
        return "i64"
    }
}

@GenNode
object I128Type : IntType {
    override fun toRust(): String {
        return "i128"
    }
}

sealed interface FloatType : Number

@GenNode
object F32Type : FloatType {
    override fun toRust(): String {
        return "f32"
    }
}

@GenNode
object F64Type : FloatType {
    override fun toRust(): String {
        return "f64"
    }
}

@GenNode
object StringType : Type {
    override fun toRust(): String {
        return "&str"
    }
}

@GenNode
object BoolType : Type {
    override fun toRust(): String {
        return "bool"
    }
}

object VoidType : Type {
    override fun toRust(): String {
        return "()"
    }
}

fun Number.zero(symbolTable: SymbolTable): Expression {
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
