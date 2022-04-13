package com.rustsmith.ast

import com.rustsmith.subclasses
import java.math.BigInteger
import kotlin.reflect.KClass
import kotlin.reflect.full.hasAnnotation

sealed interface Type : ASTNode

sealed interface BitWiseCompatibleType : Type

sealed interface NumberType : Type

sealed interface IntType : NumberType, BitWiseCompatibleType

@GenNode
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

sealed interface FloatType : NumberType

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
        return "String"
    }
}

@GenNode
object BoolType : BitWiseCompatibleType {
    override fun toRust(): String {
        return "bool"
    }
}

@GenNode
data class TupleType(val types: List<Pair<Type, Boolean>>) : Type {
    override fun toRust(): String {
        return "(${types.joinToString(",") { it.first.toRust() }})"
    }
}

@GenNode
data class StructType(val structName: String, val types: List<Triple<String, Type, Boolean>>) : Type {
    override fun toRust(): String {
        return structName
    }
}

data class FunctionType(val returnType: Type, val args: List<Type>) : Type {
    override fun toRust(): String {
        return "fn(${args.joinToString(",") { it.toRust() }}) -> ${returnType.toRust()}"
    }
}

object VoidType : Type {
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

enum class OwnershipModel {
    COPY,
    MOVE
}

fun Type.getOwnership(): OwnershipModel {
    return when (this) {
        BoolType -> OwnershipModel.COPY
        I8Type -> OwnershipModel.COPY
        I16Type -> OwnershipModel.COPY
        I32Type -> OwnershipModel.COPY
        I64Type -> OwnershipModel.COPY
        I128Type -> OwnershipModel.COPY
        F32Type -> OwnershipModel.COPY
        F64Type -> OwnershipModel.COPY
        StringType -> OwnershipModel.MOVE
        is TupleType -> this.types.map { it.first.getOwnership() }
            .firstOrNull { it == OwnershipModel.MOVE } ?: OwnershipModel.COPY
        is StructType -> OwnershipModel.MOVE
        is FunctionType -> OwnershipModel.COPY
        VoidType -> OwnershipModel.COPY
    }
}

fun KClass<out Type>.genSubClasses(): List<KClass<out Type>> {
    return this.subclasses().filter { it.hasAnnotation<GenNode>() }
}
