package com.rustsmith.ast

import com.rustsmith.CustomRandom
import com.rustsmith.subclasses
import java.math.BigInteger
import kotlin.random.nextUInt
import kotlin.reflect.KClass
import kotlin.reflect.full.hasAnnotation

sealed interface Type : ASTNode {
    fun clone(): Type

    fun memberTypes(): List<Type>

    fun lifetimeParameters(): List<UInt>
}

sealed interface NonVoidType : Type

sealed interface CLIInputType : NonVoidType
sealed interface BitWiseCompatibleType : NonVoidType

sealed interface NumberType : NonVoidType, CLIInputType

sealed interface IntType : NumberType, BitWiseCompatibleType

data class LifetimeParameterizedType<T : Type>(val type: T) : Type {
    override fun toRust(): String {
        if (type is StructType) {
            return type.copy(structName = "${type.structName}<${type.lifetimeParameters().toSet().joinToString(",") { "'a$it" }}>").toRust()
        }
        if (type is MutableReferenceType) {
            return "&'a${type.lifetimeParameter} mut ${type.internalType.toRust()}"
        }
        if (type is ReferenceType) {
            return "&'a${type.lifetimeParameter} ${type.internalType.toRust()}"
        }
        return type.toRust()
    }

    override fun clone(): Type {
        return LifetimeParameterizedType(type.clone())
    }

    override fun memberTypes(): List<Type> {
        return type.memberTypes()
    }

    override fun lifetimeParameters(): List<UInt> {
        return type.lifetimeParameters()
    }
}

@GenNode
object I8Type : IntType {
    override fun toRust(): String {
        return "i8"
    }

    override fun memberTypes(): List<Type> = listOf(I8Type)

    override fun lifetimeParameters(): List<UInt> = listOf()

    override fun clone() = I8Type
}

@GenNode
object I16Type : IntType {
    override fun toRust(): String {
        return "i16"
    }

    override fun memberTypes(): List<Type> = listOf(I16Type)

    override fun lifetimeParameters(): List<UInt> = listOf()

    override fun clone() = I16Type
}

@GenNode
object I32Type : IntType {
    override fun toRust(): String {
        return "i32"
    }

    override fun memberTypes(): List<Type> = listOf(I32Type)

    override fun lifetimeParameters(): List<UInt> = listOf()

    override fun clone() = I32Type
}

@GenNode
object I64Type : IntType {
    override fun toRust(): String {
        return "i64"
    }

    override fun memberTypes(): List<Type> = listOf(I64Type)

    override fun lifetimeParameters(): List<UInt> = listOf()

    override fun clone() = I64Type
}

@GenNode
object I128Type : IntType {
    override fun toRust(): String {
        return "i128"
    }

    override fun memberTypes(): List<Type> = listOf(I128Type)

    override fun lifetimeParameters(): List<UInt> = listOf()

    override fun clone() = I128Type
}

sealed interface FloatType : NumberType

@GenNode
object F32Type : FloatType {
    override fun toRust(): String {
        return "f32"
    }

    override fun memberTypes(): List<Type> = listOf(F32Type)

    override fun lifetimeParameters(): List<UInt> = listOf()

    override fun clone() = F32Type
}

@GenNode
object F64Type : FloatType {
    override fun toRust(): String {
        return "f64"
    }

    override fun memberTypes(): List<Type> = listOf(F64Type)

    override fun lifetimeParameters(): List<UInt> = listOf()

    override fun clone() = F64Type
}

@GenNode
object StringType : CLIInputType, NonVoidType {
    override fun toRust(): String {
        return "String"
    }

    override fun memberTypes(): List<Type> = listOf(StringType)

    override fun lifetimeParameters(): List<UInt> = listOf()

    override fun clone() = StringType
}

@GenNode
object BoolType : CLIInputType, BitWiseCompatibleType {
    override fun toRust(): String {
        return "bool"
    }

    override fun memberTypes(): List<Type> = listOf(BoolType)

    override fun lifetimeParameters(): List<UInt> = listOf()

    override fun clone() = BoolType
}

sealed interface RecursiveType : NonVoidType {
    val argumentsToOwnershipMap: MutableList<Pair<Type, OwnershipState>>
}

sealed interface ContainerType : RecursiveType

@GenNode
data class TupleType(val types: List<Type>) : RecursiveType, ContainerType {
    override val argumentsToOwnershipMap = types.map { it to OwnershipState.VALID }.toMutableList()

    override fun toRust(): String {
        return "(${types.joinToString(",") { it.toRust() }})"
    }

    override fun memberTypes(): List<Type> = types.flatMap { it.memberTypes() } + this

    override fun lifetimeParameters(): List<UInt> = types.flatMap { it.lifetimeParameters() }

    override fun clone() = TupleType(types.map { it.clone() })
}

@GenNode
data class StructType(val structName: String, val types: List<Pair<String, Type>>) : RecursiveType, ContainerType {
    override val argumentsToOwnershipMap = types.map { it.second to OwnershipState.VALID }.toMutableList()

    override fun toRust(): String {
        return structName
    }

    override fun memberTypes(): List<Type> = types.flatMap { it.second.memberTypes() } + this

    override fun lifetimeParameters(): List<UInt> = types.flatMap { it.second.lifetimeParameters() }

    override fun clone() = StructType(structName, types.map { it.first to it.second.clone() })
}

sealed interface ReferencingTypes : NonVoidType {
    val lifetimeParameter: UInt
    val internalType: Type

    fun withParameterized(lifetimeParameter: UInt): ReferencingTypes
}

@GenNode
data class ReferenceType(
    override val internalType: Type,
    override val lifetimeParameter: UInt = CustomRandom.nextUInt()
) : ReferencingTypes {
    override fun withParameterized(lifetimeParameter: UInt): ReferencingTypes {
        return ReferenceType(internalType.clone(), lifetimeParameter)
    }

    override fun clone(): Type {
        return ReferenceType(internalType.clone())
    }

    override fun toRust(): String {
        return "&${internalType.toRust()}"
    }

    override fun lifetimeParameters(): List<UInt> {
        return internalType.lifetimeParameters() + lifetimeParameter
    }

    override fun memberTypes(): List<Type> = internalType.memberTypes() + this
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as ReferenceType

        if (internalType != other.internalType) return false

        return true
    }

    override fun hashCode(): Int {
        return internalType.hashCode()
    }
}

@GenNode
data class MutableReferenceType(
    override val internalType: Type,
    override val lifetimeParameter: UInt = CustomRandom.nextUInt()
) : ReferencingTypes {
    override fun withParameterized(lifetimeParameter: UInt): ReferencingTypes {
        return MutableReferenceType(internalType.clone(), lifetimeParameter)
    }

    override fun clone(): Type {
        return MutableReferenceType(internalType.clone())
    }

    override fun toRust(): String {
        return "&mut ${internalType.toRust()}"
    }

    override fun memberTypes(): List<Type> = internalType.memberTypes() + this

    override fun lifetimeParameters(): List<UInt> {
        return internalType.lifetimeParameters() + lifetimeParameter
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as MutableReferenceType

        if (internalType != other.internalType) return false

        return true
    }

    override fun hashCode(): Int {
        return internalType.hashCode()
    }
}

data class FunctionType(val returnType: Type, val args: List<Type>) : NonVoidType {
    override fun toRust(): String {
        return "fn(${args.joinToString(",") { it.toRust() }}) -> ${returnType.toRust()}"
    }

    override fun memberTypes(): List<Type> = args.flatMap { it.memberTypes() } + this

    override fun lifetimeParameters(): List<UInt> {
        return listOf()
    }

    override fun clone() = FunctionType(returnType.clone(), args.map { it.clone() })
}

@GenNode
object VoidType : Type {
    override fun toRust(): String {
        return "()"
    }

    override fun clone() = VoidType

    override fun lifetimeParameters(): List<UInt> = listOf()

    override fun memberTypes(): List<Type> {
        return listOf(this)
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
    COPY, MOVE
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
        is TupleType -> this.types.map { it.getOwnership() }.firstOrNull { it == OwnershipModel.MOVE }
            ?: OwnershipModel.COPY
        is StructType -> OwnershipModel.MOVE
        is FunctionType -> OwnershipModel.COPY
        VoidType -> OwnershipModel.COPY
        is ReferenceType -> OwnershipModel.COPY
        is MutableReferenceType -> OwnershipModel.MOVE
        is LifetimeParameterizedType<*> -> this.type.getOwnership()
    }
}

fun KClass<out Type>.genSubClasses(): List<KClass<out Type>> {
    return this.subclasses().filter { it.hasAnnotation<GenNode>() }
}
