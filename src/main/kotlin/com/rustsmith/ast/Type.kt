package com.rustsmith.ast

import com.rustsmith.subclasses
import java.math.BigInteger
import kotlin.reflect.KClass
import kotlin.reflect.full.hasAnnotation

sealed interface Type : ASTNode {
    fun clone(): Type

    fun snapshot(): Type = clone()

    fun memberTypes(): List<Type>

    fun lifetimeParameters(): List<UInt>
}

sealed interface NonVoidType : Type

sealed interface LiteralType : NonVoidType

sealed interface BitWiseCompatibleType : NonVoidType

sealed interface ComparableType : Type

sealed interface EqType : Type

sealed interface NumberType : NonVoidType, LiteralType, EqType, ComparableType

sealed interface IntType : NumberType, BitWiseCompatibleType

sealed interface UIntType : NumberType, BitWiseCompatibleType

data class LifetimeParameterizedType<T : Type>(val type: T) : Type {
    override fun toRust(): String {
        if (type is StructType) {
            return type.copy(
                structName = "${type.structName}<${
                type.lifetimeParameters().toSet().joinToString(",") { "'a$it" }
                }>"
            ).toRust()
        }
        if (type is MutableReferenceType) {
            return "&'a${type.lifetimeParameter} mut ${type.internalType.toRust()}"
        }
        if (type is ReferenceType) {
            return "&'a${type.lifetimeParameter} ${type.internalType.toRust()}"
        }
        if (type is TypeAliasType) {
            return type.copy(
                typeAliasName = "${type.typeAliasName}<${
                type.lifetimeParameters().toSet().joinToString(",") { "'a$it" }
                }>"
            ).toRust()
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

@GenNode
object U8Type : UIntType {
    override fun toRust(): String {
        return "u8"
    }

    override fun memberTypes(): List<Type> = listOf(U8Type)

    override fun lifetimeParameters(): List<UInt> = listOf()

    override fun clone() = U8Type
}

@GenNode
object U16Type : UIntType {
    override fun toRust(): String {
        return "u16"
    }

    override fun memberTypes(): List<Type> = listOf(U16Type)

    override fun lifetimeParameters(): List<UInt> = listOf()

    override fun clone() = U16Type
}

@GenNode
object U32Type : UIntType {
    override fun toRust(): String {
        return "u32"
    }

    override fun memberTypes(): List<Type> = listOf(U32Type)

    override fun lifetimeParameters(): List<UInt> = listOf()

    override fun clone() = U32Type
}

@GenNode
object U64Type : UIntType {
    override fun toRust(): String {
        return "u64"
    }

    override fun memberTypes(): List<Type> = listOf(U64Type)

    override fun lifetimeParameters(): List<UInt> = listOf()

    override fun clone() = U64Type
}

@GenNode
object U128Type : UIntType {
    override fun toRust(): String {
        return "u128"
    }

    override fun memberTypes(): List<Type> = listOf(U128Type)

    override fun lifetimeParameters(): List<UInt> = listOf()

    override fun clone() = U128Type
}

@GenNode
object USizeType : UIntType {
    override fun toRust(): String {
        return "usize"
    }

    override fun memberTypes(): List<Type> = listOf(USizeType)

    override fun lifetimeParameters(): List<UInt> = listOf()

    override fun clone() = USizeType
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
object StringType : LiteralType, NonVoidType, EqType {
    override fun toRust(): String {
        return "String"
    }

    override fun memberTypes(): List<Type> = listOf(StringType)

    override fun lifetimeParameters(): List<UInt> = listOf()

    override fun clone() = StringType
}

@GenNode
object BoolType : LiteralType, BitWiseCompatibleType, EqType {
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
data class TupleType(
    val types: List<Type>,
    override val argumentsToOwnershipMap: MutableList<Pair<Type, OwnershipState>> = types.map { it to OwnershipState.VALID }
        .toMutableList()
) : RecursiveType, ContainerType {
    override fun toRust(): String {
        return "(${types.joinToString(",") { it.toRust() }})"
    }

    override fun memberTypes(): List<Type> = types.flatMap { it.memberTypes() } + this

    override fun lifetimeParameters(): List<UInt> = types.flatMap { it.lifetimeParameters() }

    override fun clone() = TupleType(types.map { it.clone() })

    override fun snapshot(): Type = TupleType(
        types.map { it.snapshot() },
        argumentsToOwnershipMap.map { it.first.snapshot() to it.second }.toMutableList()
    )

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as TupleType

        if (types != other.types) return false

        return true
    }

    override fun hashCode(): Int {
        return types.hashCode()
    }
}

@GenNode
data class StructType(
    val structName: String,
    val types: List<Pair<String, Type>>,
    override val argumentsToOwnershipMap: MutableList<Pair<Type, OwnershipState>> = types.map { it.second to OwnershipState.VALID }
        .toMutableList()
) : RecursiveType, ContainerType {
    override fun toRust(): String {
        return structName
    }

    override fun memberTypes(): List<Type> = types.flatMap { it.second.memberTypes() } + this

    override fun lifetimeParameters(): List<UInt> = types.flatMap { it.second.lifetimeParameters() }

    override fun clone() = StructType(structName, types.map { it.first to it.second.clone() })

    override fun snapshot(): StructType = StructType(
        structName,
        types.map { it.first to it.second.snapshot() },
        argumentsToOwnershipMap.map { it.first.snapshot() to it.second }.toMutableList()
    )

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as StructType

        if (structName != other.structName) return false
        if (types != other.types) return false

        return true
    }

    override fun hashCode(): Int {
        var result = structName.hashCode()
        result = 31 * result + types.hashCode()
        return result
    }
}

@GenNode
data class VectorType(val type: Type) : RecursiveType, ContainerType {
    override val argumentsToOwnershipMap: MutableList<Pair<Type, OwnershipState>> = mutableListOf()
    override fun toRust(): String {
        return "Vec<${type.toRust()}>"
    }

    override fun memberTypes(): List<Type> = listOf(this) + this.type.memberTypes()

    override fun lifetimeParameters(): List<UInt> = type.lifetimeParameters()

    override fun clone() = VectorType(type.clone())

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as VectorType

        if (type != other.type) return false

        return true
    }

    override fun hashCode(): Int {
        return type.hashCode()
    }
}

sealed interface ReferencingTypes : NonVoidType {
    val lifetimeParameter: UInt
    val internalType: Type

    fun withParameterized(lifetimeParameter: UInt): ReferencingTypes
}

@GenNode
data class ReferenceType(
    override val internalType: Type,
    override val lifetimeParameter: UInt
) : ReferencingTypes {
    override fun withParameterized(lifetimeParameter: UInt): ReferencingTypes {
        return ReferenceType(internalType.clone(), lifetimeParameter)
    }

    override fun clone(): Type {
        return ReferenceType(internalType.clone(), lifetimeParameter)
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
    override val lifetimeParameter: UInt
) : ReferencingTypes {
    override fun withParameterized(lifetimeParameter: UInt): ReferencingTypes {
        return MutableReferenceType(internalType.clone(), lifetimeParameter)
    }

    override fun clone(): Type {
        return MutableReferenceType(internalType.clone(), lifetimeParameter)
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

@GenNode
data class StaticSizedArrayType(val internalType: Type, val size: UInt) : ContainerType {
    override val argumentsToOwnershipMap: MutableList<Pair<Type, OwnershipState>> = mutableListOf()

    override fun clone(): Type = StaticSizedArrayType(internalType.clone(), size)
    override fun memberTypes(): List<Type> = listOf(this) + internalType.memberTypes()
    override fun lifetimeParameters(): List<UInt> = internalType.lifetimeParameters()

    override fun toRust(): String {
        return "[${internalType.toRust()}; $size]"
    }
}

@GenNode
data class BoxType(val internalType: Type) : RecursiveType {
    override val argumentsToOwnershipMap: MutableList<Pair<Type, OwnershipState>> = mutableListOf()

    override fun clone(): Type = BoxType(internalType.clone())
    override fun memberTypes(): List<Type> = listOf(this) + internalType.memberTypes()
    override fun lifetimeParameters(): List<UInt> = internalType.lifetimeParameters()

    override fun toRust(): String {
        return "Box<${internalType.toRust()}>"
    }
}

@GenNode
data class TypeAliasType(val typeAliasName: String, val internalType: Type) : RecursiveType {
    override val argumentsToOwnershipMap: MutableList<Pair<Type, OwnershipState>> = mutableListOf()

    override fun clone(): Type = TypeAliasType(typeAliasName, internalType.clone())
    override fun memberTypes(): List<Type> = listOf(this) + internalType.memberTypes()
    override fun lifetimeParameters(): List<UInt> = internalType.lifetimeParameters()

    override fun toRust(): String {
        return typeAliasName
    }
}

object DefaultHasher : Type {
    override fun clone() = DefaultHasher
    override fun memberTypes() = listOf<Type>()
    override fun lifetimeParameters() = listOf<UInt>()

    override fun toRust() = "DefaultHasher"
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
        U8Type -> UInt8Literal(0u, symbolTable = symbolTable)
        U16Type -> UInt16Literal(0u, symbolTable = symbolTable)
        U32Type -> UInt32Literal(0u, symbolTable = symbolTable)
        U64Type -> UInt64Literal(0uL, symbolTable = symbolTable)
        U128Type -> UInt128Literal(BigInteger.ZERO, symbolTable = symbolTable)
        USizeType -> USizeLiteral(0uL, symbolTable = symbolTable)
    }
}

enum class OwnershipModel {
    COPY, MOVE
}

fun Type.getOwnership(): OwnershipModel {
    return when (this) {
        BoolType -> OwnershipModel.COPY
        is NumberType -> OwnershipModel.COPY
        StringType -> OwnershipModel.MOVE
        is TupleType -> this.types.map { it.getOwnership() }.firstOrNull { it == OwnershipModel.MOVE }
            ?: OwnershipModel.COPY
        is StructType -> OwnershipModel.MOVE
        is FunctionType -> OwnershipModel.COPY
        VoidType -> OwnershipModel.COPY
        is ReferenceType -> OwnershipModel.COPY
        is MutableReferenceType -> OwnershipModel.MOVE
        is LifetimeParameterizedType<*> -> this.type.getOwnership()
        is VectorType -> OwnershipModel.MOVE
        is BoxType -> OwnershipModel.MOVE
        DefaultHasher -> OwnershipModel.MOVE
        is TypeAliasType -> this.internalType.getOwnership()
        is StaticSizedArrayType -> this.internalType.getOwnership()
    }
}

fun KClass<out Type>.genSubClasses(): List<KClass<out Type>> {
    return this.subclasses().filter { it.hasAnnotation<GenNode>() }
}
