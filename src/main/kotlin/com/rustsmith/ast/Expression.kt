package com.rustsmith.ast

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import com.rustsmith.Random
import com.rustsmith.SymbolTable
import java.math.BigInteger
import kotlin.random.asJavaRandom
import kotlin.reflect.KClass
import kotlin.reflect.full.hasAnnotation

annotation class ExpressionGenNode(val compatibleType: KClass<out Type>)

@JsonIgnoreProperties(value = ["symbolTable"])
sealed interface Expression : Statement

@ExpressionGenNode(I8Type::class)
data class Int8Literal(val value: Int, override val symbolTable: SymbolTable) : Expression {
    companion object : Randomizeable<Int8Literal> {
        override fun createRandom(symbolTable: SymbolTable, type: Type): Int8Literal {
            return Int8Literal(
                value = Random.nextBits(7),
                symbolTable
            )
        }
    }

    override fun toRust(): String {
        return "${value}i8"
    }
}

@ExpressionGenNode(I16Type::class)
data class Int16Literal(val value: Int, override val symbolTable: SymbolTable) : Expression {

    companion object : Randomizeable<Int16Literal> {
        override fun createRandom(symbolTable: SymbolTable, type: Type): Int16Literal {
            return Int16Literal(
                value = Random.nextBits(15),
                symbolTable
            )
        }
    }

    override fun toRust(): String {
        return "${value}i16"
    }
}

@ExpressionGenNode(I32Type::class)
data class Int32Literal(val value: Int, override val symbolTable: SymbolTable) :
    Expression {

    companion object : Randomizeable<Int32Literal> {
        override fun createRandom(symbolTable: SymbolTable, type: Type): Int32Literal {
            return Int32Literal(
                value = Random.nextInt(),
                symbolTable
            )
        }
    }

    override fun toRust(): String {
        return "${value}i32"
    }
}

@ExpressionGenNode(I64Type::class)
data class Int64Literal(val value: Long, override val symbolTable: SymbolTable) :
    Expression {

    companion object : Randomizeable<Int64Literal> {
        override fun createRandom(symbolTable: SymbolTable, type: Type): Int64Literal {
            return Int64Literal(
                value = Random.nextLong(),
                symbolTable
            )
        }
    }

    override fun toRust(): String {
        return "${value}i64"
    }
}

@ExpressionGenNode(I128Type::class)
data class Int128Literal(val value: BigInteger, override val symbolTable: SymbolTable) :
    Expression {

    companion object : Randomizeable<Int128Literal> {
        override fun createRandom(symbolTable: SymbolTable, type: Type): Int128Literal {
            return Int128Literal(
                value = BigInteger(127, Random.asJavaRandom()),
                symbolTable
            )
        }
    }

    override fun toRust(): String {
        return "${value}i128"
    }
}

@ExpressionGenNode(F32Type::class)
data class Float32Literal(val value: Float, override val symbolTable: SymbolTable) : Expression {
    companion object : Randomizeable<Float32Literal> {
        override fun createRandom(symbolTable: SymbolTable, type: Type): Float32Literal {
            return Float32Literal(value = Random.nextFloat(), symbolTable)
        }
    }

    override fun toRust(): String {
        return "${value}f32"
    }
}

@ExpressionGenNode(F64Type::class)
data class Float64Literal(val value: Double, override val symbolTable: SymbolTable) : Expression {
    companion object : Randomizeable<Float64Literal> {
        override fun createRandom(symbolTable: SymbolTable, type: Type): Float64Literal {
            return Float64Literal(value = Random.nextDouble(), symbolTable)
        }
    }

    override fun toRust(): String {
        return "${value}f64"
    }
}

@ExpressionGenNode(StringType::class)
data class StringLiteral(val value: String, override val symbolTable: SymbolTable) : Expression {

    companion object : Randomizeable<StringLiteral> {
        private val charPool: List<Char> = ('a'..'z') + ('A'..'Z') + ('0'..'9')

        override fun createRandom(symbolTable: SymbolTable, type: Type): StringLiteral {
            return StringLiteral(
                (1..Random.nextInt(100)).map { charPool[Random.nextInt(0, charPool.size)] }
                    .joinToString(""),
                symbolTable
            )
        }
    }

    override fun toRust(): String {
        return "\"$value\""
    }
}

@ExpressionGenNode(BoolType::class)
data class BooleanLiteral(val value: Boolean, override val symbolTable: SymbolTable) : Expression {

    companion object : Randomizeable<BooleanLiteral> {
        override fun createRandom(symbolTable: SymbolTable, type: Type): BooleanLiteral {
            return BooleanLiteral(Random.nextBoolean(), symbolTable)
        }
    }

    override fun toRust(): String {
        return value.toString()
    }
}

@ExpressionGenNode(Type::class)
data class Variable(val value: String, override val symbolTable: SymbolTable) : Expression {

    companion object : Randomizeable<Variable> {
        override fun createRandom(symbolTable: SymbolTable, type: Type): Variable {
            val value = symbolTable.getRandomVariableOfType(type)
            return Variable(value.first, symbolTable)
        }
    }

    override fun toRust(): String {
        return value
    }
}

sealed interface RecursiveExpression : Expression {
    val expr1: Expression
    val expr2: Expression
}

interface RandomizeableBinOp<T : RecursiveExpression> : Randomizeable<T> {
    fun createExpressions(symbolTable: SymbolTable, expressionType: Type): Pair<Expression, Expression>? {
        val depth = Thread.currentThread().stackTrace.size
        if (depth > 50) return null
        val exp1: Expression = generateExpression(symbolTable, expressionType)
        val exp2: Expression = generateExpression(symbolTable, expressionType)
//        {
//            if (depth > 20) !it.isSubclassOf(RecursiveExpression::class) else true
//        }
        return exp1 to exp2
    }
}

@ExpressionGenNode(NumberType::class)
data class AddExpression(
    override val expr1: Expression,
    override val expr2: Expression,
    override val symbolTable: SymbolTable
) : RecursiveExpression {

    companion object : RandomizeableBinOp<AddExpression> {
        override fun createRandom(symbolTable: SymbolTable, type: Type): AddExpression? {
            val (exp1, exp2) = this.createExpressions(symbolTable, type) ?: return null
            return AddExpression(exp1, exp2, symbolTable)
        }
    }

    override fun toRust(): String {
        return "${expr1.toRust()} + ${expr2.toRust()}"
    }
}

@ExpressionGenNode(NumberType::class)
data class SubtractExpression(
    override val expr1: Expression,
    override val expr2: Expression,
    override val symbolTable: SymbolTable
) : RecursiveExpression {

    companion object : RandomizeableBinOp<SubtractExpression> {
        override fun createRandom(symbolTable: SymbolTable, type: Type): SubtractExpression? {
            val (exp1, exp2) = this.createExpressions(symbolTable, type) ?: return null
            return SubtractExpression(exp1, exp2, symbolTable)
        }
    }

    override fun toRust(): String {
        return "${expr1.toRust()} - ${expr2.toRust()}"
    }
}

@ExpressionGenNode(NumberType::class)
data class DivideExpression(
    override val expr1: Expression,
    override val expr2: Expression,
    override val symbolTable: SymbolTable
) : RecursiveExpression {

    companion object : RandomizeableBinOp<DivideExpression> {
        override fun createRandom(symbolTable: SymbolTable, type: Type): DivideExpression? {
            val (exp1, exp2) = this.createExpressions(symbolTable, type) ?: return null
            return DivideExpression(exp1, exp2, symbolTable)
        }
    }

    override fun toRust(): String {
        return "${expr1.toRust()} / ${expr2.toRust()}"
    }
}

@ExpressionGenNode(NumberType::class)
data class MultiplyExpression(
    override val expr1: Expression,
    override val expr2: Expression,
    override val symbolTable: SymbolTable
) : RecursiveExpression {

    companion object : RandomizeableBinOp<MultiplyExpression> {
        override fun createRandom(symbolTable: SymbolTable, type: Type): MultiplyExpression? {
            val (exp1, exp2) = this.createExpressions(symbolTable, type) ?: return null
            return MultiplyExpression(exp1, exp2, symbolTable)
        }
    }

    override fun toRust(): String {
        return "${expr1.toRust()} * ${expr2.toRust()}"
    }
}

@ExpressionGenNode(NumberType::class)
data class ModExpression(
    override val expr1: Expression,
    override val expr2: Expression,
    override val symbolTable: SymbolTable
) : RecursiveExpression {

    companion object : RandomizeableBinOp<ModExpression> {
        override fun createRandom(symbolTable: SymbolTable, type: Type): ModExpression? {
            val (exp1, exp2) = this.createExpressions(symbolTable, type) ?: return null
            return ModExpression(exp1, exp2, symbolTable)
        }
    }

    override fun toRust(): String {
        return "${expr1.toRust()} % ${expr2.toRust()}"
    }
}

// @ExpressionGenNode
// data class BitwiseAndLogicalAnd(
//    override val expr1: Expression,
//    override val expr2: Expression,
//    override val symbolTable: SymbolTable
// ) : RecursiveExpression {
//
//    companion object : RandomizeableBinOp<BitwiseAndLogicalAnd> {
//        override fun createRandom(symbolTable: SymbolTable): BitwiseAndLogicalAnd {
//            val selectedType = listOf(IntType::class, BoolType::class).random(Random)
//            val (exp1, exp2) = this.createExpressions(selectedType, symbolTable)
//            return BitwiseAndLogicalAnd(exp1, exp2, symbolTable)
//        }
//    }
//
//    override fun toRust(): String {
//        return "${expr1.toRust()} & ${expr2.toRust()}"
//    }
// }
//
// @ExpressionGenNode
// data class BitwiseAndLogicalOr(
//    override val expr1: Expression,
//    override val expr2: Expression,
//    override val symbolTable: SymbolTable
// ) : RecursiveExpression {
//
//    companion object : RandomizeableBinOp<BitwiseAndLogicalOr> {
//        override fun createRandom(symbolTable: SymbolTable): BitwiseAndLogicalOr {
//            val selectedType = listOf(IntType::class, BoolType::class).random(Random)
//            val (exp1, exp2) = this.createExpressions(selectedType, symbolTable)
//            return BitwiseAndLogicalOr(exp1, exp2, symbolTable)
//        }
//    }
//
//    override fun toRust(): String {
//        return "${expr1.toRust()} | ${expr2.toRust()}"
//    }
// }
//
// @ExpressionGenNode
// data class BitwiseAndLogicalXor(
//    override val expr1: Expression,
//    override val expr2: Expression,
//    override val symbolTable: SymbolTable
// ) : RecursiveExpression {
//
//    companion object : RandomizeableBinOp<BitwiseAndLogicalXor> {
//        override fun createRandom(symbolTable: SymbolTable): BitwiseAndLogicalXor {
//            val selectedType = listOf(IntType::class, BoolType::class).random(Random)
//            val (exp1, exp2) = this.createExpressions(selectedType, symbolTable)
//            return BitwiseAndLogicalXor(exp1, exp2, symbolTable)
//        }
//    }
//
//    override fun toRust(): String {
//        return "${expr1.toRust()} ^ ${expr2.toRust()}"
//    }
// }

@ExpressionGenNode(Type::class)
data class GroupedExpression(
    val expression: Expression,
    override val symbolTable: SymbolTable
) : Expression {
    companion object : Randomizeable<GroupedExpression> {
        override fun createRandom(symbolTable: SymbolTable, type: Type): GroupedExpression {
            return GroupedExpression(generateExpression(symbolTable, type), symbolTable)
        }
    }

    override fun toRust(): String {
        return "(${expression.toRust()})"
    }
}

// @ExpressionGenNode
// data class ShiftLeftExpression(
//    override val expr1: Expression,
//    override val expr2: Expression,
//    override val symbolTable: SymbolTable
// ) : RecursiveExpression {
//
//    companion object : RandomizeableBinOp<ShiftLeftExpression> {
//        override fun createRandom(symbolTable: SymbolTable): ShiftLeftExpression {
//            val (expr1, expr2) = createExpressions(IntType::class, symbolTable)
//            return ShiftLeftExpression(expr1, expr1, symbolTable)
//        }
//    }
//
//    override fun toRust(): String {
//        return "${expr1.toRust()} << ${expr2.toRust()}"
//    }
// }

sealed interface ReconditionedExpression : Expression

data class WrappingAdd(val addExpression: AddExpression, override val symbolTable: SymbolTable) :
    ReconditionedExpression {
    override fun toRust(): String {
        return "${addExpression.expr1.toRust()}.wrapping_add(${addExpression.expr2.toRust()})"
    }
}

data class WrappingMul(val multiplyExpression: MultiplyExpression, override val symbolTable: SymbolTable) :
    ReconditionedExpression {
    override fun toRust(): String {
        return "${multiplyExpression.expr1.toRust()}.wrapping_mul(${multiplyExpression.expr2.toRust()})"
    }
}

data class WrappingSubtract(val subtractExpression: SubtractExpression, override val symbolTable: SymbolTable) :
    ReconditionedExpression {
    override fun toRust(): String {
        return "${subtractExpression.expr1.toRust()}.wrapping_sub(${subtractExpression.expr2.toRust()})"
    }
}

data class ReconditionedDivision(val divideExpression: DivideExpression, override val symbolTable: SymbolTable) :
    ReconditionedExpression {
    override fun toRust(): String {
        val zeroExpression = (divideExpression.expr1.toType() as NumberType).zero(symbolTable)
        return "(if (${divideExpression.expr2.toRust()} != ${zeroExpression.toRust()}) {${divideExpression.expr1.toRust()} / ${divideExpression.expr2.toRust()}} else {${zeroExpression.toRust()}})"
    }
}

fun Expression.toType(): Type {
    return when (this) {
        is Int8Literal -> I8Type
        is Int16Literal -> I16Type
        is Int32Literal -> I32Type
        is Int64Literal -> I64Type
        is Int128Literal -> I128Type
        is Float32Literal -> F32Type
        is Float64Literal -> F64Type
        is StringLiteral -> StringType
        is BooleanLiteral -> BoolType
        is Variable -> symbolTable[this.value]!!.type
        is WrappingAdd -> this.addExpression.toType()
        is WrappingMul -> this.multiplyExpression.toType()
        is WrappingSubtract -> this.subtractExpression.toType()
        is ReconditionedDivision -> this.divideExpression.toType()
        is RecursiveExpression -> this.expr1.toType()
        is GroupedExpression -> this.expression.toType()
    }
}

fun KClass<out Expression>.genSubClasses(): List<KClass<out Expression>> {
    return this.subclasses().filter { it.hasAnnotation<ExpressionGenNode>() }
}
