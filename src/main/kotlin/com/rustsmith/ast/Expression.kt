package com.rustsmith.ast

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import java.math.BigInteger
import kotlin.reflect.KClass

annotation class ExpressionGenNode(val compatibleType: KClass<out Type>, val weight: Int = 1)

@JsonIgnoreProperties(value = ["symbolTable"])
sealed interface Expression : ASTNode {
    val symbolTable: SymbolTable
}

@ExpressionGenNode(I8Type::class)
data class Int8Literal(val value: Int, override val symbolTable: SymbolTable) : Expression {

    override fun toRust(): String {
        return "${value}i8"
    }
}

@ExpressionGenNode(I16Type::class)
data class Int16Literal(val value: Int, override val symbolTable: SymbolTable) : Expression {

    override fun toRust(): String {
        return "${value}i16"
    }
}

@ExpressionGenNode(I32Type::class)
data class Int32Literal(val value: Int, override val symbolTable: SymbolTable) :
    Expression {

    override fun toRust(): String {
        return "${value}i32"
    }
}

@ExpressionGenNode(I64Type::class)
data class Int64Literal(val value: Long, override val symbolTable: SymbolTable) :
    Expression {

    override fun toRust(): String {
        return "${value}i64"
    }
}

@ExpressionGenNode(I128Type::class)
data class Int128Literal(val value: BigInteger, override val symbolTable: SymbolTable) :
    Expression {

    override fun toRust(): String {
        return "${value}i128"
    }
}

@ExpressionGenNode(F32Type::class)
data class Float32Literal(val value: Float, override val symbolTable: SymbolTable) : Expression {
    override fun toRust(): String {
        return "${value}f32"
    }
}

@ExpressionGenNode(F64Type::class)
data class Float64Literal(val value: Double, override val symbolTable: SymbolTable) : Expression {
    override fun toRust(): String {
        return "${value}f64"
    }
}

// @ExpressionGenNode(StringType::class)
// data class StringLiteral(val value: String, override val symbolTable: SymbolTable) : Expression {
//
//    override fun toRust(): String {
//        return "\"$value\""
//    }
// }

@ExpressionGenNode(BoolType::class)
data class BooleanLiteral(val value: Boolean, override val symbolTable: SymbolTable) : Expression {

    override fun toRust(): String {
        return value.toString()
    }
}

@ExpressionGenNode(Type::class)
data class Variable(val value: String, override val symbolTable: SymbolTable) : Expression {

    override fun toRust(): String {
        return value
    }
}

sealed interface RecursiveExpression : Expression

sealed interface BinOpExpression : RecursiveExpression {
    val expr1: Expression
    val expr2: Expression
}

@ExpressionGenNode(NumberType::class)
data class AddExpression(
    override val expr1: Expression,
    override val expr2: Expression,
    override val symbolTable: SymbolTable
) : BinOpExpression {

    override fun toRust(): String {
        return "(${expr1.toRust()} + ${expr2.toRust()})"
    }
}

@ExpressionGenNode(NumberType::class)
data class SubtractExpression(
    override val expr1: Expression,
    override val expr2: Expression,
    override val symbolTable: SymbolTable
) : BinOpExpression {

    override fun toRust(): String {
        return "(${expr1.toRust()} - ${expr2.toRust()})"
    }
}

@ExpressionGenNode(NumberType::class)
data class DivideExpression(
    override val expr1: Expression,
    override val expr2: Expression,
    override val symbolTable: SymbolTable
) : BinOpExpression {

    override fun toRust(): String {
        return "(${expr1.toRust()} / ${expr2.toRust()})"
    }
}

@ExpressionGenNode(NumberType::class)
data class MultiplyExpression(
    override val expr1: Expression,
    override val expr2: Expression,
    override val symbolTable: SymbolTable
) : BinOpExpression {

    override fun toRust(): String {
        return "(${expr1.toRust()} * ${expr2.toRust()})"
    }
}

@ExpressionGenNode(IntType::class)
data class ModExpression(
    override val expr1: Expression,
    override val expr2: Expression,
    override val symbolTable: SymbolTable
) : BinOpExpression {

    override fun toRust(): String {
        return "(${expr1.toRust()} % ${expr2.toRust()})"
    }
}

@ExpressionGenNode(BitWiseCompatibleType::class)
data class BitwiseAndLogicalAnd(
    override val expr1: Expression,
    override val expr2: Expression,
    override val symbolTable: SymbolTable
) : BinOpExpression {

    override fun toRust(): String {
        return "(${expr1.toRust()} & ${expr2.toRust()})"
    }
}

@ExpressionGenNode(BitWiseCompatibleType::class)
data class BitwiseAndLogicalOr(
    override val expr1: Expression,
    override val expr2: Expression,
    override val symbolTable: SymbolTable
) : BinOpExpression {

    override fun toRust(): String {
        return "(${expr1.toRust()} | ${expr2.toRust()})"
    }
}

@ExpressionGenNode(BitWiseCompatibleType::class)
data class BitwiseAndLogicalXor(
    override val expr1: Expression,
    override val expr2: Expression,
    override val symbolTable: SymbolTable
) : BinOpExpression {

    override fun toRust(): String {
        return "(${expr1.toRust()} ^ ${expr2.toRust()})"
    }
}

@ExpressionGenNode(Type::class)
data class GroupedExpression(
    val expression: Expression,
    override val symbolTable: SymbolTable
) : RecursiveExpression {
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

sealed interface ExpressionAndStatement : Expression, Statement {
    val isStatement: Boolean

    fun addSemiColon(): String {
        return if (isStatement) ";" else ""
    }
}

@ExpressionGenNode(Type::class)
data class FunctionCallExpression(val functionName: String, val args: List<Expression>,
                                  override val isStatement: Boolean, override val symbolTable: SymbolTable) : ExpressionAndStatement {

    override fun toRust(): String {
        return "$functionName(${args.joinToString(",") { it.toRust() }})${addSemiColon()}"
    }
}

@ExpressionGenNode(Type::class)
data class BlockExpression(
    val statement: Statement,
    val type: Type?,
    override val isStatement: Boolean,
    override val symbolTable: SymbolTable
) : RecursiveExpression, ExpressionAndStatement {
    override fun toRust(): String {
        return "{\n${statement.toRust()}\n}${addSemiColon()}"
    }
}

@ExpressionGenNode(Type::class)
data class IfElseExpression(
    val predicate: Expression,
    val ifBlock: BlockExpression,
    val elseBlock: BlockExpression,
    override val isStatement: Boolean,
    override val symbolTable: SymbolTable
) : RecursiveExpression, ExpressionAndStatement {
    override fun toRust(): String {
        return "if (${predicate.toRust()}) \n {\n ${ifBlock.toRust()} \n} else {\n ${elseBlock.toRust()} \n}${addSemiColon()}"
    }
}

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
        return "(if (${divideExpression.expr2.toRust()} != ${zeroExpression.toRust()}) {${divideExpression.toRust()}} else {${zeroExpression.toRust()}})"
    }
}

data class ReconditionedMod(val modExpression: ModExpression, override val symbolTable: SymbolTable) :
    ReconditionedExpression {
    override fun toRust(): String {
        val zeroExpression = (modExpression.expr1.toType() as NumberType).zero(symbolTable)
        return "(if (${modExpression.expr2.toRust()} != ${zeroExpression.toRust()}) {${modExpression.toRust()}} else {${zeroExpression.toRust()}})"
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
//        is StringLiteral -> StringType
        is BooleanLiteral -> BoolType
        is Variable -> symbolTable[this.value]!!.type
        is WrappingAdd -> this.addExpression.toType()
        is WrappingMul -> this.multiplyExpression.toType()
        is WrappingSubtract -> this.subtractExpression.toType()
        is ReconditionedDivision -> this.divideExpression.toType()
        is BinOpExpression -> this.expr1.toType()
        is GroupedExpression -> this.expression.toType()
        is BlockExpression -> this.type!!
        is ReconditionedMod -> this.modExpression.toType()
        is IfElseExpression -> this.ifBlock.type!!
        is FunctionCallExpression -> (symbolTable.functionSymbolTable[this.functionName]!!.type as FunctionType).returnType
    }
}

fun Expression.toStatement(addSemicolon: Boolean = true): ExpressionStatement {
    return ExpressionStatement(this, addSemicolon, symbolTable)
}
