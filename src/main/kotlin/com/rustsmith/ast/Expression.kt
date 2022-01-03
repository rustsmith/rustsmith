package com.rustsmith.ast

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import com.rustsmith.Random
import java.math.BigInteger
import kotlin.random.asJavaRandom
import kotlin.reflect.KClass
import kotlin.reflect.full.hasAnnotation
import kotlin.reflect.full.isSubclassOf

annotation class ExpressionGenNode(val compatibleType: KClass<out Type>)

@JsonIgnoreProperties(value = ["symbolTable"])
sealed interface Expression : ASTNode {
    val symbolTable: SymbolTable
}

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

sealed interface RecursiveExpression : Expression

sealed interface BinOpExpression : RecursiveExpression {
    val expr1: Expression
    val expr2: Expression
}

interface RandomizeableBinOp<T : BinOpExpression> : Randomizeable<T> {
    fun createExpressions(symbolTable: SymbolTable, expressionType: Type): Pair<Expression, Expression> {
        val exp1: Expression = generateExpression(symbolTable, expressionType)
        val exp2: Expression = generateExpression(symbolTable, expressionType)
        return exp1 to exp2
    }
}

@ExpressionGenNode(NumberType::class)
data class AddExpression(
    override val expr1: Expression,
    override val expr2: Expression,
    override val symbolTable: SymbolTable
) : BinOpExpression {

    companion object : RandomizeableBinOp<AddExpression> {
        override fun createRandom(symbolTable: SymbolTable, type: Type): AddExpression {
            val (exp1, exp2) = this.createExpressions(symbolTable, type)
            return AddExpression(exp1, exp2, symbolTable)
        }
    }

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

    companion object : RandomizeableBinOp<SubtractExpression> {
        override fun createRandom(symbolTable: SymbolTable, type: Type): SubtractExpression {
            val (exp1, exp2) = this.createExpressions(symbolTable, type)
            return SubtractExpression(exp1, exp2, symbolTable)
        }
    }

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

    companion object : RandomizeableBinOp<DivideExpression> {
        override fun createRandom(symbolTable: SymbolTable, type: Type): DivideExpression {
            val (exp1, exp2) = this.createExpressions(symbolTable, type)
            return DivideExpression(exp1, exp2, symbolTable)
        }
    }

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

    companion object : RandomizeableBinOp<MultiplyExpression> {
        override fun createRandom(symbolTable: SymbolTable, type: Type): MultiplyExpression {
            val (exp1, exp2) = this.createExpressions(symbolTable, type)
            return MultiplyExpression(exp1, exp2, symbolTable)
        }
    }

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

    companion object : RandomizeableBinOp<ModExpression> {
        override fun createRandom(symbolTable: SymbolTable, type: Type): ModExpression {
            val (exp1, exp2) = this.createExpressions(symbolTable, type)
            return ModExpression(exp1, exp2, symbolTable)
        }
    }

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

    companion object : RandomizeableBinOp<BitwiseAndLogicalAnd> {
        override fun createRandom(symbolTable: SymbolTable, type: Type): BitwiseAndLogicalAnd {
            val (exp1, exp2) = this.createExpressions(symbolTable, type)
            return BitwiseAndLogicalAnd(exp1, exp2, symbolTable)
        }
    }

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

    companion object : RandomizeableBinOp<BitwiseAndLogicalOr> {
        override fun createRandom(symbolTable: SymbolTable, type: Type): BitwiseAndLogicalOr {
            val (exp1, exp2) = this.createExpressions(symbolTable, type)
            return BitwiseAndLogicalOr(exp1, exp2, symbolTable)
        }
    }

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

    companion object : RandomizeableBinOp<BitwiseAndLogicalXor> {
        override fun createRandom(symbolTable: SymbolTable, type: Type): BitwiseAndLogicalXor {
            val (exp1, exp2) = this.createExpressions(symbolTable, type)
            return BitwiseAndLogicalXor(exp1, exp2, symbolTable)
        }
    }

    override fun toRust(): String {
        return "(${expr1.toRust()} ^ ${expr2.toRust()})"
    }
}

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

sealed interface ExpressionAndStatement : Expression

interface RandomizeableExpressionAndStatement<T> : Randomizeable<T> {
    fun createRandomStatement(symbolTable: SymbolTable): T
}

@ExpressionGenNode(Type::class)
data class BlockExpression(
    val statement: Statement,
    val type: Type?,
    override val symbolTable: SymbolTable
) : RecursiveExpression, ExpressionAndStatement {
    companion object : RandomizeableExpressionAndStatement<BlockExpression> {
        override fun createRandom(symbolTable: SymbolTable, type: Type): BlockExpression {
            val newSymbolTable = symbolTable.enterScope()
            val statement = generateStatement(newSymbolTable)
            val finalExpression = generateExpression(newSymbolTable, type)
            return BlockExpression(newSymbolTable.exitScope(ChainedStatement(statement, finalExpression.toStatement(false), newSymbolTable)), type, symbolTable)
        }

        override fun createRandomStatement(symbolTable: SymbolTable): BlockExpression {
            val newSymbolTable = symbolTable.enterScope()
            val statement = generateStatement(newSymbolTable)
            return BlockExpression(newSymbolTable.exitScope(statement), null, symbolTable)
        }
    }

    override fun toRust(): String {
        return "{\n${statement.toRust()}\n}"
    }
}

@ExpressionGenNode(Type::class)
data class IfElseExpression(
    val predicate: Expression,
    val ifBlock: BlockExpression,
    val elseBlock: BlockExpression,
    override val symbolTable: SymbolTable
) : RecursiveExpression, ExpressionAndStatement {
    companion object : RandomizeableExpressionAndStatement<IfElseExpression> {
        override fun createRandom(symbolTable: SymbolTable, type: Type): IfElseExpression {
            val predicate = generateExpression(symbolTable, BoolType)
            val ifSymbolTable = symbolTable.enterScope()
            val ifStatement = generateStatement(ifSymbolTable)
            val finalIfExpression = generateExpression(ifSymbolTable, type)
            val ifBody = BlockExpression(
                ifSymbolTable.exitScope(ChainedStatement(ifStatement, finalIfExpression.toStatement(false), ifSymbolTable)),
                type,
                symbolTable
            )
            val elseSymbolTable = symbolTable.enterScope()
            val elseStatement = generateStatement(elseSymbolTable)
            val finalElseExpression = generateExpression(elseSymbolTable, type)
            val elseBody = BlockExpression(
                elseSymbolTable.exitScope(ChainedStatement(elseStatement, finalElseExpression.toStatement(false), elseSymbolTable)),
                type,
                symbolTable
            )
            return IfElseExpression(predicate, ifBody, elseBody, symbolTable)
        }

        override fun createRandomStatement(symbolTable: SymbolTable): IfElseExpression {
            val predicate = generateExpression(symbolTable, BoolType)
            val ifSymbolTable = symbolTable.enterScope()
            val ifStatement = generateStatement(ifSymbolTable)
            val ifBody = BlockExpression(
                ifSymbolTable.exitScope(ifStatement),
                null,
                symbolTable
            )
            val elseSymbolTable = symbolTable.enterScope()
            val elseStatement = generateStatement(elseSymbolTable)
            val elseBody = BlockExpression(
                elseSymbolTable.exitScope(elseStatement),
                null,
                symbolTable
            )
            return IfElseExpression(predicate, ifBody, elseBody, symbolTable)
        }
    }

    override fun toRust(): String {
        return "if (${predicate.toRust()}) \n {\n ${ifBlock.toRust()} \n} else {\n ${elseBlock.toRust()} \n}"
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
        is StringLiteral -> StringType
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
    }
}

fun KClass<out Expression>.genSubClasses(): List<KClass<out Expression>> {
    val depth = Thread.currentThread().stackTrace.size
    return if (depth > 20) {
        this.subclasses().filter { it.hasAnnotation<ExpressionGenNode>() }.filter { !it.isSubclassOf(RecursiveExpression::class) }
    } else {
        this.subclasses().filter { it.hasAnnotation<ExpressionGenNode>() }
    }
}

fun Expression.toStatement(addSemicolon: Boolean = true): ExpressionStatement {
    return ExpressionStatement(this, addSemicolon, symbolTable)
}
