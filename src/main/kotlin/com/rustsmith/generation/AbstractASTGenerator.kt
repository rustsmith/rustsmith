import com.rustsmith.ast.*
import com.rustsmith.generation.Context
import kotlin.reflect.KClass

interface AbstractASTGenerator {
    fun generateExpressionStatement(ctx: Context): ExpressionStatement

    fun generateDeclaration(ctx: Context): Declaration

    fun generateAssignment(ctx: Context): Assignment

    fun selectRandomStatement(ctx: Context): KClass<out Statement>

    fun generateStatement(ctx: Context): Statement = when (selectRandomStatement(ctx)) {
        ExpressionStatement::class -> generateExpressionStatement(ctx)
        Declaration::class -> generateDeclaration(ctx)
        Assignment::class -> generateAssignment(ctx)
        else -> throw Exception("Unrecognized type")
    }

    fun generateInt8Literal(type: Type, ctx: Context): Int8Literal

    fun generateInt16Literal(type: Type, ctx: Context): Int16Literal

    fun generateInt32Literal(type: Type, ctx: Context): Int32Literal

    fun generateInt64Literal(type: Type, ctx: Context): Int64Literal

    fun generateInt128Literal(type: Type, ctx: Context): Int128Literal

    fun generateFloat32Literal(type: Type, ctx: Context): Float32Literal

    fun generateFloat64Literal(type: Type, ctx: Context): Float64Literal

    fun generateBooleanLiteral(type: Type, ctx: Context): BooleanLiteral

    fun generateTupleLiteral(type: Type, ctx: Context): TupleLiteral

    fun generateVariable(type: Type, ctx: Context): Variable

    fun generateFunctionCallExpression(type: Type, ctx: Context): FunctionCallExpression

    fun generateGroupedExpression(type: Type, ctx: Context): GroupedExpression

    fun generateBlockExpression(type: Type, ctx: Context): BlockExpression

    fun generateIfElseExpression(type: Type, ctx: Context): IfElseExpression

    fun generateAddExpression(type: Type, ctx: Context): AddExpression

    fun generateSubtractExpression(type: Type, ctx: Context): SubtractExpression

    fun generateDivideExpression(type: Type, ctx: Context): DivideExpression

    fun generateMultiplyExpression(type: Type, ctx: Context): MultiplyExpression

    fun generateModExpression(type: Type, ctx: Context): ModExpression

    fun generateBitwiseAndLogicalAnd(type: Type, ctx: Context): BitwiseAndLogicalAnd

    fun generateBitwiseAndLogicalOr(type: Type, ctx: Context): BitwiseAndLogicalOr

    fun generateBitwiseAndLogicalXor(type: Type, ctx: Context): BitwiseAndLogicalXor

    fun selectRandomExpression(type: Type, ctx: Context): KClass<out Expression>

    fun generateExpression(type: Type, ctx: Context): Expression =
        when (selectRandomExpression(type, ctx)) {
            Int8Literal::class -> generateInt8Literal(type, ctx)
            Int16Literal::class -> generateInt16Literal(type, ctx)
            Int32Literal::class -> generateInt32Literal(type, ctx)
            Int64Literal::class -> generateInt64Literal(type, ctx)
            Int128Literal::class -> generateInt128Literal(type, ctx)
            Float32Literal::class -> generateFloat32Literal(type, ctx)
            Float64Literal::class -> generateFloat64Literal(type, ctx)
            BooleanLiteral::class -> generateBooleanLiteral(type, ctx)
            TupleLiteral::class -> generateTupleLiteral(type, ctx)
            Variable::class -> generateVariable(type, ctx)
            FunctionCallExpression::class -> generateFunctionCallExpression(type, ctx)
            GroupedExpression::class -> generateGroupedExpression(type, ctx)
            BlockExpression::class -> generateBlockExpression(type, ctx)
            IfElseExpression::class -> generateIfElseExpression(type, ctx)
            AddExpression::class -> generateAddExpression(type, ctx)
            SubtractExpression::class -> generateSubtractExpression(type, ctx)
            DivideExpression::class -> generateDivideExpression(type, ctx)
            MultiplyExpression::class -> generateMultiplyExpression(type, ctx)
            ModExpression::class -> generateModExpression(type, ctx)
            BitwiseAndLogicalAnd::class -> generateBitwiseAndLogicalAnd(type, ctx)
            BitwiseAndLogicalOr::class -> generateBitwiseAndLogicalOr(type, ctx)
            BitwiseAndLogicalXor::class -> generateBitwiseAndLogicalXor(type, ctx)
            else -> throw Exception("Unrecognized type")
        }

    fun generateTupleType(ctx: Context): TupleType

    fun generateBoolType(ctx: Context): BoolType

    fun generateI8Type(ctx: Context): I8Type

    fun generateI16Type(ctx: Context): I16Type

    fun generateI32Type(ctx: Context): I32Type

    fun generateI64Type(ctx: Context): I64Type

    fun generateI128Type(ctx: Context): I128Type

    fun generateF32Type(ctx: Context): F32Type

    fun generateF64Type(ctx: Context): F64Type

    fun selectRandomType(ctx: Context): KClass<out Type>

    fun generateType(ctx: Context): Type = when (selectRandomType(ctx)) {
        TupleType::class -> generateTupleType(ctx)
        BoolType::class -> generateBoolType(ctx)
        I8Type::class -> generateI8Type(ctx)
        I16Type::class -> generateI16Type(ctx)
        I32Type::class -> generateI32Type(ctx)
        I64Type::class -> generateI64Type(ctx)
        I128Type::class -> generateI128Type(ctx)
        F32Type::class -> generateF32Type(ctx)
        F64Type::class -> generateF64Type(ctx)
        else -> throw Exception("Unrecognized type")
    }
}
