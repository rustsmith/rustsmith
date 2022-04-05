import com.rustsmith.ast.*
import com.rustsmith.generation.Context
import kotlin.reflect.KClass

public interface AbstractASTGenerator {
    public fun generateExpressionStatement(ctx: Context): ExpressionStatement

    public fun generateDeclaration(ctx: Context): Declaration

    public fun generateAssignment(ctx: Context): Assignment

    public fun selectRandomStatement(ctx: Context): KClass<out Statement>

    public fun generateStatement(ctx: Context): Statement = when (selectRandomStatement(ctx)) {
        ExpressionStatement::class -> generateExpressionStatement(ctx)
        Declaration::class -> generateDeclaration(ctx)
        Assignment::class -> generateAssignment(ctx)
        else -> throw Exception("Unrecognized type")
    }

    public fun generateInt8Literal(type: Type, ctx: Context): Int8Literal

    public fun generateInt16Literal(type: Type, ctx: Context): Int16Literal

    public fun generateInt32Literal(type: Type, ctx: Context): Int32Literal

    public fun generateInt64Literal(type: Type, ctx: Context): Int64Literal

    public fun generateInt128Literal(type: Type, ctx: Context): Int128Literal

    public fun generateFloat32Literal(type: Type, ctx: Context): Float32Literal

    public fun generateFloat64Literal(type: Type, ctx: Context): Float64Literal

    public fun generateBooleanLiteral(type: Type, ctx: Context): BooleanLiteral

    public fun generateTupleLiteral(type: Type, ctx: Context): TupleLiteral

    public fun generateVariable(type: Type, ctx: Context): Variable

    public fun generateFunctionCallExpression(type: Type, ctx: Context): FunctionCallExpression

    public fun generateStructInstantiationExpression(type: Type, ctx: Context):
        StructInstantiationExpression

    public fun generateGroupedExpression(type: Type, ctx: Context): GroupedExpression

    public fun generateBlockExpression(type: Type, ctx: Context): BlockExpression

    public fun generateIfElseExpression(type: Type, ctx: Context): IfElseExpression

    public fun generateAddExpression(type: Type, ctx: Context): AddExpression

    public fun generateSubtractExpression(type: Type, ctx: Context): SubtractExpression

    public fun generateDivideExpression(type: Type, ctx: Context): DivideExpression

    public fun generateMultiplyExpression(type: Type, ctx: Context): MultiplyExpression

    public fun generateModExpression(type: Type, ctx: Context): ModExpression

    public fun generateBitwiseAndLogicalAnd(type: Type, ctx: Context): BitwiseAndLogicalAnd

    public fun generateBitwiseAndLogicalOr(type: Type, ctx: Context): BitwiseAndLogicalOr

    public fun generateBitwiseAndLogicalXor(type: Type, ctx: Context): BitwiseAndLogicalXor

    public fun selectRandomExpression(type: Type, ctx: Context): KClass<out Expression>

    public fun generateExpression(type: Type, ctx: Context): Expression =
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
            StructInstantiationExpression::class -> generateStructInstantiationExpression(type, ctx)
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

    public fun generateTupleType(ctx: Context): TupleType

    public fun generateStructType(ctx: Context): StructType

    public fun generateBoolType(ctx: Context): BoolType

    public fun generateI8Type(ctx: Context): I8Type

    public fun generateI16Type(ctx: Context): I16Type

    public fun generateI32Type(ctx: Context): I32Type

    public fun generateI64Type(ctx: Context): I64Type

    public fun generateI128Type(ctx: Context): I128Type

    public fun generateF32Type(ctx: Context): F32Type

    public fun generateF64Type(ctx: Context): F64Type

    public fun selectRandomType(ctx: Context): KClass<out Type>

    public fun generateType(ctx: Context): Type = when (selectRandomType(ctx)) {
        TupleType::class -> generateTupleType(ctx)
        StructType::class -> generateStructType(ctx)
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
