import com.rustsmith.ast.AddExpression
import com.rustsmith.ast.ArrayAccess
import com.rustsmith.ast.ArrayLengthExpression
import com.rustsmith.ast.ArrayLiteral
import com.rustsmith.ast.ArrayPushExpression
import com.rustsmith.ast.ArrayType
import com.rustsmith.ast.Assignment
import com.rustsmith.ast.BitwiseAndLogicalAnd
import com.rustsmith.ast.BitwiseAndLogicalOr
import com.rustsmith.ast.BitwiseAndLogicalXor
import com.rustsmith.ast.BlockExpression
import com.rustsmith.ast.BoolType
import com.rustsmith.ast.BooleanLiteral
import com.rustsmith.ast.BoxDereferenceExpression
import com.rustsmith.ast.BoxType
import com.rustsmith.ast.BreakStatement
import com.rustsmith.ast.CLIArgumentAccessExpression
import com.rustsmith.ast.Declaration
import com.rustsmith.ast.DereferenceExpression
import com.rustsmith.ast.DivideExpression
import com.rustsmith.ast.EqExpression
import com.rustsmith.ast.Expression
import com.rustsmith.ast.ExpressionStatement
import com.rustsmith.ast.F32Type
import com.rustsmith.ast.F64Type
import com.rustsmith.ast.Float32Literal
import com.rustsmith.ast.Float64Literal
import com.rustsmith.ast.FunctionCallExpression
import com.rustsmith.ast.GTEExpression
import com.rustsmith.ast.GTExpression
import com.rustsmith.ast.GroupedExpression
import com.rustsmith.ast.I128Type
import com.rustsmith.ast.I16Type
import com.rustsmith.ast.I32Type
import com.rustsmith.ast.I64Type
import com.rustsmith.ast.I8Type
import com.rustsmith.ast.IfElseExpression
import com.rustsmith.ast.IfExpression
import com.rustsmith.ast.Int128Literal
import com.rustsmith.ast.Int16Literal
import com.rustsmith.ast.Int32Literal
import com.rustsmith.ast.Int64Literal
import com.rustsmith.ast.Int8Literal
import com.rustsmith.ast.LTEExpression
import com.rustsmith.ast.LTExpression
import com.rustsmith.ast.LoopExpression
import com.rustsmith.ast.MethodCallExpression
import com.rustsmith.ast.ModExpression
import com.rustsmith.ast.MultiplyExpression
import com.rustsmith.ast.MutableReferenceExpression
import com.rustsmith.ast.MutableReferenceType
import com.rustsmith.ast.NEqExpression
import com.rustsmith.ast.NewBoxExpression
import com.rustsmith.ast.PrintElementStatement
import com.rustsmith.ast.ReferenceExpression
import com.rustsmith.ast.ReferenceType
import com.rustsmith.ast.ReturnStatement
import com.rustsmith.ast.Statement
import com.rustsmith.ast.StringLiteral
import com.rustsmith.ast.StringType
import com.rustsmith.ast.StructElementAccessExpression
import com.rustsmith.ast.StructInstantiationExpression
import com.rustsmith.ast.StructType
import com.rustsmith.ast.SubtractExpression
import com.rustsmith.ast.TupleElementAccessExpression
import com.rustsmith.ast.TupleLiteral
import com.rustsmith.ast.TupleType
import com.rustsmith.ast.Type
import com.rustsmith.ast.TypeAliasExpression
import com.rustsmith.ast.TypeAliasType
import com.rustsmith.ast.U128Type
import com.rustsmith.ast.U16Type
import com.rustsmith.ast.U32Type
import com.rustsmith.ast.U64Type
import com.rustsmith.ast.U8Type
import com.rustsmith.ast.UInt128Literal
import com.rustsmith.ast.UInt16Literal
import com.rustsmith.ast.UInt32Literal
import com.rustsmith.ast.UInt64Literal
import com.rustsmith.ast.UInt8Literal
import com.rustsmith.ast.USizeLiteral
import com.rustsmith.ast.USizeType
import com.rustsmith.ast.Variable
import com.rustsmith.ast.VoidLiteral
import com.rustsmith.ast.VoidType
import com.rustsmith.generation.Context
import kotlin.reflect.KClass

public interface AbstractASTGenerator {
    public fun generateExpressionStatement(ctx: Context): ExpressionStatement

    public fun generateDeclaration(ctx: Context): Declaration

    public fun generateAssignment(ctx: Context): Assignment

    public fun generatePrintElementStatement(ctx: Context): PrintElementStatement

    public fun generateReturnStatement(ctx: Context): ReturnStatement

    public fun generateBreakStatement(ctx: Context): BreakStatement

    public fun selectRandomStatement(ctx: Context): KClass<out Statement>

    public fun generateStatement(ctx: Context): Statement

    public fun generateSpecificStatement(statement: KClass<out Statement>, ctx: Context): Statement =
        when (statement) {
            ExpressionStatement::class -> generateExpressionStatement(ctx)
            Declaration::class -> generateDeclaration(ctx)
            Assignment::class -> generateAssignment(ctx)
            PrintElementStatement::class -> generatePrintElementStatement(ctx)
            ReturnStatement::class -> generateReturnStatement(ctx)
            BreakStatement::class -> generateBreakStatement(ctx)
            else -> throw Exception("Unrecognized type")
        }

    public fun generateVoidLiteral(type: Type, ctx: Context): VoidLiteral

    public fun generateCLIArgumentAccessExpression(type: Type, ctx: Context):
        CLIArgumentAccessExpression

    public fun generateInt8Literal(type: Type, ctx: Context): Int8Literal

    public fun generateInt16Literal(type: Type, ctx: Context): Int16Literal

    public fun generateInt32Literal(type: Type, ctx: Context): Int32Literal

    public fun generateInt64Literal(type: Type, ctx: Context): Int64Literal

    public fun generateInt128Literal(type: Type, ctx: Context): Int128Literal

    public fun generateUInt8Literal(type: Type, ctx: Context): UInt8Literal

    public fun generateUInt16Literal(type: Type, ctx: Context): UInt16Literal

    public fun generateUInt32Literal(type: Type, ctx: Context): UInt32Literal

    public fun generateUInt64Literal(type: Type, ctx: Context): UInt64Literal

    public fun generateUInt128Literal(type: Type, ctx: Context): UInt128Literal

    public fun generateUSizeLiteral(type: Type, ctx: Context): USizeLiteral

    public fun generateFloat32Literal(type: Type, ctx: Context): Float32Literal

    public fun generateFloat64Literal(type: Type, ctx: Context): Float64Literal

    public fun generateStringLiteral(type: Type, ctx: Context): StringLiteral

    public fun generateBooleanLiteral(type: Type, ctx: Context): BooleanLiteral

    public fun generateTupleLiteral(type: Type, ctx: Context): TupleLiteral

    public fun generateStructInstantiationExpression(type: Type, ctx: Context):
        StructInstantiationExpression

    public fun generateArrayLiteral(type: Type, ctx: Context): ArrayLiteral

    public fun generateTupleElementAccessExpression(type: Type, ctx: Context):
        TupleElementAccessExpression

    public fun generateStructElementAccessExpression(type: Type, ctx: Context):
        StructElementAccessExpression

    public fun generateVariable(type: Type, ctx: Context): Variable

    public fun generateDereferenceExpression(type: Type, ctx: Context): DereferenceExpression

    public fun generateBoxDereferenceExpression(type: Type, ctx: Context): BoxDereferenceExpression

    public fun generateGroupedExpression(type: Type, ctx: Context): GroupedExpression

    public fun generateFunctionCallExpression(type: Type, ctx: Context): FunctionCallExpression

    public fun generateMethodCallExpression(type: Type, ctx: Context): MethodCallExpression

    public fun generateBlockExpression(type: Type, ctx: Context): BlockExpression

    public fun generateIfElseExpression(type: Type, ctx: Context): IfElseExpression

    public fun generateIfExpression(type: Type, ctx: Context): IfExpression

    public fun generateLoopExpression(type: Type, ctx: Context): LoopExpression

    public fun generateArrayAccess(type: Type, ctx: Context): ArrayAccess

    public fun generateNewBoxExpression(type: Type, ctx: Context): NewBoxExpression

    public fun generateTypeAliasExpression(type: Type, ctx: Context): TypeAliasExpression

    public fun generateAddExpression(type: Type, ctx: Context): AddExpression

    public fun generateSubtractExpression(type: Type, ctx: Context): SubtractExpression

    public fun generateDivideExpression(type: Type, ctx: Context): DivideExpression

    public fun generateMultiplyExpression(type: Type, ctx: Context): MultiplyExpression

    public fun generateModExpression(type: Type, ctx: Context): ModExpression

    public fun generateBitwiseAndLogicalAnd(type: Type, ctx: Context): BitwiseAndLogicalAnd

    public fun generateBitwiseAndLogicalOr(type: Type, ctx: Context): BitwiseAndLogicalOr

    public fun generateBitwiseAndLogicalXor(type: Type, ctx: Context): BitwiseAndLogicalXor

    public fun generateEqExpression(type: Type, ctx: Context): EqExpression

    public fun generateNEqExpression(type: Type, ctx: Context): NEqExpression

    public fun generateGTExpression(type: Type, ctx: Context): GTExpression

    public fun generateGTEExpression(type: Type, ctx: Context): GTEExpression

    public fun generateLTExpression(type: Type, ctx: Context): LTExpression

    public fun generateLTEExpression(type: Type, ctx: Context): LTEExpression

    public fun generateReferenceExpression(type: Type, ctx: Context): ReferenceExpression

    public fun generateMutableReferenceExpression(type: Type, ctx: Context):
        MutableReferenceExpression

    public fun generateArrayLengthExpression(type: Type, ctx: Context): ArrayLengthExpression

    public fun generateArrayPushExpression(type: Type, ctx: Context): ArrayPushExpression

    public fun selectRandomExpression(type: Type, ctx: Context): KClass<out Expression>

    public fun generateExpression(type: Type, ctx: Context): Expression

    public fun generateSpecificExpression(
        expression: KClass<out Expression>,
        type: Type,
        ctx: Context,
    ): Expression = when (expression) {
        VoidLiteral::class -> generateVoidLiteral(type, ctx)
        CLIArgumentAccessExpression::class -> generateCLIArgumentAccessExpression(type, ctx)
        Int8Literal::class -> generateInt8Literal(type, ctx)
        Int16Literal::class -> generateInt16Literal(type, ctx)
        Int32Literal::class -> generateInt32Literal(type, ctx)
        Int64Literal::class -> generateInt64Literal(type, ctx)
        Int128Literal::class -> generateInt128Literal(type, ctx)
        UInt8Literal::class -> generateUInt8Literal(type, ctx)
        UInt16Literal::class -> generateUInt16Literal(type, ctx)
        UInt32Literal::class -> generateUInt32Literal(type, ctx)
        UInt64Literal::class -> generateUInt64Literal(type, ctx)
        UInt128Literal::class -> generateUInt128Literal(type, ctx)
        USizeLiteral::class -> generateUSizeLiteral(type, ctx)
        Float32Literal::class -> generateFloat32Literal(type, ctx)
        Float64Literal::class -> generateFloat64Literal(type, ctx)
        StringLiteral::class -> generateStringLiteral(type, ctx)
        BooleanLiteral::class -> generateBooleanLiteral(type, ctx)
        TupleLiteral::class -> generateTupleLiteral(type, ctx)
        StructInstantiationExpression::class -> generateStructInstantiationExpression(type, ctx)
        ArrayLiteral::class -> generateArrayLiteral(type, ctx)
        TupleElementAccessExpression::class -> generateTupleElementAccessExpression(type, ctx)
        StructElementAccessExpression::class -> generateStructElementAccessExpression(type, ctx)
        Variable::class -> generateVariable(type, ctx)
        DereferenceExpression::class -> generateDereferenceExpression(type, ctx)
        BoxDereferenceExpression::class -> generateBoxDereferenceExpression(type, ctx)
        GroupedExpression::class -> generateGroupedExpression(type, ctx)
        FunctionCallExpression::class -> generateFunctionCallExpression(type, ctx)
        MethodCallExpression::class -> generateMethodCallExpression(type, ctx)
        BlockExpression::class -> generateBlockExpression(type, ctx)
        IfElseExpression::class -> generateIfElseExpression(type, ctx)
        IfExpression::class -> generateIfExpression(type, ctx)
        LoopExpression::class -> generateLoopExpression(type, ctx)
        ArrayAccess::class -> generateArrayAccess(type, ctx)
        NewBoxExpression::class -> generateNewBoxExpression(type, ctx)
        TypeAliasExpression::class -> generateTypeAliasExpression(type, ctx)
        AddExpression::class -> generateAddExpression(type, ctx)
        SubtractExpression::class -> generateSubtractExpression(type, ctx)
        DivideExpression::class -> generateDivideExpression(type, ctx)
        MultiplyExpression::class -> generateMultiplyExpression(type, ctx)
        ModExpression::class -> generateModExpression(type, ctx)
        BitwiseAndLogicalAnd::class -> generateBitwiseAndLogicalAnd(type, ctx)
        BitwiseAndLogicalOr::class -> generateBitwiseAndLogicalOr(type, ctx)
        BitwiseAndLogicalXor::class -> generateBitwiseAndLogicalXor(type, ctx)
        EqExpression::class -> generateEqExpression(type, ctx)
        NEqExpression::class -> generateNEqExpression(type, ctx)
        GTExpression::class -> generateGTExpression(type, ctx)
        GTEExpression::class -> generateGTEExpression(type, ctx)
        LTExpression::class -> generateLTExpression(type, ctx)
        LTEExpression::class -> generateLTEExpression(type, ctx)
        ReferenceExpression::class -> generateReferenceExpression(type, ctx)
        MutableReferenceExpression::class -> generateMutableReferenceExpression(type, ctx)
        ArrayLengthExpression::class -> generateArrayLengthExpression(type, ctx)
        ArrayPushExpression::class -> generateArrayPushExpression(type, ctx)
        else -> throw Exception("Unrecognized type")
    }

    public fun generateVoidType(ctx: Context): VoidType

    public fun generateStringType(ctx: Context): StringType

    public fun generateBoolType(ctx: Context): BoolType

    public fun generateI8Type(ctx: Context): I8Type

    public fun generateI16Type(ctx: Context): I16Type

    public fun generateI32Type(ctx: Context): I32Type

    public fun generateI64Type(ctx: Context): I64Type

    public fun generateI128Type(ctx: Context): I128Type

    public fun generateU8Type(ctx: Context): U8Type

    public fun generateU16Type(ctx: Context): U16Type

    public fun generateU32Type(ctx: Context): U32Type

    public fun generateU64Type(ctx: Context): U64Type

    public fun generateU128Type(ctx: Context): U128Type

    public fun generateUSizeType(ctx: Context): USizeType

    public fun generateF32Type(ctx: Context): F32Type

    public fun generateF64Type(ctx: Context): F64Type

    public fun generateTupleType(ctx: Context): TupleType

    public fun generateStructType(ctx: Context): StructType

    public fun generateArrayType(ctx: Context): ArrayType

    public fun generateTypeAliasType(ctx: Context): TypeAliasType

    public fun generateBoxType(ctx: Context): BoxType

    public fun generateReferenceType(ctx: Context): ReferenceType

    public fun generateMutableReferenceType(ctx: Context): MutableReferenceType

    public fun selectRandomType(ctx: Context): KClass<out Type>

    public fun generateType(ctx: Context): Type

    public fun generateSpecificType(type: KClass<out Type>, ctx: Context): Type = when (type) {
        VoidType::class -> generateVoidType(ctx)
        StringType::class -> generateStringType(ctx)
        BoolType::class -> generateBoolType(ctx)
        I8Type::class -> generateI8Type(ctx)
        I16Type::class -> generateI16Type(ctx)
        I32Type::class -> generateI32Type(ctx)
        I64Type::class -> generateI64Type(ctx)
        I128Type::class -> generateI128Type(ctx)
        U8Type::class -> generateU8Type(ctx)
        U16Type::class -> generateU16Type(ctx)
        U32Type::class -> generateU32Type(ctx)
        U64Type::class -> generateU64Type(ctx)
        U128Type::class -> generateU128Type(ctx)
        USizeType::class -> generateUSizeType(ctx)
        F32Type::class -> generateF32Type(ctx)
        F64Type::class -> generateF64Type(ctx)
        TupleType::class -> generateTupleType(ctx)
        StructType::class -> generateStructType(ctx)
        ArrayType::class -> generateArrayType(ctx)
        TypeAliasType::class -> generateTypeAliasType(ctx)
        BoxType::class -> generateBoxType(ctx)
        ReferenceType::class -> generateReferenceType(ctx)
        MutableReferenceType::class -> generateMutableReferenceType(ctx)
        else -> throw Exception("Unrecognized type")
    }
}
