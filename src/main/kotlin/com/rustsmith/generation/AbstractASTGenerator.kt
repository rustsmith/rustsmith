import com.rustsmith.ast.AddExpression
import com.rustsmith.ast.Assignment
import com.rustsmith.ast.BitwiseAndLogicalAnd
import com.rustsmith.ast.BitwiseAndLogicalOr
import com.rustsmith.ast.BitwiseAndLogicalXor
import com.rustsmith.ast.BlockExpression
import com.rustsmith.ast.BoolType
import com.rustsmith.ast.BooleanLiteral
import com.rustsmith.ast.ChainedStatement
import com.rustsmith.ast.Declaration
import com.rustsmith.ast.DivideExpression
import com.rustsmith.ast.Expression
import com.rustsmith.ast.F32Type
import com.rustsmith.ast.F64Type
import com.rustsmith.ast.Float32Literal
import com.rustsmith.ast.Float64Literal
import com.rustsmith.ast.FunctionCallExpression
import com.rustsmith.ast.GroupedExpression
import com.rustsmith.ast.I128Type
import com.rustsmith.ast.I16Type
import com.rustsmith.ast.I32Type
import com.rustsmith.ast.I64Type
import com.rustsmith.ast.I8Type
import com.rustsmith.ast.IfElseExpression
import com.rustsmith.ast.Int128Literal
import com.rustsmith.ast.Int16Literal
import com.rustsmith.ast.Int32Literal
import com.rustsmith.ast.Int64Literal
import com.rustsmith.ast.Int8Literal
import com.rustsmith.ast.ModExpression
import com.rustsmith.ast.MultiplyExpression
import com.rustsmith.ast.Statement
import com.rustsmith.ast.SubtractExpression
import com.rustsmith.ast.TupleLiteral
import com.rustsmith.ast.TupleType
import com.rustsmith.ast.Type
import com.rustsmith.ast.Variable
import com.rustsmith.generation.SelectionManager
import kotlin.reflect.KClass

public interface AbstractASTGenerator {
    public fun generateDeclaration(selectionManager: SelectionManager): Declaration

    public fun generateAssignment(selectionManager: SelectionManager): Assignment

    public fun generateChainedStatement(selectionManager: SelectionManager): ChainedStatement

    public fun generateFunctionCallExpression(selectionManager: SelectionManager):
        FunctionCallExpression

    public fun generateBlockExpression(selectionManager: SelectionManager): BlockExpression

    public fun generateIfElseExpression(selectionManager: SelectionManager): IfElseExpression

    public fun selectRandomStatement(selectionManager: SelectionManager): KClass<out Statement>

    public fun generateStatement(selectionManager: SelectionManager): Statement =
        when (selectRandomStatement(selectionManager)) {
            Declaration::class -> generateDeclaration(selectionManager)
            Assignment::class -> generateAssignment(selectionManager)
            ChainedStatement::class -> generateChainedStatement(selectionManager)
            FunctionCallExpression::class -> generateFunctionCallExpression(selectionManager)
            BlockExpression::class -> generateBlockExpression(selectionManager)
            IfElseExpression::class -> generateIfElseExpression(selectionManager)
            else -> throw Exception("Unrecognized type")
        }

    public fun generateInt8Literal(type: Type, selectionManager: SelectionManager): Int8Literal

    public fun generateInt16Literal(type: Type, selectionManager: SelectionManager): Int16Literal

    public fun generateInt32Literal(type: Type, selectionManager: SelectionManager): Int32Literal

    public fun generateInt64Literal(type: Type, selectionManager: SelectionManager): Int64Literal

    public fun generateInt128Literal(type: Type, selectionManager: SelectionManager): Int128Literal

    public fun generateFloat32Literal(type: Type, selectionManager: SelectionManager): Float32Literal

    public fun generateFloat64Literal(type: Type, selectionManager: SelectionManager): Float64Literal

    public fun generateBooleanLiteral(type: Type, selectionManager: SelectionManager): BooleanLiteral

    public fun generateTupleLiteral(type: Type, selectionManager: SelectionManager): TupleLiteral

    public fun generateVariable(type: Type, selectionManager: SelectionManager): Variable

    public fun generateGroupedExpression(type: Type, selectionManager: SelectionManager):
        GroupedExpression

    public fun generateBlockExpression(type: Type, selectionManager: SelectionManager):
        BlockExpression

    public fun generateIfElseExpression(type: Type, selectionManager: SelectionManager):
        IfElseExpression

    public fun generateAddExpression(type: Type, selectionManager: SelectionManager): AddExpression

    public fun generateSubtractExpression(type: Type, selectionManager: SelectionManager):
        SubtractExpression

    public fun generateDivideExpression(type: Type, selectionManager: SelectionManager):
        DivideExpression

    public fun generateMultiplyExpression(type: Type, selectionManager: SelectionManager):
        MultiplyExpression

    public fun generateModExpression(type: Type, selectionManager: SelectionManager): ModExpression

    public fun generateBitwiseAndLogicalAnd(type: Type, selectionManager: SelectionManager):
        BitwiseAndLogicalAnd

    public fun generateBitwiseAndLogicalOr(type: Type, selectionManager: SelectionManager):
        BitwiseAndLogicalOr

    public fun generateBitwiseAndLogicalXor(type: Type, selectionManager: SelectionManager):
        BitwiseAndLogicalXor

    public fun generateFunctionCallExpression(type: Type, selectionManager: SelectionManager):
        FunctionCallExpression

    public fun selectRandomExpression(type: Type, selectionManager: SelectionManager):
        KClass<out Expression>

    public fun generateExpression(type: Type, selectionManager: SelectionManager): Expression =
        when (selectRandomExpression(type, selectionManager)) {
            Int8Literal::class -> generateInt8Literal(type, selectionManager)
            Int16Literal::class -> generateInt16Literal(type, selectionManager)
            Int32Literal::class -> generateInt32Literal(type, selectionManager)
            Int64Literal::class -> generateInt64Literal(type, selectionManager)
            Int128Literal::class -> generateInt128Literal(type, selectionManager)
            Float32Literal::class -> generateFloat32Literal(type, selectionManager)
            Float64Literal::class -> generateFloat64Literal(type, selectionManager)
            BooleanLiteral::class -> generateBooleanLiteral(type, selectionManager)
            TupleLiteral::class -> generateTupleLiteral(type, selectionManager)
            Variable::class -> generateVariable(type, selectionManager)
            GroupedExpression::class -> generateGroupedExpression(type, selectionManager)
            BlockExpression::class -> generateBlockExpression(type, selectionManager)
            IfElseExpression::class -> generateIfElseExpression(type, selectionManager)
            AddExpression::class -> generateAddExpression(type, selectionManager)
            SubtractExpression::class -> generateSubtractExpression(type, selectionManager)
            DivideExpression::class -> generateDivideExpression(type, selectionManager)
            MultiplyExpression::class -> generateMultiplyExpression(type, selectionManager)
            ModExpression::class -> generateModExpression(type, selectionManager)
            BitwiseAndLogicalAnd::class -> generateBitwiseAndLogicalAnd(type, selectionManager)
            BitwiseAndLogicalOr::class -> generateBitwiseAndLogicalOr(type, selectionManager)
            BitwiseAndLogicalXor::class -> generateBitwiseAndLogicalXor(type, selectionManager)
            FunctionCallExpression::class -> generateFunctionCallExpression(type, selectionManager)
            else -> throw Exception("Unrecognized type")
        }

    public fun generateTupleType(selectionManager: SelectionManager): TupleType

    public fun generateBoolType(selectionManager: SelectionManager): BoolType

    public fun generateI8Type(selectionManager: SelectionManager): I8Type

    public fun generateI16Type(selectionManager: SelectionManager): I16Type

    public fun generateI32Type(selectionManager: SelectionManager): I32Type

    public fun generateI64Type(selectionManager: SelectionManager): I64Type

    public fun generateI128Type(selectionManager: SelectionManager): I128Type

    public fun generateF32Type(selectionManager: SelectionManager): F32Type

    public fun generateF64Type(selectionManager: SelectionManager): F64Type

    public fun selectRandomType(selectionManager: SelectionManager): KClass<out Type>

    public fun generateType(selectionManager: SelectionManager): Type =
        when (selectRandomType(selectionManager)) {
            TupleType::class -> generateTupleType(selectionManager)
            BoolType::class -> generateBoolType(selectionManager)
            I8Type::class -> generateI8Type(selectionManager)
            I16Type::class -> generateI16Type(selectionManager)
            I32Type::class -> generateI32Type(selectionManager)
            I64Type::class -> generateI64Type(selectionManager)
            I128Type::class -> generateI128Type(selectionManager)
            F32Type::class -> generateF32Type(selectionManager)
            F64Type::class -> generateF64Type(selectionManager)
            else -> throw Exception("Unrecognized type")
        }
}
