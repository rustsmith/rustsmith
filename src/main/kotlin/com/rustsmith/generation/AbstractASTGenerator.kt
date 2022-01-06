import com.rustsmith.ast.*
import com.rustsmith.generation.SelectionManager
import kotlin.reflect.KClass

public interface AbstractASTGenerator {
    public fun generateDeclaration(selectionManager: SelectionManager): Declaration

    public fun generateAssignment(selectionManager: SelectionManager): Assignment

    public fun generateChainedStatement(selectionManager: SelectionManager): ChainedStatement

    public fun generateBlockExpression(selectionManager: SelectionManager): BlockExpression

    public fun generateIfElseExpression(selectionManager: SelectionManager): IfElseExpression

    public fun selectRandomStatement(selectionManager: SelectionManager): KClass<out Statement>

    public fun generateStatement(selectionManager: SelectionManager): Statement =
        when (selectRandomStatement(selectionManager)) {
            Declaration::class -> generateDeclaration(selectionManager)
            Assignment::class -> generateAssignment(selectionManager)
            ChainedStatement::class -> generateChainedStatement(selectionManager)
            BlockExpression::class -> generateBlockExpression(selectionManager)
            IfElseExpression::class -> generateIfElseExpression(selectionManager)
            else -> TODO()
        }

    public fun generateInt8Literal(type: Type, selectionManager: SelectionManager): Int8Literal

    public fun generateInt16Literal(type: Type, selectionManager: SelectionManager): Int16Literal

    public fun generateInt32Literal(type: Type, selectionManager: SelectionManager): Int32Literal

    public fun generateInt64Literal(type: Type, selectionManager: SelectionManager): Int64Literal

    public fun generateInt128Literal(type: Type, selectionManager: SelectionManager): Int128Literal

    public fun generateFloat32Literal(type: Type, selectionManager: SelectionManager): Float32Literal

    public fun generateFloat64Literal(type: Type, selectionManager: SelectionManager): Float64Literal

    public fun generateBooleanLiteral(type: Type, selectionManager: SelectionManager): BooleanLiteral

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
            else -> TODO()
        }
}
