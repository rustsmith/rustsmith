package com.rustsmith.generation

import AbstractASTGenerator
import com.rustsmith.Random
import com.rustsmith.ast.*
import com.rustsmith.generation.selection.randomByWeights
import com.rustsmith.selectionManager
import java.math.BigInteger
import kotlin.random.asJavaRandom
import kotlin.reflect.KClass

class ASTGenerator(private val symbolTable: SymbolTable) : AbstractASTGenerator {
    private val dependantStatements = mutableListOf<Statement>()

    operator fun invoke(ctx: Context, type: Type = VoidType): StatementBlock {
        var currentSelectionManager = ctx.enterScope()
        val statements = mutableListOf<Statement>()
        while (selectionManager.createNewStatementWeightings(currentSelectionManager).randomByWeights()) {
            val statement = generateStatement(currentSelectionManager)
            statements.addAll(dependantStatements)
            statements.add(statement)
            dependantStatements.clear()
            currentSelectionManager = currentSelectionManager.incrementStatementCount()
        }
        val statementsWithDependants = dependantStatements + statements
        return if (type == VoidType) {
            StatementBlock(statementsWithDependants, symbolTable)
        } else {
            val finalExpression = generateExpression(type, currentSelectionManager).toStatement(false)
            StatementBlock(
                statementsWithDependants + (dependantStatements + finalExpression), symbolTable
            )
        }
    }

    /** Statement Generation **/

    override fun selectRandomStatement(ctx: Context): KClass<out Statement> {
        return selectionManager.availableStatementsWeightings(ctx).randomByWeights()
    }

    override fun generateExpressionStatement(ctx: Context): ExpressionStatement {
        return ExpressionStatement(
            generateExpression(
                generateType(ctx), ctx
            ), true, symbolTable
        )
    }

    override fun generateDeclaration(ctx: Context): Declaration {
        val declarationType = generateType(ctx)
        return generateDependantDeclarationOfType(
            declarationType, ctx = ctx
        )
    }

    private fun generateDependantDeclarationOfType(
        type: Type, mutable: Boolean = Random.nextBoolean(), ctx: Context
    ): Declaration {
        val variableName = IdentGenerator.generateVariable()
        val expression = generateExpression(type, ctx.incrementCount(Declaration::class))
        symbolTable[variableName] = IdentifierData(type, mutable)
        return Declaration(mutable, type, variableName, expression, symbolTable)
    }

    override fun generateAssignment(ctx: Context): Assignment {
        val value = symbolTable.getRandomMutableVariable()
        return if (value == null) {
            // No variables found, so a declaration is created and that statement is added to the list for chaining later
            val declaration =
                generateDependantDeclarationOfType(generateType(ctx), true, ctx.incrementCount(Assignment::class))
            dependantStatements.add(declaration)
            val expression = generateExpression(
                declaration.type, ctx.incrementCount(Assignment::class)
            )
            Assignment(declaration.variableName, expression, symbolTable)
        } else {
            val expression = generateExpression(
                value.second.type, ctx.incrementCount(Assignment::class)
            )
            Assignment(value.first, expression, symbolTable)
        }
    }

    /** Expression generation **/

    override fun selectRandomExpression(type: Type, ctx: Context): KClass<out Expression> {
        return selectionManager.availableExpressionsWeightings(ctx, type).randomByWeights()
    }

    override fun generateInt8Literal(type: Type, ctx: Context): Int8Literal =
        Int8Literal(Random.nextBits(7), symbolTable)

    override fun generateInt16Literal(type: Type, ctx: Context): Int16Literal =
        Int16Literal(Random.nextBits(15), symbolTable)

    override fun generateInt32Literal(type: Type, ctx: Context): Int32Literal =
        Int32Literal(Random.nextInt(), symbolTable)

    override fun generateInt64Literal(type: Type, ctx: Context): Int64Literal =
        Int64Literal(Random.nextLong(), symbolTable)

    override fun generateInt128Literal(type: Type, ctx: Context): Int128Literal =
        Int128Literal(BigInteger(127, Random.asJavaRandom()), symbolTable)

    override fun generateFloat32Literal(type: Type, ctx: Context): Float32Literal =
        Float32Literal(Random.nextFloat(), symbolTable)

    override fun generateFloat64Literal(type: Type, ctx: Context): Float64Literal =
        Float64Literal(Random.nextDouble(), symbolTable)

    override fun generateBooleanLiteral(type: Type, ctx: Context): BooleanLiteral =
        BooleanLiteral(Random.nextBoolean(), symbolTable)

    override fun generateTupleLiteral(type: Type, ctx: Context): TupleLiteral {
        if (type is TupleType) {
            return TupleLiteral(
                type.types.map {
                    generateExpression(
                        it, ctx.incrementCount(TupleLiteral::class)
                    )
                }, symbolTable
            )
        }
        throw Exception("Incompatible Type")
    }

    override fun generateVariable(type: Type, ctx: Context): Variable {
        val value = symbolTable.getRandomVariableOfType(type)
        return if (value == null) {
            // No variables found for given type, so a declaration is created and that statement is added to the list for chaining later
            val declaration = generateDependantDeclarationOfType(
                type, ctx = ctx.incrementCount(Variable::class)
            )

            dependantStatements.add(declaration)
            Variable(declaration.variableName, symbolTable)
        } else {
            Variable(value.first, symbolTable)
        }
    }

    override fun generateGroupedExpression(type: Type, ctx: Context): GroupedExpression = GroupedExpression(
        generateExpression(type, ctx.incrementCount(GroupedExpression::class)), symbolTable
    )

    override fun generateBlockExpression(type: Type, ctx: Context): BlockExpression {
        val newScope = symbolTable.enterScope()
        val body = ASTGenerator(newScope)(ctx.incrementCount(BlockExpression::class).withSymbolTable(newScope), type)
        return BlockExpression(body, type, symbolTable)
    }

    override fun generateIfElseExpression(type: Type, ctx: Context): IfElseExpression {
        return IfElseExpression(
            generateExpression(BoolType, ctx.incrementCount(IfElseExpression::class)),
            generateBlockExpression(type, ctx.incrementCount(IfElseExpression::class)),
            generateBlockExpression(type, ctx.incrementCount(IfElseExpression::class)),
            symbolTable
        )
    }

    override fun generateAddExpression(type: Type, ctx: Context): AddExpression {
        return AddExpression(
            generateExpression(type, ctx.incrementCount(AddExpression::class)),
            generateExpression(type, ctx.incrementCount(AddExpression::class)),
            symbolTable
        )
    }

    override fun generateSubtractExpression(type: Type, ctx: Context): SubtractExpression {
        return SubtractExpression(
            generateExpression(type, ctx.incrementCount(SubtractExpression::class)),
            generateExpression(type, ctx.incrementCount(SubtractExpression::class)),
            symbolTable
        )
    }

    override fun generateDivideExpression(type: Type, ctx: Context): DivideExpression {
        return DivideExpression(
            generateExpression(type, ctx.incrementCount(DivideExpression::class)),
            generateExpression(type, ctx.incrementCount(DivideExpression::class)),
            symbolTable
        )
    }

    override fun generateMultiplyExpression(type: Type, ctx: Context): MultiplyExpression {
        return MultiplyExpression(
            generateExpression(type, ctx.incrementCount(MultiplyExpression::class)),
            generateExpression(type, ctx.incrementCount(MultiplyExpression::class)),
            symbolTable
        )
    }

    override fun generateModExpression(type: Type, ctx: Context): ModExpression {
        return ModExpression(
            generateExpression(type, ctx.incrementCount(ModExpression::class)),
            generateExpression(type, ctx.incrementCount(ModExpression::class)),
            symbolTable
        )
    }

    override fun generateBitwiseAndLogicalAnd(type: Type, ctx: Context): BitwiseAndLogicalAnd {
        return BitwiseAndLogicalAnd(
            generateExpression(type, ctx.incrementCount(BitwiseAndLogicalAnd::class)),
            generateExpression(type, ctx.incrementCount(BitwiseAndLogicalAnd::class)),
            symbolTable
        )
    }

    override fun generateBitwiseAndLogicalOr(type: Type, ctx: Context): BitwiseAndLogicalOr {
        return BitwiseAndLogicalOr(
            generateExpression(type, ctx.incrementCount(BitwiseAndLogicalOr::class)),
            generateExpression(type, ctx.incrementCount(BitwiseAndLogicalOr::class)),
            symbolTable
        )
    }

    override fun generateBitwiseAndLogicalXor(type: Type, ctx: Context): BitwiseAndLogicalXor {
        return BitwiseAndLogicalXor(
            generateExpression(type, ctx.incrementCount(BitwiseAndLogicalXor::class)),
            generateExpression(type, ctx.incrementCount(BitwiseAndLogicalXor::class)),
            symbolTable
        )
    }

    override fun generateFunctionCallExpression(type: Type, ctx: Context): FunctionCallExpression {
        val functionData = symbolTable.functionSymbolTable.getRandomFunctionOfType(type)
        if (functionData == null) {
            val newFunctionType = generateFunction(type, ctx)
            return FunctionCallExpression(
                newFunctionType.first, newFunctionType.second.args.map {
                    generateExpression(
                        it, ctx.incrementCount(FunctionCallExpression::class)
                    )
                }, symbolTable
            )
        } else {
            return FunctionCallExpression(
                functionData.first, (functionData.second.type as FunctionType).args.map {
                    generateExpression(
                        it, ctx.incrementCount(FunctionCallExpression::class)
                    )
                }, symbolTable
            )
        }
    }

    private fun generateFunction(returnType: Type, ctx: Context): Pair<String, FunctionType> {
        val numArgs = Random.nextInt(5)
        val argTypes = (0 until numArgs).map { generateType(ctx.incrementCount(FunctionType::class)) }
        val symbolTableForFunction = SymbolTable(
            null, symbolTable.functionSymbolTable
        )
        val functionDefinition = FunctionDefinition(
            returnType,
            IdentGenerator.generateFunctionName(),
            argTypes.associateBy { IdentGenerator.generateVariable() },
            ASTGenerator(
                symbolTableForFunction
            )(ctx.incrementCount(FunctionCallExpression::class).withSymbolTable(symbolTableForFunction), returnType)
        )
        val functionType = FunctionType(returnType, argTypes)
        symbolTable.functionSymbolTable[functionDefinition.functionName] = IdentifierData(functionType, false)
        symbolTable.functionSymbolTable.addFunction(functionDefinition)
        return functionDefinition.functionName to functionType
    }

    /** Type generators **/

    override fun selectRandomType(ctx: Context): KClass<out Type> {
        return selectionManager.availableTypesWeightings(ctx).randomByWeights()
    }

    override fun generateBoolType(ctx: Context) = BoolType

    override fun generateI8Type(ctx: Context) = I8Type

    override fun generateI16Type(ctx: Context) = I16Type

    override fun generateI32Type(ctx: Context) = I32Type

    override fun generateI64Type(ctx: Context) = I64Type

    override fun generateI128Type(ctx: Context) = I128Type

    override fun generateF32Type(ctx: Context) = F32Type

    override fun generateF64Type(ctx: Context) = F64Type

    override fun generateTupleType(ctx: Context): TupleType {
        val numArgs = Random.nextInt(1, 5)
        val argTypes = (0 until numArgs).map { generateType(ctx.incrementCount(TupleType::class)) }
        return TupleType(argTypes)
    }
}
