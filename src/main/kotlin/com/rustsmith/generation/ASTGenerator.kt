package com.rustsmith.generation

import AbstractASTGenerator
import com.rustsmith.Random
import com.rustsmith.ast.*
import java.math.BigInteger
import kotlin.random.asJavaRandom
import kotlin.reflect.KClass

class ASTGenerator(private val symbolTable: SymbolTable) : AbstractASTGenerator {
    private val statements = mutableListOf<Statement>()

    private fun combineStatementWithDependencies(statement: Statement): Statement {
        return if (statements.isEmpty()) {
            statement
        } else {
            ChainedStatement(ChainedStatement.createFromList(statements, symbolTable), statement, symbolTable)
        }
    }

    operator fun invoke(selectionManager: SelectionManager, type: Type = VoidType): Statement {
        val statement = generateStatement(selectionManager)
        val finalStatement = combineStatementWithDependencies(statement)
        statements.clear()
        return if (type == VoidType) finalStatement else ChainedStatement(finalStatement, combineStatementWithDependencies(generateExpression(type, selectionManager).toStatement(false)), symbolTable)
    }

    /** Statement Generation **/

    override fun selectRandomStatement(selectionManager: SelectionManager): KClass<out Statement> {
        return selectionManager.availableStatements().random(Random)
    }

    override fun generateExpressionStatement(selectionManager: SelectionManager): ExpressionStatement {
        return ExpressionStatement(generateExpression(generateType(selectionManager), selectionManager), true, symbolTable)
    }

    override fun generateDeclaration(selectionManager: SelectionManager): Declaration {
        val declarationType = generateType(selectionManager)
        return generateDependantDeclarationOfType(declarationType, selectionManager)
    }

    private fun generateDependantDeclarationOfType(type: Type, selectionManager: SelectionManager): Declaration {
        val variableName = IdentGenerator.generateVariable()
        val expression = generateExpression(type, selectionManager.incrementCount(Declaration::class))
        symbolTable[variableName] = IdentifierData(type)
        return Declaration(type, variableName, expression, symbolTable)
    }

    override fun generateAssignment(selectionManager: SelectionManager): Assignment {
        val value = symbolTable.getRandomVariable()
        return if (value == null) {
            // No variables found, so a declaration is created and that statement is added to the list for chaining later
            val declaration = generateDeclaration(selectionManager.incrementCount(Assignment::class))
            statements.add(declaration)
            val expression = generateExpression(declaration.type, selectionManager.incrementCount(Assignment::class))
            Assignment(declaration.variableName, expression, symbolTable)
        } else {
            val expression = generateExpression(value.second.type, selectionManager.incrementCount(Assignment::class))
            Assignment(value.first, expression, symbolTable)
        }
    }

    override fun generateChainedStatement(selectionManager: SelectionManager): ChainedStatement {
        val s1 = ASTGenerator(symbolTable)(selectionManager.incrementCount(ChainedStatement::class))
        val s2 = ASTGenerator(symbolTable)(selectionManager.incrementCount(ChainedStatement::class))
        return ChainedStatement(s1, s2, symbolTable)
    }

//    override fun generateFunctionCallExpression(selectionManager: SelectionManager): FunctionCallExpression {
//        return generateFunctionCallExpression(VoidType, selectionManager)
//    }
//
//    override fun generateBlockExpression(selectionManager: SelectionManager): BlockExpression {
//        return BlockExpression(
//            ASTGenerator(symbolTable.enterScope())(selectionManager.incrementCount(BlockExpression::class)),
//            null,
//            symbolTable
//        )
//    }
//
//    override fun generateIfElseExpression(selectionManager: SelectionManager): IfElseExpression {
//        return IfElseExpression(
//            generateExpression(BoolType, selectionManager.incrementCount(IfElseExpression::class)),
//            generateBlockExpression(selectionManager.incrementCount(IfElseExpression::class)),
//            generateBlockExpression(selectionManager.incrementCount(IfElseExpression::class)),
//            symbolTable
//        )
//    }

    /** Expression generation **/

    override fun selectRandomExpression(type: Type, selectionManager: SelectionManager): KClass<out Expression> {
        return selectionManager.availableExpressions(type).random(Random)
    }

    override fun generateInt8Literal(type: Type, selectionManager: SelectionManager): Int8Literal =
        Int8Literal(Random.nextBits(7), symbolTable)

    override fun generateInt16Literal(type: Type, selectionManager: SelectionManager): Int16Literal =
        Int16Literal(Random.nextBits(15), symbolTable)

    override fun generateInt32Literal(type: Type, selectionManager: SelectionManager): Int32Literal =
        Int32Literal(Random.nextInt(), symbolTable)

    override fun generateInt64Literal(type: Type, selectionManager: SelectionManager): Int64Literal =
        Int64Literal(Random.nextLong(), symbolTable)

    override fun generateInt128Literal(type: Type, selectionManager: SelectionManager): Int128Literal =
        Int128Literal(BigInteger(127, Random.asJavaRandom()), symbolTable)

    override fun generateFloat32Literal(type: Type, selectionManager: SelectionManager): Float32Literal =
        Float32Literal(Random.nextFloat(), symbolTable)

    override fun generateFloat64Literal(type: Type, selectionManager: SelectionManager): Float64Literal =
        Float64Literal(Random.nextDouble(), symbolTable)

    override fun generateBooleanLiteral(type: Type, selectionManager: SelectionManager): BooleanLiteral =
        BooleanLiteral(Random.nextBoolean(), symbolTable)

    override fun generateTupleLiteral(type: Type, selectionManager: SelectionManager): TupleLiteral {
        if (type is TupleType) {
            return TupleLiteral(type.types.map { generateExpression(it, selectionManager.incrementCount(TupleLiteral::class)) }, symbolTable)
        }
        throw Exception("Incompatible Type")
    }

    override fun generateVariable(type: Type, selectionManager: SelectionManager): Variable {
        val value = symbolTable.getRandomVariableOfType(type)
        return if (value == null) {
            // No variables found for given type, so a declaration is created and that statement is added to the list for chaining later
            val declaration = generateDependantDeclarationOfType(type, selectionManager.incrementCount(Variable::class))
            statements.add(declaration)
            Variable(declaration.variableName, symbolTable)
        } else {
            Variable(value.first, symbolTable)
        }
    }

    override fun generateGroupedExpression(type: Type, selectionManager: SelectionManager): GroupedExpression =
        GroupedExpression(
            generateExpression(type, selectionManager.incrementCount(GroupedExpression::class)),
            symbolTable
        )

    override fun generateBlockExpression(type: Type, selectionManager: SelectionManager): BlockExpression {
        val newScope = symbolTable.enterScope()
        val body = ASTGenerator(newScope)(selectionManager.incrementCount(BlockExpression::class), type)
        return BlockExpression(body, type, symbolTable)
    }

    override fun generateIfElseExpression(type: Type, selectionManager: SelectionManager): IfElseExpression {
        return IfElseExpression(
            generateExpression(BoolType, selectionManager.incrementCount(IfElseExpression::class)),
            generateBlockExpression(type, selectionManager.incrementCount(IfElseExpression::class)),
            generateBlockExpression(type, selectionManager.incrementCount(IfElseExpression::class)),
            symbolTable
        )
    }

    override fun generateAddExpression(type: Type, selectionManager: SelectionManager): AddExpression {
        return AddExpression(
            generateExpression(type, selectionManager.incrementCount(AddExpression::class)),
            generateExpression(type, selectionManager.incrementCount(AddExpression::class)),
            symbolTable
        )
    }

    override fun generateSubtractExpression(type: Type, selectionManager: SelectionManager): SubtractExpression {
        return SubtractExpression(
            generateExpression(type, selectionManager.incrementCount(SubtractExpression::class)),
            generateExpression(type, selectionManager.incrementCount(SubtractExpression::class)),
            symbolTable
        )
    }

    override fun generateDivideExpression(type: Type, selectionManager: SelectionManager): DivideExpression {
        return DivideExpression(
            generateExpression(type, selectionManager.incrementCount(DivideExpression::class)),
            generateExpression(type, selectionManager.incrementCount(DivideExpression::class)),
            symbolTable
        )
    }

    override fun generateMultiplyExpression(type: Type, selectionManager: SelectionManager): MultiplyExpression {
        return MultiplyExpression(
            generateExpression(type, selectionManager.incrementCount(MultiplyExpression::class)),
            generateExpression(type, selectionManager.incrementCount(MultiplyExpression::class)),
            symbolTable
        )
    }

    override fun generateModExpression(type: Type, selectionManager: SelectionManager): ModExpression {
        return ModExpression(
            generateExpression(type, selectionManager.incrementCount(ModExpression::class)),
            generateExpression(type, selectionManager.incrementCount(ModExpression::class)),
            symbolTable
        )
    }

    override fun generateBitwiseAndLogicalAnd(type: Type, selectionManager: SelectionManager): BitwiseAndLogicalAnd {
        return BitwiseAndLogicalAnd(
            generateExpression(type, selectionManager.incrementCount(BitwiseAndLogicalAnd::class)),
            generateExpression(type, selectionManager.incrementCount(BitwiseAndLogicalAnd::class)),
            symbolTable
        )
    }

    override fun generateBitwiseAndLogicalOr(type: Type, selectionManager: SelectionManager): BitwiseAndLogicalOr {
        return BitwiseAndLogicalOr(
            generateExpression(type, selectionManager.incrementCount(BitwiseAndLogicalOr::class)),
            generateExpression(type, selectionManager.incrementCount(BitwiseAndLogicalOr::class)),
            symbolTable
        )
    }

    override fun generateBitwiseAndLogicalXor(type: Type, selectionManager: SelectionManager): BitwiseAndLogicalXor {
        return BitwiseAndLogicalXor(
            generateExpression(type, selectionManager.incrementCount(BitwiseAndLogicalXor::class)),
            generateExpression(type, selectionManager.incrementCount(BitwiseAndLogicalXor::class)),
            symbolTable
        )
    }

    override fun generateFunctionCallExpression(
        type: Type,
        selectionManager: SelectionManager
    ): FunctionCallExpression {
        val functionData = symbolTable.functionSymbolTable.getRandomFunctionOfType(type)
        if (functionData == null) {
            val newFunctionType = generateFunction(type, selectionManager)
            return FunctionCallExpression(
                newFunctionType.first,
                newFunctionType.second.args.map {
                    generateExpression(
                        it,
                        selectionManager.incrementCount(FunctionCallExpression::class)
                    )
                },
                symbolTable
            )
        } else {
            return FunctionCallExpression(
                functionData.first,
                (functionData.second.type as FunctionType).args.map {
                    generateExpression(
                        it,
                        selectionManager.incrementCount(FunctionCallExpression::class)
                    )
                },
                symbolTable
            )
        }
    }

    private fun generateFunction(returnType: Type, selectionManager: SelectionManager): Pair<String, FunctionType> {
        val numArgs = Random.nextInt(5)
        val argTypes = (0 until numArgs).map { generateType(selectionManager.incrementCount(FunctionType::class)) }
        val functionDefinition =
            FunctionDefinition(
                returnType,
                IdentGenerator.generateFunctionName(),
                argTypes.associateBy { IdentGenerator.generateVariable() },
                ASTGenerator(SymbolTable(null, symbolTable.functionSymbolTable))(selectionManager.incrementCount(FunctionCallExpression::class), returnType)
            )
        val functionType = FunctionType(returnType, argTypes)
        symbolTable.functionSymbolTable[functionDefinition.functionName] = IdentifierData(functionType)
        symbolTable.functionSymbolTable.addFunction(functionDefinition)
        return functionDefinition.functionName to functionType
    }

    /** Type generators **/

    override fun selectRandomType(selectionManager: SelectionManager): KClass<out Type> {
        return selectionManager.availableTypes().random(Random)
    }

    override fun generateBoolType(selectionManager: SelectionManager) = BoolType

    override fun generateI8Type(selectionManager: SelectionManager) = I8Type

    override fun generateI16Type(selectionManager: SelectionManager) = I16Type

    override fun generateI32Type(selectionManager: SelectionManager) = I32Type

    override fun generateI64Type(selectionManager: SelectionManager) = I64Type

    override fun generateI128Type(selectionManager: SelectionManager) = I128Type

    override fun generateF32Type(selectionManager: SelectionManager) = F32Type

    override fun generateF64Type(selectionManager: SelectionManager) = F64Type

    override fun generateTupleType(selectionManager: SelectionManager): TupleType {
        val numArgs = Random.nextInt(1, 5)
        val argTypes = (0 until numArgs).map { generateType(selectionManager.incrementCount(TupleType::class)) }
        return TupleType(argTypes)
    }
}
