package com.rustsmith.generation

import AbstractASTGenerator
import com.rustsmith.CustomRandom
import com.rustsmith.ast.*
import com.rustsmith.logging.Logger
import com.rustsmith.randomByWeights
import com.rustsmith.selectionManager
import com.rustsmith.subclasses
import java.math.BigInteger
import kotlin.random.asJavaRandom
import kotlin.reflect.KClass

class ASTGenerator(private val symbolTable: SymbolTable) : AbstractASTGenerator {
    private val dependantStatements = mutableListOf<Statement>()

    operator fun invoke(ctx: Context, type: Type = VoidType, expression: Expression? = null): StatementBlock {
        var currentCtx = ctx.enterScope()
        val statements = mutableListOf<Statement>()
        while (selectionManager.choiceGenerateNewStatementWeightings(currentCtx).randomByWeights()) {
            val statement = generateStatement(currentCtx.forNewStatement())
            statements.addAll(dependantStatements)
            statements.add(statement)
            dependantStatements.clear()
            currentCtx = currentCtx.incrementStatementCount(statement)
            if (statement::class in BlockEndingStatement::class.subclasses()) {
                break
            }
        }
        val statementsWithDependants = dependantStatements + statements
        return if (type == VoidType) {
            StatementBlock(statementsWithDependants, symbolTable)
        } else {
            val finalExpression =
                expression?.toStatement(false) ?: generateExpression(type, currentCtx).toStatement(false)
            StatementBlock(
                statementsWithDependants + (dependantStatements + finalExpression), symbolTable
            )
        }
    }

    /** Statement Generation **/

    override fun selectRandomStatement(ctx: Context): KClass<out Statement> {
        val pickRandomByWeight = selectionManager.availableStatementsWeightings(ctx).pickRandomByWeight()
        Logger.logText("Picking statement: $pickRandomByWeight", ctx)
        return pickRandomByWeight
    }

    override fun generateVoidLiteral(type: Type, ctx: Context): VoidLiteral = VoidLiteral(symbolTable)
    override fun generateCLIArgumentAccessExpression(type: Type, ctx: Context): CLIArgumentAccessExpression {
        if (
            selectionManager.choiceGenerateNewCLIArgumentWeightings(ctx).randomByWeights() ||
            !symbolTable.globalSymbolTable.commandLineTypes.contains(type)
        ) {
            symbolTable.globalSymbolTable.commandLineTypes.add(type as CLIInputType)
            return CLIArgumentAccessExpression(
                symbolTable.globalSymbolTable.commandLineTypes.mapIndexed { i, t -> i to t }
                    .filter { it.second == type }.random().first + 1,
                type,
                symbolTable
            )
        }
        return CLIArgumentAccessExpression(
            symbolTable.globalSymbolTable.commandLineTypes.mapIndexed { i, t -> i to t }.filter { it.second == type }
                .random().first + 1,
            type,
            symbolTable
        )
    }

    override fun generateExpressionStatement(ctx: Context): ExpressionStatement {
        return ExpressionStatement(
            generateExpression(
                generateType(ctx.incrementCount(ExpressionStatement::class)),
                ctx.incrementCount(ExpressionStatement::class)
            ),
            true, symbolTable
        )
    }

    override fun generateDeclaration(ctx: Context): Declaration {
        val declarationType = generateType(ctx.incrementCount(Declaration::class))
        return generateDependantDeclarationOfType(declarationType, ctx = ctx)
    }

    private fun generateDependantDeclarationOfType(
        type: Type,
        mutable: Boolean = CustomRandom.nextBoolean(),
        ctx: Context
    ): Declaration {
        val variableName = IdentGenerator.generateVariable()
        val expression = generateExpression(type, ctx.incrementCount(Declaration::class))
        symbolTable[variableName] = IdentifierData(expression.toType().clone(), mutable, OwnershipState.VALID)
        return Declaration(mutable, type.clone(), variableName, expression, symbolTable)
    }

    override fun generateAssignment(ctx: Context): Assignment {
        val value = symbolTable.getRandomMutableVariable(ctx)
        return if (value == null) {
            Logger.logText("No LHSAssignment found, so create declaration", ctx)
            // No variables found, so a declaration is created and that statement is added to the list for chaining later
            val declaration =
                generateDependantDeclarationOfType(
                    generateType(ctx.incrementCount(Assignment::class)),
                    true,
                    ctx.forDependantDeclaration().incrementCount(Assignment::class)
                )
            dependantStatements.add(declaration)
            val expression = generateExpression(declaration.type, ctx.incrementCount(Assignment::class))
            Assignment(Variable(declaration.variableName, symbolTable), expression, symbolTable)
        } else {
            val lhsAssignmentType = value.toType()
            val lhsNodeAndRhsExpression: Pair<LHSAssignmentNode, Expression> =
                if (lhsAssignmentType is MutableReferenceType) {
                    val useDereferenceLhs = CustomRandom.nextBoolean()
                    (if (useDereferenceLhs) DereferenceExpression(value, value.symbolTable) else value) to
                        generateExpression(
                            if (useDereferenceLhs) lhsAssignmentType.internalType else lhsAssignmentType,
                            ctx.incrementCount(Assignment::class).withAssignmentNode(value.rootNode())
                        )
                } else {
                    value to
                        generateExpression(
                            lhsAssignmentType,
                            ctx.incrementCount(Assignment::class).withAssignmentNode(value.rootNode())
                        )
                }
            Assignment(lhsNodeAndRhsExpression.first, lhsNodeAndRhsExpression.second, symbolTable)
        }
    }

    override fun generateReturnStatement(ctx: Context): ReturnStatement {
        val expression = generateExpression(ctx.returnExpressionType!!, ctx.incrementCount(ReturnStatement::class))
        return ReturnStatement(expression, symbolTable)
    }

    override fun generateBreakStatement(ctx: Context): BreakStatement {
//        return if (ctx.returnLoopType == null) {
//            BreakStatement(null, symbolTable)
//        } else {
//            val expression = generateExpression(ctx.returnLoopType, ctx)
//            BreakStatement(expression, symbolTable)
//        }
        return BreakStatement(symbolTable)
    }

    /** Expression generation **/

    override fun selectRandomExpression(type: Type, ctx: Context): KClass<out Expression> {
        val pickRandomByWeight = selectionManager.availableExpressionsWeightings(ctx, type).pickRandomByWeight()
        Logger.logText("Picking expression $pickRandomByWeight for type:${type.toRust()}", ctx)
        return pickRandomByWeight
    }

    override fun generateInt8Literal(type: Type, ctx: Context): Int8Literal =
        Int8Literal(CustomRandom.nextBits(7), symbolTable)

    override fun generateInt16Literal(type: Type, ctx: Context): Int16Literal =
        Int16Literal(CustomRandom.nextBits(15), symbolTable)

    override fun generateInt32Literal(type: Type, ctx: Context): Int32Literal =
        Int32Literal(CustomRandom.nextInt(), symbolTable)

    override fun generateInt64Literal(type: Type, ctx: Context): Int64Literal =
        Int64Literal(CustomRandom.nextLong(), symbolTable)

    override fun generateInt128Literal(type: Type, ctx: Context): Int128Literal =
        Int128Literal(BigInteger(127, CustomRandom.asJavaRandom()), symbolTable)

    override fun generateFloat32Literal(type: Type, ctx: Context): Float32Literal =
        Float32Literal(CustomRandom.nextFloat(), symbolTable)

    override fun generateFloat64Literal(type: Type, ctx: Context): Float64Literal =
        Float64Literal(CustomRandom.nextDouble(), symbolTable)

    override fun generateStringLiteral(type: Type, ctx: Context): StringLiteral {
        val charPool: List<Char> = ('a'..'z') + ('A'..'Z') + ('0'..'9')
        return StringLiteral(
            (1..CustomRandom.nextInt(100)).map { charPool[CustomRandom.nextInt(0, charPool.size)] }
                .joinToString(""),
            symbolTable
        )
    }

    override fun generateBooleanLiteral(type: Type, ctx: Context): BooleanLiteral =
        BooleanLiteral(CustomRandom.nextBoolean(), symbolTable)

    override fun generateTupleLiteral(type: Type, ctx: Context): TupleLiteral {
        if (type is TupleType) {
            return TupleLiteral(
                type.types.map { generateExpression(it, ctx.incrementCount(TupleLiteral::class)) },
                symbolTable
            )
        }
        throw Exception("Incompatible Type")
    }

    override fun generateTupleElementAccessExpression(type: Type, ctx: Context): TupleElementAccessExpression {
        var tupleWithType = symbolTable.globalSymbolTable.findTupleWithType(type)
        if (tupleWithType == null) {
            tupleWithType = generateTupleTypeWithType(type, ctx)
        }
        val tupleExpression = generateExpression(
            tupleWithType,
            ctx.incrementCount(TupleElementAccessExpression::class).setRequiredType(type)
        )
        // Collect all indices that have required type
        val tupleType = tupleExpression.toType() as TupleType
        val typeIndices = tupleType.types
            .mapIndexed { index, pair -> pair to index }.filter { it.first == type }.map { it.second }
            .filter { tupleType.argumentsToOwnershipMap[it].second != OwnershipState.INVALID }
        val chosenIndex = typeIndices.random(CustomRandom)
        if (type.getOwnership() == OwnershipModel.MOVE) {
            val newOwnershipState = if (ctx.previousIncrement in PartialMoveExpression::class.subclasses()) {
                /* A partial move, so set the ownership state to partially valid */
                OwnershipState.PARTIALLY_VALID
            } else {
                /* Not a partial move, so set the ownership state to completely invalid */
                OwnershipState.INVALID
            }
            tupleType.argumentsToOwnershipMap[chosenIndex] =
                tupleType.argumentsToOwnershipMap[chosenIndex].copy(second = newOwnershipState)
        }

        if (ctx.previousIncrement == ReferenceExpression::class) {
            tupleType.argumentsToOwnershipMap[chosenIndex] =
                tupleType.argumentsToOwnershipMap[chosenIndex].copy(second = OwnershipState.BORROWED)
        }

        if (ctx.previousIncrement == MutableReferenceExpression::class) {
            tupleType.argumentsToOwnershipMap[chosenIndex] =
                tupleType.argumentsToOwnershipMap[chosenIndex].copy(second = OwnershipState.MUTABLY_BORROWED)
        }
        return TupleElementAccessExpression(tupleExpression, chosenIndex, symbolTable)
    }

    private fun generateTupleTypeWithType(type: Type, ctx: Context): TupleType {
        val numArgs = CustomRandom.nextInt(2, 5)
        val argTypes = (0 until numArgs).map { generateType(ctx.incrementCount(TupleType::class)) } + type.clone()
        return TupleType(argTypes.shuffled(CustomRandom))
    }

    override fun generateStructElementAccessExpression(type: Type, ctx: Context): StructElementAccessExpression {
        var structTypeWithType = symbolTable.globalSymbolTable.findStructWithType(type)
        if (structTypeWithType == null) {
            structTypeWithType = generateStructTypeWithType(type, ctx)
        }
        val structExpression =
            generateExpression(
                structTypeWithType,
                ctx.incrementCount(StructElementAccessExpression::class).setRequiredType(type)
            )
        val structType = structExpression.toType() as StructType
        val typeIndices = structTypeWithType.types.mapIndexed { index, triple -> triple to index }
            .filter { it.first.second == type }
            .filter { structType.argumentsToOwnershipMap[it.second].second != OwnershipState.INVALID }
            .map { it.first.first }
        val chosenElement = typeIndices.random(CustomRandom)
        if (type.getOwnership() == OwnershipModel.MOVE) {
            val newOwnershipState = if (ctx.previousIncrement in PartialMoveExpression::class.subclasses()) {
                /* A partial move, so set the ownership state to partially valid */
                OwnershipState.PARTIALLY_VALID
            } else {
                /* Not a partial move, so set the ownership state to completely invalid */
                OwnershipState.INVALID
            }
            val elementIndex = structType.types.indexOfFirst { it.first == chosenElement }
            structType.argumentsToOwnershipMap[elementIndex] =
                structType.argumentsToOwnershipMap[elementIndex].copy(second = newOwnershipState)
        }

        val elementIndex = structType.types.indexOfFirst { it.first == chosenElement }
        if (ctx.previousIncrement == ReferenceExpression::class) {
            structType.argumentsToOwnershipMap[elementIndex] =
                structType.argumentsToOwnershipMap[elementIndex].copy(second = OwnershipState.BORROWED)
        }
        if (ctx.previousIncrement == MutableReferenceExpression::class) {
            structType.argumentsToOwnershipMap[elementIndex] =
                structType.argumentsToOwnershipMap[elementIndex].copy(second = OwnershipState.MUTABLY_BORROWED)
        }
        return StructElementAccessExpression(structExpression, chosenElement, symbolTable)
    }

    private fun generateStructTypeWithType(type: Type, ctx: Context): StructType {
        return createNewStructType(ctx, type)
    }

    override fun generateVariable(type: Type, ctx: Context): Variable {
        val mutableRequired = ctx.getDepthLast(MutableReferenceExpression::class) > 0
        val value = symbolTable.getRandomVariableOfType(type, ctx.requiredType, ctx, mutableRequired)
        val variableNode = if (value == null) {
            Logger.logText("No Variable found for ${type.toRust()}, so create declaration", ctx)
            // No variables found for given type, so a declaration is created and that statement is added to the list for chaining later
            val declaration = generateDependantDeclarationOfType(
                type,
                mutableRequired,
                ctx = ctx.forDependantDeclaration().incrementCount(Variable::class)
            )
            dependantStatements.add(declaration)
//            if (type is ReferencingTypes) {
//                if (type is MutableReferenceType) MutableReferenceExpression(Variable(declaration.variableName, symbolTable),symbolTable) else ReferenceExpression(Variable(declaration.variableName, symbolTable), symbolTable)
            Variable(declaration.variableName, symbolTable)
        } else {
            Variable(value.first, symbolTable)
        }

        /* The variable is to be moved instead of copied */
        if (type.getOwnership() == OwnershipModel.MOVE) {
            if (ctx.previousIncrement in PartialMoveExpression::class.subclasses()) {
                /* A partial move, so set the ownership state to partially valid */
                symbolTable.setVariableOwnershipState(variableNode.value, OwnershipState.PARTIALLY_VALID)
            } else {
                /* Not a partial move, so set the ownership state to completely invalid */
                symbolTable.setVariableOwnershipState(variableNode.value, OwnershipState.INVALID)
            }
        }

        if (ctx.getDepthLast(ReferenceExpression::class) > 0) {
            symbolTable.setVariableOwnershipState(variableNode.value, OwnershipState.BORROWED)
        }

        if (ctx.getDepthLast(MutableReferenceExpression::class) > 0) {
            symbolTable.setVariableOwnershipState(variableNode.value, OwnershipState.MUTABLY_BORROWED)
        }
        return variableNode
    }

    override fun generateGroupedExpression(type: Type, ctx: Context): GroupedExpression = GroupedExpression(
        generateExpression(type, ctx.incrementCount(GroupedExpression::class)), symbolTable
    )

    override fun generateBlockExpression(type: Type, ctx: Context): BlockExpression {
        val body = generateStatementBlock(
            type,
            ctx.incrementCount(BlockExpression::class),
            generateEndingBlockExpression(type, ctx)
        )
        return BlockExpression(body, type.clone(), symbolTable)
    }

    private fun generateStatementBlock(type: Type, ctx: Context, expression: Expression?): StatementBlock {
        val newScope = symbolTable.enterScope()
        return ASTGenerator(newScope)(ctx.withSymbolTable(newScope), type, expression)
    }

    override fun generateIfElseExpression(type: Type, ctx: Context): IfElseExpression {
        val expressionEndIf = generateEndingBlockExpression(type, ctx)
        val expressionEndElse = generateEndingBlockExpression(type, ctx)
        return IfElseExpression(
            generateExpression(BoolType, ctx.incrementCount(IfElseExpression::class)),
            generateStatementBlock(type, ctx.incrementCount(IfElseExpression::class), expressionEndIf),
            generateStatementBlock(type, ctx.incrementCount(IfElseExpression::class), expressionEndElse),
            type,
            symbolTable
        )
    }

    override fun generateIfExpression(type: Type, ctx: Context): IfExpression {
        val expressionEndIf = generateEndingBlockExpression(type, ctx)
        return IfExpression(
            generateExpression(BoolType, ctx.incrementCount(IfExpression::class)),
            generateStatementBlock(VoidType, ctx.incrementCount(IfExpression::class), expressionEndIf),
            symbolTable
        )
    }

    override fun generateLoopExpression(type: Type, ctx: Context): LoopExpression {
        val expressionEndLoop = generateEndingBlockExpression(type, ctx)
        return LoopExpression(
            generateStatementBlock(
                type,
                ctx.incrementCount(LoopExpression::class).setLoopReturnType(type),
                expressionEndLoop
            ),
            symbolTable
        )
    }

    private fun generateEndingBlockExpression(type: Type, ctx: Context): Expression? {
        val expressionEndIf =
            if (type.memberTypes().count { it is ReferencingTypes } > 0 || ctx.getDepth(ReferencingTypes::class) > 0) {
                generateExpression(type, ctx.incrementCount(StatementBlock::class))
            } else null
        return expressionEndIf
    }

    override fun generateReferenceExpression(type: Type, ctx: Context): ReferenceExpression {
        if (type is ReferenceType) {
            val internalExpression =
                generateExpression(type.internalType, ctx.incrementCount(ReferenceExpression::class))
            return ReferenceExpression(internalExpression, symbolTable)
        }
        throw IllegalArgumentException("Invalid type $type")
    }

    override fun generateMutableReferenceExpression(type: Type, ctx: Context): MutableReferenceExpression {
        if (type is MutableReferenceType) {
            val internalExpression =
                generateExpression(type.internalType, ctx.incrementCount(MutableReferenceExpression::class))
            return MutableReferenceExpression(internalExpression, symbolTable)
        }
        throw IllegalArgumentException("Invalid type $type")
    }

    override fun generateDereferenceExpression(type: Type, ctx: Context): DereferenceExpression {
        val mutableRequired = ctx.getDepthLast(MutableReferenceExpression::class) > 0
        val internalExpression =
            generateExpression(
                if (mutableRequired) MutableReferenceType(type) else ReferenceType(type),
                ctx.incrementCount(DereferenceExpression::class)
            )
        return DereferenceExpression(internalExpression, symbolTable)
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
        if (functionData == null || selectionManager.choiceGenerateNewFunctionWeightings(ctx).randomByWeights()) {
            val newFunctionType = generateFunction(type.clone(), ctx)
            return FunctionCallExpression(
                newFunctionType.first,
                newFunctionType.second.args.map {
                    generateExpression(it, ctx.incrementCount(FunctionCallExpression::class))
                },
                symbolTable
            )
        } else {
            return FunctionCallExpression(
                functionData.first,
                (functionData.second.type as FunctionType).args.map {
                    generateExpression(
                        it, ctx.incrementCount(FunctionCallExpression::class)
                    )
                },
                symbolTable
            )
        }
    }

    private fun generateFunction(returnType: Type, ctx: Context): Pair<String, FunctionType> {
        val numArgs = CustomRandom.nextInt(5)
        val argTypes = (0 until numArgs).map { generateType(ctx.incrementCount(FunctionType::class)) }
        val symbolTableForFunction = SymbolTable(
            null, symbolTable.functionSymbolTable, symbolTable.globalSymbolTable
        )
        val functionName = IdentGenerator.generateFunctionName()
        val functionDefinition = FunctionDefinition(
            returnType,
            functionName,
            argTypes.associateBy { IdentGenerator.generateVariable() },
            ASTGenerator(symbolTableForFunction)
            (
                ctx.incrementCount(FunctionCallExpression::class)
                    .resetContextForFunction()
                    .setReturnExpressionType(returnType)
                    .withSymbolTable(symbolTableForFunction)
                    .withFunctionName(functionName),
                returnType
            )
        )
        val functionType = FunctionType(returnType, argTypes)
        symbolTable.functionSymbolTable[functionDefinition.functionName] =
            IdentifierData(functionType, false, OwnershipState.VALID)
        symbolTable.functionSymbolTable.addFunction(functionDefinition)
        return functionDefinition.functionName to functionType
    }

    override fun generateStructInstantiationExpression(type: Type, ctx: Context): StructInstantiationExpression {
        if (type !is StructType) {
            throw IllegalArgumentException("Type is not a struct type")
        } else {
            return StructInstantiationExpression(
                structName = type.structName,
                args = type.types.map {
                    it.first to generateExpression(it.second, ctx.incrementCount(StructInstantiationExpression::class))
                },
                symbolTable
            )
        }
    }

    /** Type generators **/

    override fun selectRandomType(ctx: Context): KClass<out Type> {
        val pickRandomByWeight = selectionManager.availableTypesWeightings(ctx).pickRandomByWeight()
        Logger.logText("Picking type: $pickRandomByWeight", ctx)
        return pickRandomByWeight
    }

    override fun generateBoolType(ctx: Context) = BoolType

    override fun generateI8Type(ctx: Context) = I8Type

    override fun generateI16Type(ctx: Context) = I16Type

    override fun generateI32Type(ctx: Context) = I32Type

    override fun generateI64Type(ctx: Context) = I64Type

    override fun generateI128Type(ctx: Context) = I128Type

    override fun generateF32Type(ctx: Context) = F32Type

    override fun generateF64Type(ctx: Context) = F64Type

    override fun generateStringType(ctx: Context) = StringType
    override fun generateVoidType(ctx: Context): VoidType = VoidType
    override fun generateReferenceType(ctx: Context): ReferenceType {
        val internalType = generateType(ctx.incrementCount(ReferenceType::class))
        return ReferenceType(internalType)
    }

    override fun generateMutableReferenceType(ctx: Context): MutableReferenceType {
        val internalType = generateType(ctx.incrementCount(MutableReferenceType::class))
        return MutableReferenceType(internalType)
    }

    override fun generateTupleType(ctx: Context): TupleType {
        val randomTupleType = symbolTable.globalSymbolTable.getRandomTuple()
        /* Create a new struct if the choice was made to, or if the choice was made not to but there are no structs
           currently available */
        if (randomTupleType == null || selectionManager.choiceGenerateNewTupleWeightings(ctx).randomByWeights()) {
            /* Generate Tuple Definition */
            val numArgs = CustomRandom.nextInt(2, 5)
            val argTypes = (0 until numArgs).map { generateType(ctx.incrementCount(TupleType::class)) }
            val tupleType = TupleType(argTypes)
            symbolTable.globalSymbolTable.addTupleType(tupleType)
            return tupleType
        } else {
            return randomTupleType
        }
    }

    override fun generateStructType(ctx: Context): StructType {
        val randomStructType = symbolTable.globalSymbolTable.getRandomStruct()
        /* Create a new struct if the choice was made to, or if the choice was made not to but there are no structs
           currently available */
        if (selectionManager.choiceGenerateNewStructWeightings(ctx).randomByWeights() || randomStructType == null) {
            return createNewStructType(ctx)
        } else {
            return randomStructType.second.type.clone() as StructType
        }
    }

    private fun wrapWithLifetimeParameters(type: Type): Type {
        return when (type) {
            is RecursiveType -> when (type) {
                is StructType -> LifetimeParameterizedType(
                    type.copy(
                        types = type.types.map {
                            it.first to wrapWithLifetimeParameters(it.second)
                        }
                    )
                )
                is TupleType -> type.copy(types = type.types.map { wrapWithLifetimeParameters(it) })
            }
            is ReferencingTypes -> {
                when (type) {
                    is MutableReferenceType -> LifetimeParameterizedType(type.copy(wrapWithLifetimeParameters(type.internalType)))
                    is ReferenceType -> LifetimeParameterizedType(type.copy(wrapWithLifetimeParameters(type.internalType)))
                }
            }
            else -> type
        }
    }

    private fun createNewStructType(ctx: Context, specificType: Type? = null): StructType {
        val structName = IdentGenerator.generateStructName()
        /* Generate Struct Definition */
        val numArgs = CustomRandom.nextInt(1, 5)
        val argTypes =
            (0 until numArgs).map {
                IdentGenerator.generateVariable() to generateType(ctx.incrementCount(StructType::class))
            }.toMutableList()

        if (specificType != null) {
            argTypes += IdentGenerator.generateVariable() to specificType
        }

        val structType = StructType(structName, argTypes)
        symbolTable.globalSymbolTable[structName] = IdentifierData(structType, false, OwnershipState.VALID)
        symbolTable.globalSymbolTable.addStruct(StructDefinition(wrapWithLifetimeParameters(structType) as LifetimeParameterizedType<StructType>))
        return structType
    }

    fun generateCLIArgumentsForLiteralType(type: CLIInputType, ctx: Context): String {
        return when (type) {
            BoolType -> generateBooleanLiteral(type, ctx).value.toString()
            F32Type -> generateFloat32Literal(type, ctx).value.toString()
            F64Type -> generateFloat64Literal(type, ctx).value.toString()
            I128Type -> generateInt128Literal(type, ctx).value.toString()
            I16Type -> generateInt16Literal(type, ctx).value.toString()
            I32Type -> generateInt32Literal(type, ctx).value.toString()
            I64Type -> generateInt64Literal(type, ctx).value.toString()
            I8Type -> generateInt8Literal(type, ctx).value.toString()
            StringType -> generateStringLiteral(type, ctx).value
        }
    }
}
