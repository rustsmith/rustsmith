package com.rustsmith.generation

import AbstractASTGenerator
import com.andreapivetta.kolor.Color
import com.rustsmith.CustomRandom
import com.rustsmith.ast.*
import com.rustsmith.exceptions.ExpressionGenerationRejectedException
import com.rustsmith.exceptions.NoAvailableExpressionException
import com.rustsmith.exceptions.NoAvailableTypeException
import com.rustsmith.exceptions.StatementGenerationRejectedException
import com.rustsmith.logging.Logger
import com.rustsmith.randomByWeights
import com.rustsmith.selectionManager
import com.rustsmith.subclasses
import java.math.BigInteger
import kotlin.math.min
import kotlin.math.pow
import kotlin.random.asJavaRandom
import kotlin.random.nextUInt
import kotlin.random.nextULong
import kotlin.reflect.KClass

const val MAX_TRIES_FOR_TYPES = 10

class ASTGenerator(
    private val symbolTable: SymbolTable,
    private val failFast: Boolean,
    private val identGenerator: IdentGenerator
) : AbstractASTGenerator {
    private val dependantStatements = mutableListOf<Declaration>()

    operator fun invoke(ctx: Context, type: Type = VoidType, depth: Int? = null): StatementBlock {
        var currentCtx = ctx.enterScope()
        val statements = mutableListOf<Statement>()
        while (selectionManager.choiceGenerateNewStatementWeightings(currentCtx).randomByWeights()) {
            val statement = generateStatement(currentCtx.forNewStatement())
            statements.addAll(dependantStatements)
            statements.add(statement)
            dependantStatements.clear()
            currentCtx = currentCtx.incrementStatementCount(statement)
            if (statement::class in BlockEndingStatement::class.subclasses()) break
        }
        val statementsWithDependants = dependantStatements + statements
        return if (type == VoidType) {
            StatementBlock(statementsWithDependants, symbolTable)
        } else {
            val newCtx = currentCtx.withLifetimeRequirement(
                min(
                    ctx.lifetimeRequirement ?: symbolTable.depth.value, depth ?: symbolTable.depth.value
                )
            )
            val finalExpression = generateExpression(type, newCtx).toStatement(false)
            StatementBlock(statementsWithDependants + (dependantStatements + finalExpression), symbolTable)
        }
    }

    /** Statement Generation **/

    fun generateConstantDeclaration(ctx: Context): ConstDeclaration {
        val constName = identGenerator.generateConst()
        while (true) {
            val type = generateType(ctx)
            if (type is LiteralType && type !is StringType) {
                symbolTable.root()[constName] = IdentifierData(type, false, OwnershipState.VALID, 0, true)
                return ConstDeclaration(type, constName, generateLiteral(type, ctx), symbolTable)
            } else {
                continue
            }
        }
    }

    override fun selectRandomStatement(ctx: Context): KClass<out Statement> {
        val pickRandomByWeight = selectionManager.availableStatementsWeightings(ctx).pickRandomByWeight()
        Logger.logText("Picking statement: $pickRandomByWeight", ctx, Color.MAGENTA)
        return pickRandomByWeight
    }

    override fun generateStatement(ctx: Context): Statement {
        val symbolTableSnapshot = symbolTable.snapshot()
        var currentCtx = ctx
        while (true) {
            val selectedStatement = selectRandomStatement(currentCtx)
            try {
                return generateSpecificStatement(selectedStatement, ctx)
            } catch (e: StatementGenerationRejectedException) {
                dependantStatements.forEach {
                    symbolTable.symbolMap.remove(it.variableName)
                }
                dependantStatements.clear()
                symbolTable.mergeSnapshot(symbolTableSnapshot)
                currentCtx = currentCtx.addFailedNode(selectedStatement)
                Logger.logText(
                    "Node $selectedStatement not possible currently, trying different node", currentCtx, Color.LIGHT_RED
                )
            } catch (e: NoAvailableExpressionException) {
                dependantStatements.forEach {
                    symbolTable.symbolMap.remove(it.variableName)
                }
                dependantStatements.clear()
                symbolTable.mergeSnapshot(symbolTableSnapshot)
                currentCtx = currentCtx.addFailedNode(selectedStatement)
                Logger.logText(
                    "Node $selectedStatement not possible currently, trying different node", currentCtx, Color.LIGHT_RED
                )
            }
        }
    }

    override fun generateVoidLiteral(type: Type, ctx: Context): VoidLiteral = VoidLiteral(symbolTable)
    override fun generateCLIArgumentAccessExpression(type: Type, ctx: Context): CLIArgumentAccessExpression {
        if (selectionManager.choiceGenerateNewCLIArgumentWeightings(ctx)
            .randomByWeights() || !symbolTable.globalSymbolTable.commandLineTypes.contains(type)
        ) {
            symbolTable.globalSymbolTable.commandLineTypes.add(type as LiteralType)
            return CLIArgumentAccessExpression(
                symbolTable.globalSymbolTable.commandLineTypes.mapIndexed { i, t -> i to t }
                    .filter { it.second == type }.random(CustomRandom).first + 1,
                type, symbolTable
            )
        }
        return CLIArgumentAccessExpression(
            symbolTable.globalSymbolTable.commandLineTypes.mapIndexed { i, t -> i to t }
                .filter { it.second == type }.random(CustomRandom).first + 1,
            type, symbolTable
        )
    }

    override fun generateExpressionStatement(ctx: Context): ExpressionStatement {
        val symbolTableSnapshot = symbolTable.snapshot()
        var currentCtx = ctx
        var count = 0
        while (count < MAX_TRIES_FOR_TYPES) {
            count++
            try {
                val type = generateType(currentCtx.incrementCount(ExpressionStatement::class))
                try {
                    return ExpressionStatement(
                        generateExpression(
                            type, ctx.incrementCount(ExpressionStatement::class)
                        ),
                        true, symbolTable
                    )
                } catch (e: NoAvailableExpressionException) {
                    dependantStatements.forEach {
                        symbolTable.symbolMap.remove(it.variableName)
                    }
                    dependantStatements.clear()
                    symbolTable.mergeSnapshot(symbolTableSnapshot)
                    currentCtx = currentCtx.addFailedNode(type::class)
                    Logger.logText("Expression Statement generation failed for type $type", currentCtx, Color.LIGHT_RED)
                }
            } catch (e: NoAvailableTypeException) {
                dependantStatements.forEach {
                    symbolTable.symbolMap.remove(it.variableName)
                }
                dependantStatements.clear()
                symbolTable.mergeSnapshot(symbolTableSnapshot)
                // Types have been exhausted to make an expression generation, throw statement not generated exception
                Logger.logText(
                    "Expression Statement generation failed as no types available", currentCtx, Color.LIGHT_RED
                )
                throw StatementGenerationRejectedException()
            }
        }
        dependantStatements.forEach {
            symbolTable.symbolMap.remove(it.variableName)
        }
        dependantStatements.clear()
        symbolTable.mergeSnapshot(symbolTableSnapshot)
        // MAX_TRIES exceeded, give up on making Expression Statement
        throw StatementGenerationRejectedException()
    }

    override fun generateDeclaration(ctx: Context): Declaration {
        val symbolTableSnapshot = symbolTable.snapshot()
        var currentCtx = ctx
        var count = 0
        while (count < MAX_TRIES_FOR_TYPES) {
            try {
                val declarationType = generateType(ctx.incrementCount(Declaration::class))
                try {
                    return generateDependantDeclarationOfType(declarationType, ctx = ctx)
                } catch (e: NoAvailableExpressionException) {
                    dependantStatements.forEach {
                        symbolTable.symbolMap.remove(it.variableName)
                    }
                    dependantStatements.clear()
                    symbolTable.mergeSnapshot(symbolTableSnapshot)
                    currentCtx = currentCtx.addFailedNode(declarationType::class)
                    Logger.logText(
                        "Declaration generation failed for type $declarationType", currentCtx, Color.LIGHT_RED
                    )
                }
            } catch (e: NoAvailableTypeException) {
                dependantStatements.forEach {
                    symbolTable.symbolMap.remove(it.variableName)
                }
                dependantStatements.clear()
                symbolTable.mergeSnapshot(symbolTableSnapshot)
                // Types have been exhausted to make an expression generation, throw statement not generated exception
                Logger.logText("Declaration generation failed as no types available", currentCtx, Color.LIGHT_RED)
                throw StatementGenerationRejectedException()
            }
            count++
        }
        dependantStatements.forEach {
            symbolTable.symbolMap.remove(it.variableName)
        }
        dependantStatements.clear()
        symbolTable.mergeSnapshot(symbolTableSnapshot)
        // MAX_TRIES exceeded, give up on making Expression Statement
        throw StatementGenerationRejectedException()
    }

    private fun generateDependantDeclarationOfType(
        type: Type,
        mutable: Boolean = CustomRandom.nextBoolean(),
        ctx: Context
    ): Declaration {

        val variableName = identGenerator.generateVariable()
        val expression = generateExpression(type, ctx.incrementCount(Declaration::class))
        symbolTable[variableName] =
            IdentifierData(expression.toType().clone(), mutable, OwnershipState.VALID, symbolTable.depth.value)
        return Declaration(mutable, type.clone(), variableName, expression, symbolTable)
    }

    override fun generateAssignment(ctx: Context): Assignment {
        val value = symbolTable.getRandomMutableVariable(ctx)
        return if (value == null) {
            if (failFast) throw StatementGenerationRejectedException()
            Logger.logText("No LHSAssignment found, so create declaration", ctx, Color.LIGHT_BLUE)
            // No variables found, so a declaration is created and that statement is added to the list for chaining later
            val declaration = generateDependantDeclarationOfType(
                generateType(ctx.incrementCount(Assignment::class)),
                true,
                ctx.forDependantDeclaration().incrementCount(Assignment::class)
            )
            dependantStatements.add(declaration)
            val expression = generateExpression(
                declaration.type, ctx.incrementCount(Assignment::class).withLifetimeRequirement(symbolTable.depth.value)
            )
            Assignment(Variable(declaration.variableName, symbolTable), expression, symbolTable)
        } else {
            val lhsAssignmentType = value.node.toType()
            val lhsNodeAndRhsExpression: Pair<LHSAssignmentNode, Expression> =
                if (lhsAssignmentType is MutableReferenceType) {
                    val useDereferenceLhs = CustomRandom.nextBoolean() || !value.identifierData.mutable
                    val lhsExpression =
                        if (useDereferenceLhs) DereferenceExpression(value.node, value.node.symbolTable) else value.node
                    val exprType = if (useDereferenceLhs) lhsAssignmentType.internalType else lhsAssignmentType
                    lhsExpression to generateExpression(
                        exprType,
                        ctx.incrementCount(Assignment::class).withAssignmentNode(value.node.rootNode())
                            .withLifetimeRequirement(value.identifierData.depth)
                    )
                } else if (lhsAssignmentType is BoxType) {
                    val useDereferenceLhs = CustomRandom.nextBoolean() || !value.identifierData.mutable
                    val lhsExpression = if (useDereferenceLhs) BoxDereferenceExpression(
                        value.node, value.node.symbolTable
                    ) else value.node
                    val exprType = if (useDereferenceLhs) lhsAssignmentType.internalType else lhsAssignmentType
                    lhsExpression to generateExpression(
                        exprType,
                        ctx.incrementCount(Assignment::class).withAssignmentNode(value.node.rootNode())
                            .withLifetimeRequirement(value.identifierData.depth)
                    )
                } else {
                    value.node to generateExpression(
                        lhsAssignmentType,
                        ctx.incrementCount(Assignment::class).withAssignmentNode(value.node.rootNode())
                            .withLifetimeRequirement(value.identifierData.depth)
                    )
                }
            Assignment(lhsNodeAndRhsExpression.first, lhsNodeAndRhsExpression.second, symbolTable)
        }
    }

    override fun generatePrintElementStatement(ctx: Context): PrintElementStatement {
        val variableName = symbolTable.getOwnedVariables(false)
            .filter { ctx.assignmentRootNode?.map { variable -> variable.value }?.contains(it)?.not() ?: true }
            .randomOrNull(
                CustomRandom
            )
        return if (variableName == null) {
            if (failFast) throw StatementGenerationRejectedException()
            val declaration = generateDependantDeclarationOfType(
                generateType(ctx.incrementCount(Assignment::class)),
                true,
                ctx.forDependantDeclaration().incrementCount(Assignment::class)
            )
            dependantStatements.add(declaration)
            if (declaration.type.getOwnership() == OwnershipModel.MOVE) {
                symbolTable.setVariableOwnershipState(
                    declaration.variableName, OwnershipState.INVALID, symbolTable.depth.value
                )
            }
            PrintElementStatement(declaration.variableName, symbolTable)
        } else {
            if (symbolTable[variableName]!!.type.getOwnership() == OwnershipModel.MOVE) {
                symbolTable.setVariableOwnershipState(
                    variableName, OwnershipState.INVALID, symbolTable[variableName]!!.depth
                )
            }
            PrintElementStatement(variableName, symbolTable)
        }
    }

    override fun generateReturnStatement(ctx: Context): ReturnStatement {
        val expression = generateExpression(ctx.returnExpressionType!!, ctx.incrementCount(ReturnStatement::class))
        return ReturnStatement(expression, symbolTable)
    }

    override fun generateBreakStatement(ctx: Context): BreakStatement {
        return BreakStatement(symbolTable)
    }

    /** Expression generation **/

    override fun selectRandomExpression(type: Type, ctx: Context): KClass<out Expression> {
        val availableExpressionsWeightings = selectionManager.availableExpressionsWeightings(ctx, type)
        val pickRandomByWeight = availableExpressionsWeightings.pickRandomByWeight()
        Logger.logText("Picking expression $pickRandomByWeight for type:${type.toRust()}", ctx, Color.GREEN)
        return pickRandomByWeight
    }

    override fun generateExpression(type: Type, ctx: Context): Expression {
        val symbolTableSnapshot = symbolTable.snapshot()
        var currentCtx = ctx
        while (true) {
            val currentlyChosenExpression = selectRandomExpression(type, currentCtx)
            try {
                return generateSpecificExpression(currentlyChosenExpression, type, ctx)
            } catch (e: ExpressionGenerationRejectedException) {
                symbolTable.mergeSnapshot(symbolTableSnapshot)
                currentCtx = currentCtx.addFailedNode(currentlyChosenExpression)
                Logger.logText(
                    "Node $currentlyChosenExpression not possible currently, trying different node",
                    currentCtx,
                    Color.LIGHT_RED
                )
            }
        }
    }

    override fun generateElementAccess(type: Type, ctx: Context): ElementAccess {
        val symbolTableVariables = symbolTable.getAllVariables().filter { it.value.validity != OwnershipState.INVALID }

        val allAvailableElementAccesses = mutableListOf<Expression>()

        val mutableRequired =
            ctx.getDepthLast(MutableReferenceExpression::class) > 0 || ctx.getDepthLast(VectorPushExpression::class) > 0
        val availableVariables = symbolTable.getRandomVariableOfType(type, ctx.requiredType, ctx, mutableRequired)

        allAvailableElementAccesses.addAll(availableVariables.map { Variable(it, symbolTable) })

        // Partially or completely valid structs and tuples (which might have the right type inside)
        var containerVariables = symbolTableVariables.filter { it.value.type is ContainerType }

        if ((
            type.memberTypes()
                .count { it is ReferencingTypes } > 0 || ctx.getDepth(ReferencingExpressions::class) > 0
            ) &&
            ctx.lifetimeRequirement != null
        ) {
            containerVariables = containerVariables.filter {
                it.value.depth <= ctx.lifetimeRequirement
            }.toMutableMap()
        }

        allAvailableElementAccesses.addAll(
            containerVariables.filter {
                if (ctx.getDepthLast(ReferenceExpression::class) > 0) it.value.validity.borrowable() else it.value.validity.movable()
            }.filter { it.value.mutable == mutableRequired }.flatMap {
                findSubExpressionsForExpression(
                    Variable(
                        it.key,
                        symbolTable
                    ),
                    type
                )
            }
        )

        val potentialExpression = allAvailableElementAccesses.randomOrNull(CustomRandom)

        val expression = if (potentialExpression != null) {
            // Traverse chosen expression and actually call relevant functions
            generateRelevantElementAccess(potentialExpression as ElementAccessExpression, type, ctx)
        } else {
            if ((
                type.memberTypes()
                    .count { it is ReferencingTypes } > 0 || ctx.getDepth(ReferencingExpressions::class) > 0
                ) &&
                ctx.lifetimeRequirement != null && ctx.lifetimeRequirement < symbolTable.depth.value
            ) {
                throw ExpressionGenerationRejectedException()
            }
            val declaration = generateDependantDeclarationOfType(
                type, mutableRequired, ctx = ctx.forDependantDeclaration().incrementCount(ElementAccess::class)
            )
            dependantStatements.add(declaration)
            generateVariable(declaration.variableName, type, ctx)
        }

        return ElementAccess(expression, symbolTable)
    }

    private fun generateRelevantElementAccess(
        expression: ElementAccessExpression,
        type: Type,
        ctx: Context
    ): Expression {
        return when (expression) {
            is StructElementAccessExpression -> generateStructElementAccessExpression(
                expression.expression,
                expression.elementName,
                type,
                ctx
            )

            is TupleElementAccessExpression -> generateTupleElementAccessExpression(
                expression.expression,
                expression.index,
                type,
                ctx
            )

            is Variable -> generateVariable(expression.value, type, ctx)
//            is VectorAccess -> generateVectorAccess(expression.vectorExpression, expression.indexExpression, type, ctx)
            else -> TODO()
        }
    }

    private fun findSubExpressionsForExpression(expression: Expression, requiredType: Type): List<Expression> {
        return when (val type = expression.toType() as ContainerType) {
            is StructType -> {
                val validContainerTypes =
                    type.types.filterIndexed { index, typePair -> type.argumentsToOwnershipMap[index].second !== OwnershipState.INVALID && typePair.second is ContainerType }
                val expressionsFromContainerElements = validContainerTypes.flatMap {
                    findSubExpressionsForExpression(
                        StructElementAccessExpression(
                            expression,
                            it.first,
                            symbolTable
                        ),
                        requiredType
                    )
                }.toMutableList()
                val validElementAccessForRequiredType =
                    type.types.filterIndexed { index, typePair -> typePair.second == requiredType && type.argumentsToOwnershipMap[index].second == OwnershipState.VALID }
                expressionsFromContainerElements.addAll(
                    validElementAccessForRequiredType.map {
                        StructElementAccessExpression(
                            expression,
                            it.first,
                            symbolTable
                        )
                    }
                )
                expressionsFromContainerElements
            }

            is TupleType -> {
                val validContainerTypes =
                    type.types.filterIndexed { index, typePair -> type.argumentsToOwnershipMap[index].second !== OwnershipState.INVALID && typePair is ContainerType }
                val expressionsFromContainerElements = validContainerTypes.flatMap {
                    findSubExpressionsForExpression(
                        TupleElementAccessExpression(
                            expression,
                            type.types.indexOf(it),
                            symbolTable
                        ),
                        requiredType
                    )
                }.toMutableList()
                val validElementAccessForRequiredType =
                    type.types.filterIndexed { index, typePair -> typePair == requiredType && type.argumentsToOwnershipMap[index].second == OwnershipState.VALID }
                expressionsFromContainerElements.addAll(
                    validElementAccessForRequiredType.map {
                        TupleElementAccessExpression(
                            expression,
                            type.types.indexOf(it),
                            symbolTable
                        )
                    }
                )
                expressionsFromContainerElements
            }
        }
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

    override fun generateUInt8Literal(type: Type, ctx: Context): UInt8Literal {
        return UInt8Literal(CustomRandom.nextUInt(2.0.pow(8).toUInt()), symbolTable)
    }

    override fun generateUInt16Literal(type: Type, ctx: Context): UInt16Literal {
        return UInt16Literal(CustomRandom.nextUInt(2.0.pow(16).toUInt()), symbolTable)
    }

    override fun generateUInt32Literal(type: Type, ctx: Context): UInt32Literal {
        return UInt32Literal(CustomRandom.nextUInt(2.0.pow(32).toUInt()), symbolTable)
    }

    override fun generateUInt64Literal(type: Type, ctx: Context): UInt64Literal {
        return UInt64Literal(CustomRandom.nextULong(), symbolTable)
    }

    override fun generateUInt128Literal(type: Type, ctx: Context): UInt128Literal {
        return UInt128Literal(BigInteger(127, CustomRandom.asJavaRandom()), symbolTable)
    }

    override fun generateUSizeLiteral(type: Type, ctx: Context): USizeLiteral {
        return USizeLiteral(CustomRandom.nextULong(), symbolTable)
    }

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
            if (!failFast) {
                val membersWithReferenceType = type.memberTypes().filterIsInstance<ReferencingTypes>()
                membersWithReferenceType.forEach {
                    dependantStatements.add(
                        generateDependantDeclarationOfType(
                            it, ctx = ctx.incrementCount(StructInstantiationExpression::class)
                        )
                    )
                }
            }
            return TupleLiteral(
                type.types.map { generateExpression(it, ctx.incrementCount(TupleLiteral::class)) }, symbolTable
            )
        }
        throw Exception("Incompatible Type")
    }

    private fun generateTupleElementAccessExpression(
        tupleExpression: Expression,
        tupleIndex: Int,
        type: Type,
        ctx: Context
    ): TupleElementAccessExpression {
        val tupleExpressionGenerated = generateRelevantElementAccess(
            tupleExpression as ElementAccessExpression,
            tupleExpression.toType(),
            ctx.incrementCount(TupleElementAccessExpression::class)
        )
        val tupleType = tupleExpressionGenerated.toType() as TupleType
//        val typeIndices =
//            tupleType.types.mapIndexed { index, pair -> pair to index }.filter { it.first == type }.map { it.second }
//                .filter { tupleType.argumentsToOwnershipMap[it].second != OwnershipState.INVALID }
        if (type.getOwnership() == OwnershipModel.MOVE) {
            val newOwnershipState =
                if (ctx.previousIncrement in PartialMoveExpression::class.subclasses()) { /* A partial move, so set the ownership state to partially valid */
                    OwnershipState.PARTIALLY_VALID
                } else { /* Not a partial move, so set the ownership state to completely invalid */
                    OwnershipState.INVALID
                }
            tupleType.argumentsToOwnershipMap[tupleIndex] =
                tupleType.argumentsToOwnershipMap[tupleIndex].copy(second = newOwnershipState)
        }

        if (ctx.previousIncrement == ReferenceExpression::class) {
            tupleType.argumentsToOwnershipMap[tupleIndex] =
                tupleType.argumentsToOwnershipMap[tupleIndex].copy(second = OwnershipState.BORROWED)
        }

        if (ctx.previousIncrement == MutableReferenceExpression::class) {
            tupleType.argumentsToOwnershipMap[tupleIndex] =
                tupleType.argumentsToOwnershipMap[tupleIndex].copy(second = OwnershipState.MUTABLY_BORROWED)
        }
        return TupleElementAccessExpression(tupleExpressionGenerated, tupleIndex, symbolTable)
    }

    private fun generateTupleTypeWithType(type: Type, ctx: Context): TupleType {
        val numArgs = CustomRandom.nextInt(2, 5)
        val argTypes = (0 until numArgs).map { generateType(ctx.incrementCount(TupleType::class)) } + type.clone()
        return TupleType(argTypes.shuffled(CustomRandom))
    }

    private fun generateStructElementAccessExpression(
        structExpression: Expression,
        elementName: String,
        type: Type,
        ctx: Context
    ): StructElementAccessExpression {
        val structExpressionGenerated = generateRelevantElementAccess(
            structExpression as ElementAccessExpression,
            structExpression.toType(),
            ctx.incrementCount(TupleElementAccessExpression::class)
        )
        val structType = structExpressionGenerated.toType() as StructType
        if (type.getOwnership() == OwnershipModel.MOVE) {
            val newOwnershipState =
                if (ctx.previousIncrement in PartialMoveExpression::class.subclasses()) { /* A partial move, so set the ownership state to partially valid */
                    OwnershipState.PARTIALLY_VALID
                } else { /* Not a partial move, so set the ownership state to completely invalid */
                    OwnershipState.INVALID
                }
            val elementIndex = structType.types.indexOfFirst { it.first == elementName }
            structType.argumentsToOwnershipMap[elementIndex] =
                structType.argumentsToOwnershipMap[elementIndex].copy(second = newOwnershipState)
        }

        val elementIndex = structType.types.indexOfFirst { it.first == elementName }
        if (ctx.previousIncrement == ReferenceExpression::class) {
            structType.argumentsToOwnershipMap[elementIndex] =
                structType.argumentsToOwnershipMap[elementIndex].copy(second = OwnershipState.BORROWED)
        }
        if (ctx.previousIncrement == MutableReferenceExpression::class) {
            structType.argumentsToOwnershipMap[elementIndex] =
                structType.argumentsToOwnershipMap[elementIndex].copy(second = OwnershipState.MUTABLY_BORROWED)
        }
        return StructElementAccessExpression(structExpressionGenerated, elementName, symbolTable)
    }

    private fun createNewStructTypeWithType(type: Type, ctx: Context): StructType {
        return createNewStructType(ctx, type)
    }

    private fun generateVariable(variableName: String, type: Type, ctx: Context): Variable {

        val variableNode = Variable(variableName, symbolTable)

        /* The variable is to be moved instead of copied */
        if (type.getOwnership() == OwnershipModel.MOVE) {
            if (ctx.previousIncrement != NonMovingExpressions::class) {
                if (ctx.previousIncrement in PartialMoveExpression::class.subclasses()) { /* A partial move, so set the ownership state to partially valid */
                    symbolTable.setVariableOwnershipState(
                        variableNode.value, OwnershipState.PARTIALLY_VALID, ctx.lifetimeRequirement
                    )
                } else { /* Not a partial move, so set the ownership state to completely invalid */
                    symbolTable.setVariableOwnershipState(
                        variableNode.value, OwnershipState.INVALID, ctx.lifetimeRequirement
                    )
                }
            }
        }

        if (ctx.getDepthLast(ReferenceExpression::class) > 0) {
            symbolTable.setVariableOwnershipState(variableNode.value, OwnershipState.BORROWED, ctx.lifetimeRequirement)
        }

        if (ctx.getDepthLast(MutableReferenceExpression::class) > 0) {
            symbolTable.setVariableOwnershipState(
                variableNode.value, OwnershipState.MUTABLY_BORROWED, ctx.lifetimeRequirement
            )
        }
        return variableNode
    }

    override fun generateGroupedExpression(type: Type, ctx: Context): GroupedExpression = GroupedExpression(
        generateExpression(type, ctx.incrementCount(GroupedExpression::class)), symbolTable
    )

    override fun generateBlockExpression(type: Type, ctx: Context): BlockExpression {
        val body = generateStatementBlock(type, ctx.incrementCount(BlockExpression::class))
        return BlockExpression(body, type.clone(), symbolTable)
    }

    override fun generateExtractOptionExpression(type: Type, ctx: Context): ExtractOptionExpression {
        val optionType = generateOptionType(ctx.incrementCount(ExtractOptionExpression::class))
        val expressionToMatch = generateExpression(optionType, ctx.incrementCount(ExtractOptionExpression::class))
        val extractedVariable = identGenerator.generateVariable()
        val matchedStatement = generateStatementBlock(type, ctx.incrementCount(ExtractOptionExpression::class), extractedVariable to optionType.type)
        val noneStatement = generateStatementBlock(type, ctx.incrementCount(ExtractOptionExpression::class))
        return ExtractOptionExpression(expressionToMatch, extractedVariable, matchedStatement, noneStatement, type, symbolTable)
    }

    private fun generateStatementBlock(type: Type, ctx: Context, addVariable: Pair<String, Type>? = null): StatementBlock {
        val currentSymbolTableDepth = symbolTable.depth.value
        val newScope = symbolTable.enterScope()
        if (addVariable != null) {
            newScope[addVariable.first] = IdentifierData(addVariable.second.clone(), false, OwnershipState.VALID, newScope.depth.value)
        }
        return ASTGenerator(newScope, failFast, identGenerator)(
            ctx.withSymbolTable(newScope), type, currentSymbolTableDepth
        )
    }

    override fun generateIfElseExpression(type: Type, ctx: Context): IfElseExpression {
        val ifBlock = generateStatementBlock(type, ctx.incrementCount(IfElseExpression::class))
        return IfElseExpression(
            generateExpression(BoolType, ctx.incrementCount(IfElseExpression::class)),
            ifBlock,
            if (mapOf(true to 0.1, false to 0.9).randomByWeights()) ifBlock else generateStatementBlock(
                type, ctx.incrementCount(IfElseExpression::class)
            ),
            type,
            symbolTable
        )
    }

    override fun generateIfExpression(type: Type, ctx: Context): IfExpression {
        return IfExpression(
            generateExpression(BoolType, ctx.incrementCount(IfExpression::class)),
            generateStatementBlock(VoidType, ctx.incrementCount(IfExpression::class)),
            symbolTable
        )
    }

    override fun generateLoopExpression(type: Type, ctx: Context): LoopExpression {
        return LoopExpression(
            generateStatementBlock(
                type, ctx.incrementCount(LoopExpression::class).setLoopReturnType(type)
            ),
            symbolTable
        )
    }

    override fun generateVectorLiteral(type: Type, ctx: Context): VectorLiteral {
        if (type is VectorType) {
            val size = CustomRandom.nextInt(1, 10)
            return VectorLiteral(
                (0 until size).map {
                    generateExpression(
                        type.type, ctx.incrementCount(VectorLiteral::class)
                    )
                },
                symbolTable
            )
        }
        throw IllegalArgumentException("Invalid type $type")
    }

    override fun generateSomeLiteral(type: Type, ctx: Context): SomeLiteral {
        if (type is OptionType) {
            return SomeLiteral(generateExpression(type.type, ctx.incrementCount(SomeLiteral::class)), symbolTable)
        }
        throw IllegalArgumentException("Invalid type $type")
    }

    override fun generateNoneLiteral(type: Type, ctx: Context): NoneLiteral {
        if (type is OptionType) {
            return NoneLiteral(type.type, symbolTable)
        }
        throw IllegalArgumentException("Invalid type $type")
    }

    override fun generateVectorAccess(type: Type, ctx: Context): VectorAccess {
        val arrayExpression = generateExpression(VectorType(type), ctx.incrementCount(VectorAccess::class))
        val indexExpression = generateExpression(USizeType, ctx.incrementCount(VectorAccess::class))
        return VectorAccess(arrayExpression, indexExpression, symbolTable)
    }

    override fun generateNewBoxExpression(type: Type, ctx: Context): NewBoxExpression {
        if (type is BoxType) {
            val internalExpression = generateExpression(type.internalType, ctx.incrementCount(NewBoxExpression::class))
            return NewBoxExpression(internalExpression, symbolTable)
        }
        throw IllegalArgumentException("Not a box type")
    }

    override fun generateTypeAliasExpression(type: Type, ctx: Context): TypeAliasExpression {
        if (type is TypeAliasType) {
            return TypeAliasExpression(
                generateExpression(
                    type.internalType, ctx.incrementCount(TypeAliasExpression::class)
                ),
                symbolTable
            )
        }
        throw IllegalArgumentException("Incorrect type")
    }

    override fun generateBoxDereferenceExpression(type: Type, ctx: Context): BoxDereferenceExpression {
        val internalExpression = generateExpression(
            BoxType(type), ctx.incrementCount(BoxDereferenceExpression::class)
        )
        return BoxDereferenceExpression(internalExpression, symbolTable)
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

    override fun generateVectorLengthExpression(type: Type, ctx: Context): VectorLengthExpression {
        val arrayExpression = generateExpression(
            generateVectorType(ctx.incrementCount(VectorLengthExpression::class)),
            ctx.incrementCount(VectorLengthExpression::class)
        )
        return VectorLengthExpression(
            arrayExpression, symbolTable
        )
    }

    override fun generateVectorPushExpression(type: Type, ctx: Context): VectorPushExpression {
        val arrayExpression = generateExpression(
            generateVectorType(ctx.incrementCount(VectorPushExpression::class)),
            ctx.incrementCount(VectorPushExpression::class)
        )
        val pushExpression = generateExpression((arrayExpression.toType() as VectorType).type, ctx)
        return VectorPushExpression(arrayExpression, pushExpression, symbolTable)
    }

    override fun generateDereferenceExpression(type: Type, ctx: Context): DereferenceExpression {
        val mutableRequired = ctx.getDepthLast(MutableReferenceExpression::class) > 0
        val internalExpression = generateExpression(
            if (mutableRequired) MutableReferenceType(type, symbolTable.depth.value.toUInt()) else ReferenceType(
                type, symbolTable.depth.value.toUInt()
            ),
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

    private fun generateEqType(ctx: Context): EqType {
        do {
            val type = generateType(ctx)
            if (type is EqType) return type
        } while (true)
    }

    private fun generateComparableType(ctx: Context): ComparableType {
        do {
            val type = generateType(ctx)
            if (type is ComparableType) return type
        } while (true)
    }

    override fun generateEqExpression(type: Type, ctx: Context): EqExpression {
        val eqType = generateEqType(ctx)
        return EqExpression(
            generateExpression(eqType, ctx.incrementCount(EqExpression::class)),
            generateExpression(eqType, ctx.incrementCount(EqExpression::class)),
            symbolTable
        )
    }

    override fun generateNEqExpression(type: Type, ctx: Context): NEqExpression {
        val eqType = generateEqType(ctx)
        return NEqExpression(
            generateExpression(eqType, ctx.incrementCount(NEqExpression::class)),
            generateExpression(eqType, ctx.incrementCount(NEqExpression::class)),
            symbolTable
        )
    }

    override fun generateGTExpression(type: Type, ctx: Context): GTExpression {
        val cmpType = generateComparableType(ctx)
        return GTExpression(
            generateExpression(cmpType, ctx.incrementCount(GTExpression::class)),
            generateExpression(cmpType, ctx.incrementCount(GTExpression::class)),
            symbolTable
        )
    }

    override fun generateGTEExpression(type: Type, ctx: Context): GTEExpression {
        val cmpType = generateComparableType(ctx)
        return GTEExpression(
            generateExpression(cmpType, ctx.incrementCount(GTEExpression::class)),
            generateExpression(cmpType, ctx.incrementCount(GTEExpression::class)),
            symbolTable
        )
    }

    override fun generateLTExpression(type: Type, ctx: Context): LTExpression {
        val cmpType = generateComparableType(ctx)
        return LTExpression(
            generateExpression(cmpType, ctx.incrementCount(LTExpression::class)),
            generateExpression(cmpType, ctx.incrementCount(LTExpression::class)),
            symbolTable
        )
    }

    override fun generateLTEExpression(type: Type, ctx: Context): LTEExpression {
        val cmpType = generateComparableType(ctx)
        return LTEExpression(
            generateExpression(cmpType, ctx.incrementCount(LTEExpression::class)),
            generateExpression(cmpType, ctx.incrementCount(LTEExpression::class)),
            symbolTable
        )
    }

    override fun generateFunctionCallExpression(type: Type, ctx: Context): FunctionCallExpression {
        val functionData = symbolTable.functionSymbolTable.getRandomFunctionOfType(type)
        val functionInformation =
            if (functionData == null || selectionManager.choiceGenerateNewFunctionWeightings(ctx).randomByWeights()) {
                generateFunction(type.clone(), ctx)
            } else {
                functionData.first to (functionData.second.type as FunctionType)
            }
        if (!failFast) {
            val membersWithReferenceType =
                functionInformation.second.args.flatMap { it.memberTypes().filterIsInstance<ReferencingTypes>() }
            membersWithReferenceType.forEach {
                dependantStatements.add(
                    generateDependantDeclarationOfType(
                        it, ctx = ctx.incrementCount(FunctionCallExpression::class)
                    )
                )
            }
        }
        return FunctionCallExpression(
            functionInformation.first,
            functionInformation.second.args.map {
                generateExpression(it, ctx.incrementCount(FunctionCallExpression::class))
            },
            symbolTable
        )
    }

    override fun generateMethodCallExpression(type: Type, ctx: Context): MethodCallExpression {
        val methodInformation: Pair<StructType, FunctionDefinition> =
            symbolTable.globalSymbolTable.getRandomMethodOfType(type) ?: generateMethod(type.clone(), ctx)
        if (!failFast) {
            val membersWithReferenceType =
                methodInformation.second.arguments.filter { it.key != "hasher" }.map { it.value }
                    .flatMap { it.memberTypes().filterIsInstance<ReferencingTypes>() }
            membersWithReferenceType.forEach {
                dependantStatements.add(
                    generateDependantDeclarationOfType(it, ctx = ctx.incrementCount(MethodCallExpression::class))
                )
            }
        }
        val structExpression = generateExpression(methodInformation.first, ctx)
        return MethodCallExpression(
            structExpression,
            methodInformation.second.functionName,
            methodInformation.second.arguments.filter { it.key != "hasher" }.map { it.value }.map {
                generateExpression(it, ctx.incrementCount(FunctionCallExpression::class))
            },
            symbolTable
        )
    }

    private fun generateFunction(returnType: Type, ctx: Context): Pair<String, FunctionType> {
        val numArgs = CustomRandom.nextInt(5)
        val argTypes = (0 until numArgs).map { generateType(ctx.incrementCount(FunctionType::class)) }
        val symbolTableForFunction = SymbolTable(
            symbolTable.root(), symbolTable.functionSymbolTable, symbolTable.globalSymbolTable
        )
        val arguments = argTypes.associateBy { identGenerator.generateVariable() }
        arguments.forEach {
            symbolTableForFunction[it.key] = IdentifierData(it.value, false, OwnershipState.VALID, 0)
        }
        val bodySymbolTable = symbolTableForFunction.enterScope()
        val functionName = identGenerator.generateFunctionName()
        val functionDefinition = FunctionDefinition(
            returnType, functionName,
            arguments + mapOf(
                "hasher" to MutableReferenceType(
                    DefaultHasher, symbolTable.depth.value.toUInt()
                )
            ),
            ASTGenerator(bodySymbolTable, failFast, identGenerator)(
                ctx.incrementCount(FunctionCallExpression::class).resetContextForFunction()
                    .setReturnExpressionType(returnType).withSymbolTable(bodySymbolTable)
                    .withFunctionName(functionName),
                returnType
            ),
            CustomRandom.nextBoolean(), addSelfVariable = false
        )
        val functionType = FunctionType(returnType, argTypes)
        symbolTable.functionSymbolTable[functionDefinition.functionName] =
            IdentifierData(functionType, false, OwnershipState.VALID, symbolTable.depth.value)
        symbolTable.functionSymbolTable.addFunction(functionDefinition)
        return functionDefinition.functionName to functionType
    }

    private fun generateMethod(returnType: Type, ctx: Context): Pair<StructType, FunctionDefinition> {
        val structType = generateStructType(ctx.incrementCount(MethodCallExpression::class))
        val numArgs = CustomRandom.nextInt(5)
        val argTypes = (0 until numArgs).map { generateType(ctx.incrementCount(FunctionType::class)) }
        val symbolTableForFunction = SymbolTable(
            symbolTable.root(), symbolTable.functionSymbolTable, symbolTable.globalSymbolTable
        )
        val arguments = argTypes.associateBy { identGenerator.generateVariable() }
        symbolTableForFunction["self"] =
            IdentifierData(ReferenceType(structType, symbolTable.depth.value.toUInt()), false, OwnershipState.VALID, 0)
        arguments.forEach {
            symbolTableForFunction[it.key] = IdentifierData(it.value, false, OwnershipState.VALID, 0)
        }
        val bodySymbolTable = symbolTableForFunction.enterScope()
        val functionName = identGenerator.generateFunctionName()
        val functionDefinition = FunctionDefinition(
            returnType, functionName,
            arguments + mapOf(
                "hasher" to MutableReferenceType(
                    DefaultHasher, symbolTable.depth.value.toUInt()
                )
            ),
            ASTGenerator(bodySymbolTable, failFast, identGenerator)(
                ctx.incrementCount(MethodCallExpression::class).resetContextForFunction()
                    .setReturnExpressionType(returnType).withSymbolTable(bodySymbolTable)
                    .withFunctionName(functionName),
                returnType
            ),
            CustomRandom.nextBoolean(), addSelfVariable = true
        )
        symbolTable.globalSymbolTable.addMethod(structType, functionDefinition)
        return structType to functionDefinition
    }

    override fun generateStructInstantiationExpression(type: Type, ctx: Context): StructInstantiationExpression {
        if (type !is StructType) {
            throw IllegalArgumentException("Type is not a struct type")
        } else {
            if (!failFast) {
                val membersWithReferenceType = type.memberTypes().filterIsInstance<ReferencingTypes>()
                membersWithReferenceType.forEach {
                    dependantStatements.add(
                        generateDependantDeclarationOfType(
                            it, ctx = ctx.incrementCount(StructInstantiationExpression::class)
                        )
                    )
                }
            }
            return StructInstantiationExpression(
                structName = type.structName,
                args = type.types.map {
                    it.first to generateExpression(it.second, ctx.incrementCount(StructInstantiationExpression::class))
                },
                symbolTable
            )
        }
    }

    override fun generateStaticSizedArrayLiteral(type: Type, ctx: Context): StaticSizedArrayLiteral {
        if (type is StaticSizedArrayType) {
            return StaticSizedArrayLiteral(
                (0 until type.size.toInt()).map {
                    generateExpression(
                        type.internalType, ctx.incrementCount(StaticSizedArrayLiteral::class)
                    )
                },
                symbolTable
            )
        }
        throw IllegalArgumentException("Invalid type $type")
    }

    override fun generateStaticSizedArrayDefaultLiteral(type: Type, ctx: Context): StaticSizedArrayDefaultLiteral {
        if (type is StaticSizedArrayType) {
            if (type.internalType.getOwnership() != OwnershipModel.COPY) throw ExpressionGenerationRejectedException()
            return StaticSizedArrayDefaultLiteral(
                type.size,
                generateExpression(type.internalType, ctx.incrementCount(StaticSizedArrayLiteral::class)),
                symbolTable
            )
        }
        throw IllegalArgumentException("Invalid type $type")
    }

    /** Type generators **/

    override fun selectRandomType(ctx: Context): KClass<out Type> {
        val pickRandomByWeight = selectionManager.availableTypesWeightings(ctx).pickRandomByWeight()
        Logger.logText("Picking type: $pickRandomByWeight", ctx, Color.GREEN)
        return pickRandomByWeight
    }

    override fun generateType(ctx: Context): Type {
        return generateSpecificType(selectRandomType(ctx), ctx)
    }

    override fun generateBoolType(ctx: Context) = BoolType

    override fun generateI8Type(ctx: Context) = I8Type

    override fun generateI16Type(ctx: Context) = I16Type

    override fun generateI32Type(ctx: Context) = I32Type

    override fun generateI64Type(ctx: Context) = I64Type

    override fun generateI128Type(ctx: Context) = I128Type

    override fun generateU8Type(ctx: Context) = U8Type

    override fun generateU16Type(ctx: Context) = U16Type

    override fun generateU32Type(ctx: Context) = U32Type

    override fun generateU64Type(ctx: Context) = U64Type

    override fun generateU128Type(ctx: Context) = U128Type
    override fun generateUSizeType(ctx: Context): USizeType = USizeType

    override fun generateF32Type(ctx: Context) = F32Type

    override fun generateF64Type(ctx: Context) = F64Type

    override fun generateStringType(ctx: Context) = StringType
    override fun generateVoidType(ctx: Context): VoidType = VoidType
    override fun generateReferenceType(ctx: Context): ReferenceType {
        val internalType = generateType(ctx.incrementCount(ReferenceType::class))
        return ReferenceType(internalType, symbolTable.depth.value.toUInt())
    }

    override fun generateMutableReferenceType(ctx: Context): MutableReferenceType {
        val internalType = generateType(ctx.incrementCount(MutableReferenceType::class))
        return MutableReferenceType(internalType, symbolTable.depth.value.toUInt())
    }

    override fun generateTupleType(ctx: Context): TupleType {
        val randomTupleType = symbolTable.globalSymbolTable.getRandomTuple(ctx)/* Create a new struct if the choice was made to, or if the choice was made not to but there are no structs
           currently available */
        if (randomTupleType == null || selectionManager.choiceGenerateNewTupleWeightings(ctx)
            .randomByWeights()
        ) { /* Generate Tuple Definition */
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
        val randomStructType = symbolTable.globalSymbolTable.getRandomStruct(ctx)/* Create a new struct if the choice was made to, or if the choice was made not to but there are no structs
           currently available */
        if (selectionManager.choiceGenerateNewStructWeightings(ctx).randomByWeights() || randomStructType == null) {
            return createNewStructType(ctx)
        } else {
            return randomStructType.second.type.clone() as StructType
        }
    }

    override fun generateVectorType(ctx: Context): VectorType {
        val randomVectorType = symbolTable.globalSymbolTable.getRandomVectorType()/* Create a new array type if the choice was made to, or if the choice was made not to but there are no structs
           currently available */
        if (randomVectorType == null || selectionManager.choiceGenerateNewVectorWeightings(ctx).randomByWeights()) {
            val internalType = generateType(ctx.incrementCount(VectorType::class))
            symbolTable.globalSymbolTable.addVectorType(internalType)
            return VectorType(internalType)
        }
        return VectorType(randomVectorType)
    }

    override fun generateOptionType(ctx: Context): OptionType {
        val randomVectorType = symbolTable.globalSymbolTable.getRandomOptionType()/* Create a new array type if the choice was made to, or if the choice was made not to but there are no structs
           currently available */
        if (randomVectorType == null || selectionManager.choiceGenerateNewOptionWeightings(ctx).randomByWeights()) {
            val internalType = generateType(ctx.incrementCount(OptionType::class))
            symbolTable.globalSymbolTable.addOptionType(internalType)
            return OptionType(internalType)
        }
        return OptionType(randomVectorType)
    }

    override fun generateTypeAliasType(ctx: Context): TypeAliasType {
        val typeAliasType = symbolTable.globalSymbolTable.getRandomTypeAlias()
        return if (typeAliasType == null || selectionManager.choiceGenerateNewTypeAliasWeightings(ctx)
            .randomByWeights()
        ) {
            val typeAlias = TypeAliasType(
                identGenerator.generateTypeAlias(), generateType(ctx.incrementCount(TypeAliasType::class))
            )
            symbolTable.globalSymbolTable.addTypeAlias(TypeAliasDefinition(wrapWithLifetimeParameters(typeAlias) as LifetimeParameterizedType<TypeAliasType>))
            typeAlias
        } else {
            typeAliasType
        }
    }

//    override fun generateStaticSizedArrayType(ctx: Context): StaticSizedArrayType {
//        val arraySize = CustomRandom.nextUInt(1u, 10u)
//        val randomVectorType = symbolTable.globalSymbolTable.getRandomArrayType()/* Create a new array type if the choice was made to, or if the choice was made not to but there are no structs
//           currently available */
//        if (randomVectorType == null || selectionManager.choiceGenerateNewArrayWeightings(ctx).randomByWeights()) {
//            val internalType = generateType(ctx.incrementCount(StaticSizedArrayType::class))
//            symbolTable.globalSymbolTable.addArrayType(internalType, arraySize)
//            return StaticSizedArrayType(internalType, arraySize)
//        }
//        return StaticSizedArrayType(randomVectorType, arraySize)
//    }

    override fun generateBoxType(ctx: Context): BoxType {
        val randomBoxType = symbolTable.globalSymbolTable.getRandomBoxType()
        if (randomBoxType == null || selectionManager.choiceGenerateNewTupleWeightings(ctx).randomByWeights()) {
            val internalType = generateType(ctx.incrementCount(BoxType::class))
            symbolTable.globalSymbolTable.addBoxType(internalType)
            return BoxType(internalType)
        }
        return BoxType(randomBoxType)
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
                is VectorType -> type.copy(type = wrapWithLifetimeParameters(type.type))
                is BoxType -> type.copy(internalType = wrapWithLifetimeParameters(type.internalType))
                is TypeAliasType -> LifetimeParameterizedType(type.copy(internalType = wrapWithLifetimeParameters(type.internalType)))
                is StaticSizedArrayType -> type.copy(internalType = wrapWithLifetimeParameters(type.internalType))
                is OptionType -> type.copy(type = wrapWithLifetimeParameters(type.type))
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
        val structName = identGenerator.generateStructName()/* Generate Struct Definition */
        val numArgs = CustomRandom.nextInt(1, 5)
        val argTypes = (0 until numArgs).map {
            identGenerator.generateVariable() to generateType(ctx.incrementCount(StructType::class))
        }.toMutableList()

        if (specificType != null) {
            argTypes += identGenerator.generateVariable() to specificType
        }

        val structType = StructType(structName, argTypes)
        symbolTable.globalSymbolTable[structName] =
            IdentifierData(structType, false, OwnershipState.VALID, symbolTable.depth.value)
        symbolTable.globalSymbolTable.addStruct(StructDefinition(wrapWithLifetimeParameters(structType.clone()) as LifetimeParameterizedType<StructType>))
        return structType
    }

    fun generateCLIArgumentsForLiteralType(type: LiteralType, ctx: Context): String {
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
            U128Type -> generateUInt128Literal(type, ctx).value.toString()
            U16Type -> generateUInt16Literal(type, ctx).value.toString()
            U32Type -> generateUInt32Literal(type, ctx).value.toString()
            U64Type -> generateUInt64Literal(type, ctx).value.toString()
            U8Type -> generateUInt8Literal(type, ctx).value.toString()
            USizeType -> generateUSizeLiteral(type, ctx).value.toString()
        }
    }

    fun generateLiteral(type: LiteralType, ctx: Context): LiteralExpression {
        return when (type) {
            BoolType -> generateBooleanLiteral(type, ctx)
            F32Type -> generateFloat32Literal(type, ctx)
            F64Type -> generateFloat64Literal(type, ctx)
            I128Type -> generateInt128Literal(type, ctx)
            I16Type -> generateInt16Literal(type, ctx)
            I32Type -> generateInt32Literal(type, ctx)
            I64Type -> generateInt64Literal(type, ctx)
            I8Type -> generateInt8Literal(type, ctx)
            StringType -> generateStringLiteral(type, ctx)
            U128Type -> generateUInt128Literal(type, ctx)
            U16Type -> generateUInt16Literal(type, ctx)
            U32Type -> generateUInt32Literal(type, ctx)
            U64Type -> generateUInt64Literal(type, ctx)
            U8Type -> generateUInt8Literal(type, ctx)
            USizeType -> generateUSizeLiteral(type, ctx)
        }
    }
}
