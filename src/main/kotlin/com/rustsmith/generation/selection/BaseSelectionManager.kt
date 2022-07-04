package com.rustsmith.generation.selection

import com.rustsmith.ast.*
import com.rustsmith.exceptions.NoAvailableExpressionException
import com.rustsmith.exceptions.NoAvailableStatementException
import com.rustsmith.exceptions.NoAvailableTypeException
import com.rustsmith.exceptions.StatementGenerationRejectedException
import com.rustsmith.generation.Context
import com.rustsmith.subclasses
import kotlin.reflect.KClass
import kotlin.reflect.full.findAnnotation
import kotlin.reflect.full.hasAnnotation
import kotlin.reflect.full.isSubclassOf

/**
 * Base Selection Manager where each option is associated with an equal weighting to choose from
 *
 */
open class BaseSelectionManager : SelectionManager {

    // Config describes what limit each specific ASTNode should have in terms of depth
    override val config: MutableMap<KClass<out ASTNode>, Int> = mutableMapOf(
        FunctionCallExpression::class to 3,
        MethodCallExpression::class to 3,
        TupleType::class to 3,
        RecursiveExpression::class to 3,
        ContainerType::class to 3,
        ArrayLiteral::class to 3,
        ArrayLengthExpression::class to 3
    ).withDefault { Int.MAX_VALUE }

    /* Block size for statement blocks */
    override fun choiceGenerateNewStatementWeightings(ctx: Context) = mapOf(true to 0.9, false to 0.1)

    /* Both false as default. Eg: never create a new struct or function if a struct already exists or a function
       with the return type required already exists
     */
    override fun choiceGenerateNewStructWeightings(ctx: Context): Map<Boolean, Double> = mapOf(false to 1.0)

    override fun choiceGenerateNewTupleWeightings(ctx: Context): Map<Boolean, Double> = mapOf(false to 1.0)

    override fun choiceGenerateNewBoxTypeWeightings(ctx: Context) = mapOf(true to 0.5, false to 0.5)

    override fun choiceGenerateNewArrayTypeWeightings(ctx: Context) = mapOf(true to 0.1, false to 0.9)

    override fun choiceGenerateNewFunctionWeightings(ctx: Context): Map<Boolean, Double> =
        mapOf(false to 0.5, true to 0.5)

    override fun choiceGenerateNewCLIArgumentWeightings(ctx: Context): Map<Boolean, Double> = mapOf(false to 1.0)

    override fun availableStatementsWeightings(ctx: Context): NodeSelectionWeighting<Statement> {
        val allStatements = Statement::class.subclasses().filter { it.hasAnnotation<GenNode>() }
        val filteredStatements = filterNodes(allStatements.toMutableList(), ctx).associateWith { 1.0 }.toMutableMap()
        if (ctx.returnExpressionType == null) {
            filteredStatements.remove(ReturnStatement::class)
        }

        if (ctx.returnLoopType == null) {
            filteredStatements.remove(BreakStatement::class)
        }

        ctx.failedGenerationNodes.forEach {
            filteredStatements.remove(it)
        }
        if (filteredStatements.isEmpty()) {
            throw NoAvailableStatementException()
        }
        return NodeSelectionWeighting(filteredStatements)
    }

    override fun availableExpressionsWeightings(ctx: Context, type: Type): NodeSelectionWeighting<Expression> {
        val allExpressions =
            Expression::class.subclasses().filter { it.hasAnnotation<ExpressionGenNode>() }.filter {
                it.findAnnotation<ExpressionGenNode>()?.compatibleType?.genSubClasses()
                    ?.contains(type::class) ?: false
            }
        val filteredExpressions: MutableMap<KClass<out Expression>, Double> =
            filterNodes(allExpressions.toMutableList(), ctx).associateWith { 1.0 }.toMutableMap()
        if (ctx.getDepthLast(ReferencingExpressions::class) > 0) {
            filteredExpressions.clear()
            filteredExpressions[Variable::class] = 1.0
            if (ctx.nodeDepthState.last().values.sum() < 4) {
                filteredExpressions[TupleElementAccessExpression::class] = 1.0
                filteredExpressions[StructElementAccessExpression::class] = 1.0
                filteredExpressions[DereferenceExpression::class] = 1.0
            }
        }
        if (ctx.currentFunctionName != "main") {
            filteredExpressions.remove(CLIArgumentAccessExpression::class)
        }
        if (type.getOwnership() == OwnershipModel.MOVE) {
            filteredExpressions.remove(ArrayAccess::class)
            filteredExpressions.remove(DereferenceExpression::class)
        }
        if (ctx.previousIncrement == ExpressionStatement::class) {
            filteredExpressions.remove(ArrayAccess::class)
        }
        // To stop explicit && cases
        if (ctx.previousIncrement in ReferencingExpressions::class.subclasses()) {
            filteredExpressions.remove(ReferenceExpression::class)
            filteredExpressions.remove(MutableReferenceExpression::class)
        }
        if (type.memberTypes().count { it is ReferencingTypes } > 0) {
            filteredExpressions.remove(FunctionCallExpression::class)
            filteredExpressions.remove(MethodCallExpression::class)
        }

        val variableRequiringExpressions = listOf(StructInstantiationExpression::class, TupleLiteral::class, FunctionCallExpression::class, MethodCallExpression::class)
        if (ctx.previousIncrement in variableRequiringExpressions && type is ReferencingTypes) {
            // Ensure the variables created beforehand are used
            filteredExpressions.clear()
            filteredExpressions[Variable::class] = 1.0
        }

        if (ctx.getDepthLast(ArrayAccess::class) > 0) {
            filteredExpressions.clear()
            filteredExpressions[Variable::class] = 1.0
//            RecursiveStatementBlockExpression::class.subclasses().forEach { filteredExpressions.remove(it) }
        }

        val weightings = filterNodes(filteredExpressions.keys.toMutableList(), ctx).associateWith { 1.0 }.toMutableMap()
        ctx.failedGenerationNodes.forEach {
            weightings.remove(it)
        }
        if (weightings.isEmpty()) {
            throw NoAvailableExpressionException()
        }

        if (ctx.getDepth(Expression::class) > 30) {
            throw StatementGenerationRejectedException()
        }
        return NodeSelectionWeighting(weightings)
    }

    override fun availableTypesWeightings(ctx: Context): NodeSelectionWeighting<Type> {
        val allTypes =
            filterNodes(Type::class.genSubClasses().toMutableList(), ctx).associateWith { 1.0 }.toMutableMap()
        if (ctx.getDepth(MutableReferenceType::class) > 0) {
            allTypes.remove(ReferenceType::class)
            allTypes.remove(MutableReferenceType::class)
        }

        if (ctx.previousIncrement != ExpressionStatement::class) {
            allTypes.remove(VoidType::class)
        }

        ctx.failedGenerationNodes.forEach {
            allTypes.remove(it)
        }
        if (allTypes.isEmpty()) {
            throw NoAvailableTypeException()
        }
        return NodeSelectionWeighting(allTypes)
    }

    private fun <T : ASTNode> filterNodes(
        nodes: MutableList<KClass<out T>>,
        ctx: Context
    ): List<KClass<out T>> {
        config.filterKeys { !it.isFinal }.forEach { entry ->
            val totalForSubclasses = entry.key.subclasses().sumOf { ctx.getDepth(it) }
            if (totalForSubclasses >= entry.value) {
                val elements = nodes.filter { it.isSubclassOf(entry.key) }.toSet()
                nodes.removeAll(elements)
            }
        }
        nodes.removeIf { ctx.getDepth(it) >= config.getValue(it) }
        return nodes
    }
}
