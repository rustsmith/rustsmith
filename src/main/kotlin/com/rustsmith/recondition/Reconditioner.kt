package com.rustsmith.recondition

import com.rustsmith.ast.*
import kotlin.reflect.KClass

class Reconditioner {
    private val reconditioningMacros = mutableSetOf<Macros>()
    val nodeCounters: MutableMap<KClass<out ASTNode>, Int> = mutableMapOf<KClass<out ASTNode>, Int>().withDefault { 0 }
    val variableUsageCounter: MutableMap<String, Int> = mutableMapOf()

    init {
//        Statement::class.subclasses().map {
//            nodeCounters[it] = 0
//        }
//        Expression::class.subclasses().filter { it.hasAnnotation<ExpressionGenNode>() }.map {
//            nodeCounters[it] = 0
//        }
    }

    private fun reconditionBinOpExpression(node: BinOpExpression): Expression {
        val (expr1, expr2) = reconditionExpression(node.expr1) to reconditionExpression(node.expr2)
        val symbolTable = node.symbolTable
        return when (node) {
            is AddExpression -> if (node.toType() is IntType || node.toType() is UIntType) WrappingAdd(
                node.copy(expr1 = expr1, expr2 = expr2),
                symbolTable
            ) else node.copy(expr1 = expr1, expr2 = expr2)
            is MultiplyExpression -> if (node.toType() is IntType || node.toType() is UIntType) WrappingMul(
                node.copy(expr1 = expr1, expr2 = expr2),
                symbolTable
            ) else node.copy(expr1 = expr1, expr2 = expr2)
            is SubtractExpression -> if (node.toType() is IntType || node.toType() is UIntType) WrappingSubtract(
                node.copy(expr1 = expr1, expr2 = expr2),
                symbolTable
            ) else node.copy(expr1 = expr1, expr2 = expr2)
            is DivideExpression -> {
                reconditioningMacros.add(ReconditionedDivision)
                ReconditionedDivisionExpression(
                    node.copy(expr1 = expr1, expr2 = expr2),
                    symbolTable
                )
            }
            is ModExpression -> {
                reconditioningMacros.add(ReconditionedMod)
                ReconditionedModExpression(
                    node.copy(expr1 = expr1, expr2 = expr2),
                    symbolTable
                )
            }
            is BitwiseAndLogicalAnd -> node.copy(expr1 = expr1, expr2 = expr2)
            is BitwiseAndLogicalOr -> node.copy(expr1 = expr1, expr2 = expr2)
            is BitwiseAndLogicalXor -> node.copy(expr1 = expr1, expr2 = expr2)
            is EqExpression -> node.copy(expr1 = expr1, expr2 = expr2)
            is NEqExpression -> node.copy(expr1 = expr1, expr2 = expr2)
            is GTEExpression -> node.copy(expr1 = expr1, expr2 = expr2)
            is GTExpression -> node.copy(expr1 = expr1, expr2 = expr2)
            is LTEExpression -> node.copy(expr1 = expr1, expr2 = expr2)
            is LTExpression -> node.copy(expr1 = expr1, expr2 = expr2)
        }
    }

    private fun reconditionExpression(node: Expression): Expression {
        nodeCounters[node::class] = nodeCounters.getValue(node::class) + 1
        return when (node) {
            is Int32Literal -> node
            is StringLiteral -> node
            is Variable -> {
                if (node.toType().getOwnership() == OwnershipModel.COPY) {
                    variableUsageCounter[node.value] = (variableUsageCounter[node.value] ?: 0) + 1
                }
                node
            }
            is BooleanLiteral -> node
            is Float64Literal -> node
            is Int128Literal -> node
            is Int16Literal -> node
            is Int64Literal -> node
            is Int8Literal -> node
            is BinOpExpression -> reconditionBinOpExpression(node)
            is GroupedExpression -> node.copy(expression = reconditionExpression(node.expression))
            is ReconditionedExpression -> node
            is BlockExpression -> node.copy(statement = reconditionStatementBlock(node.statement))
            is IfElseExpression -> node.copy(
                predicate = reconditionExpression(node.predicate),
                ifBlock = reconditionStatementBlock(node.ifBlock),
                elseBlock = reconditionStatementBlock(node.elseBlock)
            )
            is FunctionCallExpression -> node.copy(args = node.args.map { reconditionExpression(it) })
            is TupleLiteral -> node.copy(values = node.values.map { reconditionExpression(it) })
            is StructInstantiationExpression -> node.copy(args = node.args.map { it.first to reconditionExpression(it.second) })
            is TupleElementAccessExpression -> node.copy(expression = reconditionExpression(node.expression))
            is StructElementAccessExpression -> node.copy(expression = reconditionExpression(node.expression))
            is LoopExpression -> node.copy(body = reconditionStatementBlock(node.body))
            is VoidLiteral -> node.copy()
            is IfExpression -> node.copy(
                predicate = reconditionExpression(node.predicate),
                ifBlock = reconditionStatementBlock(node.ifBlock)
            )
            is CLIArgumentAccessExpression -> node
            is ReferenceExpression -> node.copy(expression = reconditionExpression(node.expression))
            is MutableReferenceExpression -> node.copy(expression = reconditionExpression(node.expression))
            is DereferenceExpression -> node.copy(expression = reconditionExpression(node.expression))
            is Float32Literal -> node
            is UInt128Literal -> node
            is UInt16Literal -> node
            is UInt32Literal -> node
            is UInt64Literal -> node
            is UInt8Literal -> node
            is VectorLiteral -> node.copy(expressions = node.expressions.map { reconditionExpression(it) })
            is VectorAccess -> {
                reconditioningMacros.add(ReconditionedArrayAccess)
                ReconditionedIndexAccess(
                    node.copy(
                        vectorExpression = reconditionExpression(node.vectorExpression),
                        indexExpression = reconditionExpression(node.indexExpression)
                    ),
                    node.symbolTable
                )
            }
            is USizeLiteral -> node
            is VectorLengthExpression -> {
                node.copy(vectorExpression = reconditionExpression(node.vectorExpression))
            }
            is VectorPushExpression -> node.copy(
                vectorExpression = reconditionExpression(node.vectorExpression),
                pushExpression = reconditionExpression(node.pushExpression)
            )
            is NewBoxExpression -> node.copy(internalExpression = reconditionExpression(node.internalExpression))
            is BoxDereferenceExpression -> node.copy(internalExpression = reconditionExpression(node.internalExpression))
            is MethodCallExpression -> node.copy(
                structExpression = reconditionExpression(node.structExpression),
                args = node.args.map { reconditionExpression(it) }
            )
            is TypeAliasExpression -> node.copy(internalExpression = reconditionExpression(node.internalExpression))
            is StaticSizedArrayDefaultLiteral -> node.copy(expression = reconditionExpression(node.expression))
            is StaticSizedArrayLiteral -> node.copy(expressions = node.expressions.map { reconditionExpression(it) })
            is ElementAccess -> node.copy(expression = reconditionExpression(node.expression))
            is NoneLiteral -> node
            is SomeLiteral -> node.copy(internalExpression = reconditionExpression(node.internalExpression))
            is ExtractOptionExpression -> node.copy(
                internalExpression = reconditionExpression(node.internalExpression),
                matchedStatement = reconditionStatementBlock(node.matchedStatement),
                noneStatement = reconditionStatementBlock(node.noneStatement),
            )
        }
    }

    private fun reconditionStatementBlock(statementBlock: StatementBlock): StatementBlock {
        return statementBlock.copy(statements = statementBlock.statements.map { reconditionStatement(it) })
    }

    private fun reconditionStatement(node: Statement): Statement {
        nodeCounters[node::class] = nodeCounters.getValue(node::class) + 1
        return when (node) {
            is Assignment -> node.copy(value = reconditionExpression(node.value))
            is Declaration -> node.copy(value = reconditionExpression(node.value))
            is ExpressionStatement -> node.copy(expression = reconditionExpression(node.expression))
            is ReturnStatement -> node.copy(expression = reconditionExpression(node.expression))
            is BreakStatement -> node.copy()
            is Output -> node
            is FetchCLIArgs -> node
            is PrintElementStatement -> node
            is ConstDeclaration -> node
        }
    }

    fun recondition(node: ASTNode): ASTNode {
        return when (node) {
            is Program -> {
                nodeCounters[FunctionDefinition::class] = node.functions.size
                nodeCounters[StructDefinition::class] = node.structs.size
                val reconditionedFunctions = node.functions.map { it.copy(body = reconditionStatementBlock(it.body)) }
                val reconditionedStructs = node.structs.map {
                    it.copy(
                        methods = it.methods.map { method ->
                            method.copy(
                                body = reconditionStatementBlock(method.body)
                            )
                        }.toMutableList()
                    )
                }
                Program(
                    node.seed,
                    reconditioningMacros,
                    node.constants,
                    node.aliases,
                    reconditionedStructs,
                    reconditionedFunctions
                )
            }
            is Expression -> reconditionExpression(node)
            is Statement -> reconditionStatement(node)
            is FunctionDefinition -> node
            is Type -> node
            is StatementBlock -> node
            is StructDefinition -> node.copy(
                methods = node.methods.map { it.copy(body = reconditionStatementBlock(it.body)) }
                    .toMutableList()
            )
            is TypeAliasDefinition -> node
        }
    }
}
