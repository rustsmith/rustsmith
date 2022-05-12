package com.rustsmith.recondition

import com.rustsmith.ast.*

class Reconditioner {

    private val reconditioningMacros = mutableSetOf<Macros>()

    private fun reconditionBinOpExpression(node: BinOpExpression): Expression {
        val (expr1, expr2) = reconditionExpression(node.expr1) to reconditionExpression(node.expr2)
        val symbolTable = node.symbolTable
        return when (node) {
            is AddExpression -> if (node.toType() is IntType) WrappingAdd(
                node.copy(expr1 = expr1, expr2 = expr2),
                symbolTable
            ) else node.copy(expr1 = expr1, expr2 = expr2)
            is MultiplyExpression -> if (node.toType() is IntType) WrappingMul(
                node.copy(expr1 = expr1, expr2 = expr2),
                symbolTable
            ) else node.copy(expr1 = expr1, expr2 = expr2)
            is SubtractExpression -> if (node.toType() is IntType) WrappingSubtract(
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
        }
    }

    private fun reconditionExpression(node: Expression): Expression {
        return when (node) {
            is Float32Literal -> node
            is Int32Literal -> node
            is StringLiteral -> node
            is Variable -> node
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
        }
    }

    private fun reconditionStatementBlock(statementBlock: StatementBlock): StatementBlock {
        return statementBlock.copy(statements = statementBlock.statements.map { reconditionStatement(it) })
    }

    private fun reconditionStatement(node: Statement): Statement {
        return when (node) {
            is Assignment -> node.copy(value = reconditionExpression(node.value))
            is Declaration -> node.copy(value = reconditionExpression(node.value))
            is ExpressionStatement -> node.copy(expression = reconditionExpression(node.expression))
            is ReturnStatement -> node.copy(expression = reconditionExpression(node.expression))
            is BreakStatement -> node.copy()
            is Output -> node
            is FetchCLIArgs -> node
        }
    }

    fun recondition(node: ASTNode): ASTNode {
        return when (node) {
            is Program -> {
                val reconditionedFunctions = node.functions.map { it.copy(body = reconditionStatementBlock(it.body)) }
                Program(
                    node.seed,
                    reconditioningMacros,
                    node.structs,
                    reconditionedFunctions
                )
            }
            is Expression -> reconditionExpression(node)
            is Statement -> reconditionStatement(node)
            is FunctionDefinition -> node
            is Type -> node
            is StatementBlock -> node
            is StructDefinition -> node
        }
    }
}
