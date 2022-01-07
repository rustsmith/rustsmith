package com.rustsmith.recondition

import com.rustsmith.ast.*

object Reconditioner {

    private fun reconditionBinOpExpression(node: BinOpExpression): Expression {
        val (expr1, expr2) = reconditionExpression(node.expr1) to reconditionExpression(node.expr2)
        val symbolTable = node.symbolTable
        return when (node) {
            is AddExpression -> if (node.toType() is IntType) WrappingAdd(node.copy(expr1 = expr1, expr2 = expr2), symbolTable) else node.copy(expr1 = expr1, expr2 = expr2)
            is MultiplyExpression -> if (node.toType() is IntType) WrappingMul(node.copy(expr1 = expr1, expr2 = expr2), symbolTable) else node.copy(expr1 = expr1, expr2 = expr2)
            is SubtractExpression -> if (node.toType() is IntType) WrappingSubtract(node.copy(expr1 = expr1, expr2 = expr2), symbolTable) else node.copy(expr1 = expr1, expr2 = expr2)
            is DivideExpression -> ReconditionedDivision(node.copy(expr1 = expr1, expr2 = expr2), symbolTable)
            is ModExpression -> ReconditionedMod(node.copy(expr1 = expr1, expr2 = expr2), symbolTable)
            is BitwiseAndLogicalAnd -> node.copy(expr1 = expr1, expr2 = expr2)
            is BitwiseAndLogicalOr -> node.copy(expr1 = expr1, expr2 = expr2)
            is BitwiseAndLogicalXor -> node.copy(expr1 = expr1, expr2 = expr2)
        }
    }

    private fun reconditionExpression(node: Expression): Expression {
        return when (node) {
            is Float32Literal -> node
            is Int32Literal -> node
//            is StringLiteral -> node
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
            is BlockExpression -> node.copy(statement = reconditionStatement(node.statement))
            is IfElseExpression -> node.copy(
                predicate = reconditionExpression(node.predicate),
                ifBlock = reconditionExpression(node.ifBlock) as BlockExpression,
                elseBlock = reconditionExpression(node.elseBlock) as BlockExpression
            )
            is FunctionCallExpression -> node.copy(args = node.args.map { reconditionExpression(it) })
        }
    }

    private fun reconditionStatement(node: Statement): Statement {
        return when (node) {
            is Assignment -> node.copy(value = reconditionExpression(node.value))
            is ChainedStatement -> node.copy(s1 = reconditionStatement(node.s1), s2 = reconditionStatement(node.s2))
            is Declaration -> node.copy(value = reconditionExpression(node.value))
            is Output -> node
            is ExpressionAndStatement -> reconditionExpression(node) as ExpressionAndStatement
            is ExpressionStatement -> node.copy(expression = reconditionExpression(node.expression))
        }
    }

    fun recondition(node: ASTNode): ASTNode {
        return when (node) {
            is Program -> Program(
                node.seed,
                node.structs,
                node.functions.map { it.copy(body = reconditionStatement(it.body)) }
            )
            is Expression -> reconditionExpression(node)
            is Statement -> reconditionStatement(node)
            is FunctionDefinition -> node
            is Type -> node
        }
    }
}
