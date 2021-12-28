package com.rustsmith.recondition

import com.rustsmith.ast.*

object Reconditioner {

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
            is ModExpression -> node.copy(expr1 = reconditionExpression(node.expr1), expr2 = reconditionExpression(node.expr2))
//            is BitwiseAndLogicalAnd -> node.copy(expr1 = reconditionExpression(node.expr1), expr2 = reconditionExpression(node.expr2))
//            is BitwiseAndLogicalOr -> node.copy(expr1 = reconditionExpression(node.expr1), expr2 = reconditionExpression(node.expr2))
//            is BitwiseAndLogicalXor -> node.copy(expr1 = reconditionExpression(node.expr1), expr2 = reconditionExpression(node.expr2))
            is GroupedExpression -> node.copy(expression = reconditionExpression(node.expression))
            is AddExpression -> if (node.toType() is IntType) WrappingAdd(node.copy(expr1 = reconditionExpression(node.expr1), expr2 = reconditionExpression(node.expr2)), node.symbolTable) else node
            is DivideExpression -> ReconditionedDivision(node.copy(expr1 = reconditionExpression(node.expr1), expr2 = reconditionExpression(node.expr2)), node.symbolTable)
            is MultiplyExpression -> if (node.toType() is IntType) WrappingMul(node.copy(expr1 = reconditionExpression(node.expr1), expr2 = reconditionExpression(node.expr2)), node.symbolTable) else node
            is SubtractExpression -> if (node.toType() is IntType) WrappingSubtract(node.copy(expr1 = reconditionExpression(node.expr1), expr2 = reconditionExpression(node.expr2)), node.symbolTable) else node
            is ReconditionedExpression -> node
        }
    }

    private fun reconditionStatement(node: Statement): Statement {
        return when (node) {
            is Assignment -> node.copy(value = reconditionExpression(node.value))
            is ChainedStatement -> node.copy(s1 = reconditionStatement(node.s1), s2 = reconditionStatement(node.s2))
            is Declaration -> node.copy(value = reconditionExpression(node.value))
            is Output -> node
            is Expression -> reconditionExpression(node)
            is ExpressionStatement -> node.copy(expression = reconditionExpression(node.expression))
        }
    }

    fun recondition(node: ASTNode): ASTNode {
        return when (node) {
            is Program -> Program(node.seed, node.structs, node.functions.map { it.copy(body = reconditionStatement(it.body)) })
            is Expression -> reconditionExpression(node)
            is Statement -> reconditionStatement(node)
            is FunctionDefinition -> node
            is Type -> node
        }
    }
}
