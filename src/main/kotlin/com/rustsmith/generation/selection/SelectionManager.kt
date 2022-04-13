package com.rustsmith.generation.selection

import com.rustsmith.ast.ASTNode
import com.rustsmith.ast.Expression
import com.rustsmith.ast.Statement
import com.rustsmith.ast.Type
import com.rustsmith.generation.Context
import kotlin.reflect.KClass

interface SelectionManager {
    val config: Map<KClass<out ASTNode>, Int>

    fun choiceGenerateNewStatementWeightings(ctx: Context): Map<Boolean, Double>

    fun choiceGenerateNewStructWeightings(ctx: Context): Map<Boolean, Double>

    fun choiceGenerateNewTupleWeightings(ctx: Context): Map<Boolean, Double>

    fun choiceGenerateNewFunctionWeightings(ctx: Context): Map<Boolean, Double>

    fun availableStatementsWeightings(ctx: Context): NodeSelectionWeighting<Statement>

    fun availableExpressionsWeightings(ctx: Context, type: Type): NodeSelectionWeighting<Expression>

    fun availableTypesWeightings(ctx: Context): NodeSelectionWeighting<Type>
}
