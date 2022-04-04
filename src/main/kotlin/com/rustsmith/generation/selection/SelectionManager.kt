package com.rustsmith.generation.selection

import com.rustsmith.Random
import com.rustsmith.ast.ASTNode
import com.rustsmith.ast.Expression
import com.rustsmith.ast.Statement
import com.rustsmith.ast.Type
import com.rustsmith.generation.Context
import kotlin.reflect.KClass

typealias SelectionWeighting<T> = Map<T, Double>

fun <T> pickRandomByWeight(weightingsMap: List<Pair<T, Double>>): T {
    val sum = weightingsMap.sumOf { it.second }
    val normalisedWeightingsMap = weightingsMap.map { it.first to it.second / sum }
    var acc = 0.0
    val cumulativeProbabilityWeights =
        normalisedWeightingsMap.map { acc += it.second; it.first to acc }
    val selectedProbability = Random.nextDouble(0.0, 1.0)
    return cumulativeProbabilityWeights.first { it.second > selectedProbability }.first
}

fun <T> SelectionWeighting<T>.randomByWeights(): T {
    return pickRandomByWeight(this.toList())
}

interface SelectionManager {

    val config: Map<KClass<out ASTNode>, Int>

    fun createNewStatementWeightings(ctx: Context): Map<Boolean, Double>

    fun availableStatementsWeightings(ctx: Context): SelectionWeighting<KClass<out Statement>>

    fun availableExpressionsWeightings(ctx: Context, type: Type):  SelectionWeighting<KClass<out Expression>>

    fun availableTypesWeightings(ctx: Context): Map<KClass<out Type>, Double>
}