package com.rustsmith

import com.rustsmith.ast.Expression
import com.rustsmith.ast.ExpressionGenNode
import com.rustsmith.ast.SwarmNode
import kotlin.reflect.KClass
import kotlin.reflect.full.hasAnnotation

fun getRandomConfiguration(): List<KClass<out Expression>> {
    val expressionConfigurableNodes = Expression::class.subclasses()
        .filter { it.hasAnnotation<ExpressionGenNode>() && it.hasAnnotation<SwarmNode>() }
    val numberNodesToTurnOff = (0..5).random(CustomRandom)
    return expressionConfigurableNodes.shuffled(CustomRandom).take(numberNodesToTurnOff)
}

fun <T> pickRandomByWeight(weightingsMap: List<Pair<T, Double>>): T {
    val sum = weightingsMap.sumOf { it.second }
    val normalisedWeightingsMap = weightingsMap.map { it.first to it.second / sum }.shuffled(CustomRandom)
    var acc = 0.0
    val cumulativeProbabilityWeights =
        normalisedWeightingsMap.map { acc += it.second; it.first to acc }
    val selectedProbability = CustomRandom.nextDouble(0.0, 1.0)
    return cumulativeProbabilityWeights.first { it.second > selectedProbability }.first
}

fun <T> Map<T, Double>.randomByWeights(): T {
    return pickRandomByWeight(this.toList())
}

fun <T : Any> KClass<T>.subclasses(): Set<KClass<out T>> {
    if (this.isFinal) {
        return setOf(this)
    }
    val nonFinalClasses = this.sealedSubclasses.filter { !it.isFinal }
    val result = mutableSetOf<KClass<out T>>()
    for (nonFinalClass in nonFinalClasses) {
        result.addAll(nonFinalClass.subclasses())
    }
    return this.sealedSubclasses.filter { it.isFinal }.toSet() + result
}
