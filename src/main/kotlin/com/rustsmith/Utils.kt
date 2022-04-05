package com.rustsmith

import kotlin.reflect.KClass

fun <T> pickRandomByWeight(weightingsMap: List<Pair<T, Double>>): T {
    val sum = weightingsMap.sumOf { it.second }
    val normalisedWeightingsMap = weightingsMap.map { it.first to it.second / sum }
    var acc = 0.0
    val cumulativeProbabilityWeights =
        normalisedWeightingsMap.map { acc += it.second; it.first to acc }
    val selectedProbability = Random.nextDouble(0.0, 1.0)
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