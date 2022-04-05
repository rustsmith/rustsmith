package com.rustsmith.generation.selection

import com.rustsmith.ast.ASTNode
import com.rustsmith.randomByWeights
import com.rustsmith.subclasses
import kotlin.reflect.KClass

class NodeSelectionWeighting<T : ASTNode>(private val weightings: MutableMap<KClass<out T>, Double>) {

    fun updateWeighting(kClass: KClass<out T>, weighting: Double) {
        kClass.subclasses().forEach {
            if (weightings.containsKey(it)) {
                weightings[it] = weighting
            }
        }
    }

    fun pickRandomByWeight() = weightings.randomByWeights()
}
