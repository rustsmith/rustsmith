package com.rustsmith.logging

import com.rustsmith.generation.Context

val DEBUG = false

object Logger {

    fun logText(text: String, ctx: Context) {
        if (DEBUG) {
            val spaceLeftSize: Int = ctx.nodeDepthState.sumOf { it.values.sum() }
            val logText = "${IntRange(0, spaceLeftSize).joinToString("") { " " }}$text"
            println("RUSTSMITH:$logText")
        }
    }
}
