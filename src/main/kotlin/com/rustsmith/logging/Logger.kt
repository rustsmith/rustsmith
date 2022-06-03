package com.rustsmith.logging

import com.andreapivetta.kolor.*
import com.rustsmith.generation.Context
import java.util.Date

const val DEBUG = false

object Logger {

    fun logText(text: String, ctx: Context, color: Color = Color.YELLOW) {
        if (DEBUG) {
            val spaceLeftSize: Int = ctx.nodeDepthState.sumOf { it.values.sum() }
            val logText = Kolor.foreground("${IntRange(0, spaceLeftSize).joinToString("") { "\t" }}$text", color)
            println("[RUSTSMITH: ${Date().toLocaleString()}]:$logText".lightBlue())
        }
    }
}
