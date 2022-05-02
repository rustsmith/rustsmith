package com.rustsmith

import com.fasterxml.jackson.module.kotlin.jacksonObjectMapper
import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.options.default
import com.github.ajalt.clikt.parameters.options.flag
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.types.int
import com.github.ajalt.clikt.parameters.types.long
import com.rustsmith.ast.*
import com.rustsmith.generation.IdentGenerator
import com.rustsmith.generation.selection.OptimalSelectionManager
import com.rustsmith.generation.selection.SelectionManager
import com.rustsmith.recondition.Reconditioner
import me.tongfei.progressbar.ProgressBarBuilder
import me.tongfei.progressbar.ProgressBarStyle
import java.io.File
import kotlin.io.path.Path
import kotlin.random.Random

lateinit var CustomRandom: Random
lateinit var selectionManager: SelectionManager

class RustSmith : CliktCommand(name = "rustsmith") {
    private val count: Int by option(help = "No. of files to generate", names = arrayOf("-n", "-count")).int().default(1)
    private val print: Boolean by option("-p", "-print", help = "Print out program only").flag(default = false)
    private val seed: Long? by option(help = "Optional Seed", names = arrayOf("-s", "-seed")).long()
    private val directory: String by option(help = "Directory to save files").default("outRust")

    override fun run() {
        val currentConfig = getRandomConfiguration()
        selectionManager = OptimalSelectionManager(currentConfig)
        if (!print) {
            File(directory).deleteRecursively()
            File(directory).mkdirs()
        }
        val mapper = jacksonObjectMapper().writerWithDefaultPrettyPrinter()

        // Don't make progress bar if printing out the program in console
        val progressBar = if (!print) ProgressBarBuilder().setTaskName("Generating").setInitialMax(count.toLong())
            .setStyle(ProgressBarStyle.ASCII).setUpdateIntervalMillis(10).build() else null
        var i = 0
        while (i < count) {
            val randomSeed = seed ?: Random.nextLong()
            CustomRandom = Random(randomSeed)
            val reconditioner = Reconditioner()
            val program = reconditioner.recondition(generateProgram(randomSeed))
            if (program.toRust().count { char -> char == '\n' } > 7000) {
                continue
            }
            if (print) {
                println(program.toRust())
                return
            }
            val path = Path(directory, "file$i")
            path.toFile().mkdir()
            path.resolve("file$i.rs").toFile().writeText(program.toRust())
            path.resolve("file$i.json").toFile().writeText("{}")
            IdentGenerator.reset()
            progressBar?.step()
            i++
        }
        progressBar?.close()
    }
}

fun main(args: Array<String>) = RustSmith().main(args)
