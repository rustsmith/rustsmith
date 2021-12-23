package com.rustsmith

import com.fasterxml.jackson.module.kotlin.jacksonObjectMapper
import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.options.default
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.types.int
import com.github.ajalt.clikt.parameters.types.long
import com.rustsmith.ast.Program
import com.rustsmith.ast.generateMain
import com.rustsmith.ast.varCount
import com.rustsmith.recondition.Reconditioner
import java.io.File
import kotlin.io.path.Path
import kotlin.random.Random as Random1

lateinit var Random: Random1

class RustSmith : CliktCommand() {
    private val count: Int by option(help = "Number of files to generate", names = arrayOf("-n", "-count")).int().default(1)
    private val seed: Long? by option(help = "Optional Seed", names = arrayOf("-s", "-seed")).long()
    private val directory: String by option(help = "Directory to save files").default("outRust")

    override fun run() {

        File(directory).deleteRecursively()
        File(directory).mkdirs()
        repeat(count) {
            val mapper = jacksonObjectMapper().writerWithDefaultPrettyPrinter()
            val randomSeed = seed ?: Random1.nextLong()
            Random = kotlin.random.Random(randomSeed)
            val program = Reconditioner.recondition(Program(seed = randomSeed, functions = listOf(generateMain())))
            println(program.toRust())
            val path = Path(directory, "file$it")
            path.toFile().mkdir()
            path.resolve("file$it.rs").toFile().writeText(program.toRust())
            path.resolve("file$it.json").toFile().writeText(mapper.writeValueAsString(program))
            varCount = 0
        }
    }
}

fun main(args: Array<String>) = RustSmith().main(args)
