import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
    id("org.jetbrains.kotlin.jvm") version "1.6.0"
    id("org.jmailen.kotlinter") version "3.6.0"
    id("com.github.johnrengelman.shadow") version "7.1.1"
    application
}

group = "org.example"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

tasks.withType<KotlinCompile> {
    kotlinOptions.jvmTarget = "15"
}
dependencies {
    implementation("org.jetbrains.kotlin:kotlin-stdlib")
    implementation("org.jetbrains.kotlin:kotlin-reflect:1.6.0")
    implementation("com.github.ajalt.clikt:clikt:3.4.0")
    implementation("com.fasterxml.jackson.module:jackson-module-kotlin:2.13.2")
    implementation("com.squareup:kotlinpoet:1.11.0")
    implementation("me.tongfei:progressbar:0.9.3")
}

tasks.jar {
    duplicatesStrategy = DuplicatesStrategy.EXCLUDE
    manifest {
        attributes("Main-Class" to "com.rustsmith.MainKt")
    }
    from(configurations.compileClasspath.get().map { if (it.isDirectory) it else zipTree(it) })
    exclude("**/module-info.class")
}

tasks.shadowJar {
    mergeServiceFiles()
}

tasks.register<DefaultTask>("shadowJarExecutable") {
    description = "Creates self-executable file, that runs generated shadow jar"
    group = "Distribution"

    inputs.files(tasks.named("shadowJar"))
    outputs.files("run/rustsmith")

    doLast {
        val execFile = outputs.files.files.first()
        execFile.outputStream().use {
            it.write("#!/bin/sh\n\nexec java -Xmx512m -jar \"\$0\" \"\$@\"\n\n".toByteArray())
            it.write(inputs.files.singleFile.readBytes())
        }
        execFile.setExecutable(true, false)
    }
}

tasks.register<JavaExec>("generateInterface") {
    group = "Tools"
    mainClass.set("com.tools.InterfaceGeneratorKt")
    classpath = sourceSets["main"].runtimeClasspath
}

application {
    applicationName = "rustsmith"
    mainClass.set("com.rustsmith.MainKt")
}


kotlinter{
    disabledRules = arrayOf("no-wildcard-imports")
}