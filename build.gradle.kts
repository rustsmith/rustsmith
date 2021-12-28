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
    implementation("com.github.ajalt.clikt:clikt:3.3.0")
    implementation("com.fasterxml.jackson.module:jackson-module-kotlin:2.13.1")
}

tasks.jar {
    duplicatesStrategy = DuplicatesStrategy.EXCLUDE
    manifest {
        attributes("Main-Class" to "com.rustsmith.MainKt")
    }
    from(configurations.compileClasspath.get().map { if (it.isDirectory) it else zipTree(it) })
    exclude("**/module-info.class")
}

//def shadowJarExecutableTask = tasks.register("shadowJarExecutable", DefaultTask.class) {
//    description = "Creates self-executable file, that runs generated shadow jar"
//    group = "Distribution"
//
//    inputs.files tasks.named("shadowJar")
//    outputs.files("$buildDir/run/ktlint")
//    if (!version.toString().endsWith("SNAPSHOT")) {
//        outputs.files("$buildDir/run/ktlint.asc")
//    }
//
//    doLast {
//        File execFile = outputs.files.getFiles().first()
//        execFile.withOutputStream {
//            it.write "#!/bin/sh\n\nexec java -Xmx512m -jar \"\$0\" \"\$@\"\n\n".bytes
//            it.write inputs.files.singleFile.bytes
//        }
//        execFile.setExecutable(true, false)
//        if (!version.toString().endsWith("SNAPSHOT")) {
//            signing.sign(execFile)
//        }
//    }
//    finalizedBy tasks.named("shadowJarExecutableChecksum")
//}

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

application {
    applicationName = "rustsmith"
    mainClass.set("com.rustsmith.MainKt")
}


kotlinter{
    disabledRules = arrayOf("no-wildcard-imports")
}