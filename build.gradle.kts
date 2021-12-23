import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
    id("org.jetbrains.kotlin.jvm") version "1.6.0"
    id("org.jmailen.kotlinter") version "3.6.0"
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

application {
    applicationName = "rustsmith"
    mainClass.set("com.rustsmith.MainKt")
}


kotlinter{
    disabledRules = arrayOf("no-wildcard-imports")
}