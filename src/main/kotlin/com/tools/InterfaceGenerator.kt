package com.tools

import com.rustsmith.ast.*
import com.rustsmith.generation.Context
import com.squareup.kotlinpoet.*
import com.squareup.kotlinpoet.ParameterizedTypeName.Companion.parameterizedBy
import java.io.File
import kotlin.reflect.full.hasAnnotation

val kClassType = ClassName("kotlin.reflect", "KClass")

class InterfaceGenerator {
    private val generatorInterface = TypeSpec.interfaceBuilder("AbstractASTGenerator")

    private fun generateStatementFunctions() {
        val randomStatementFunction = FunSpec.builder("selectRandomStatement")
            .addParameter(ParameterSpec.builder("ctx", Context::class).build())
            .returns(kClassType.parameterizedBy(WildcardTypeName.producerOf(Statement::class.asClassName())))
            .addModifiers(KModifier.ABSTRACT)
            .build()
        val codeBlock = CodeBlock.builder()
            .beginControlFlow("return when(%N(%N))", randomStatementFunction, "ctx")
        Statement::class.subclasses()
            .filter { it.hasAnnotation<GenNode>() || it.hasAnnotation<ExpressionGenNode>() }
            .forEach {
                val funSpec = FunSpec.builder("generate${it.simpleName}")
                    .addParameter(ParameterSpec.builder("ctx", Context::class).build())
                    .returns(it)
                    .addModifiers(KModifier.ABSTRACT)
                    .build()
                generatorInterface.addFunction(funSpec)
                codeBlock.addStatement("%T::class -> %N(%N)", it.asClassName(), funSpec, "ctx")
            }
        codeBlock.addStatement("else -> throw Exception(\"Unrecognized type\")")
        codeBlock.endControlFlow()

        generatorInterface.addFunction(randomStatementFunction)

        generatorInterface.addFunction(
            FunSpec.builder("generateStatement")
                .addParameter(ParameterSpec.builder("ctx", Context::class).build())
                .returns(Statement::class).addCode(codeBlock.build()).build()
        )
    }

    private fun generateTypeFunctions() {
        val randomTypeFunction = FunSpec.builder("selectRandomType")
            .addParameter(ParameterSpec.builder("ctx", Context::class).build())
            .returns(kClassType.parameterizedBy(WildcardTypeName.producerOf(Type::class.asClassName())))
            .addModifiers(KModifier.ABSTRACT)
            .build()

        val codeBlock =
            CodeBlock.builder().beginControlFlow("return when(%N(%N))", randomTypeFunction, "ctx")
        Type::class.genSubClasses().forEach {
            val funSpec = FunSpec.builder("generate${it.simpleName}")
                .addParameter(ParameterSpec.builder("ctx", Context::class).build())
                .returns(it)
                .addModifiers(KModifier.ABSTRACT)
                .build()
            generatorInterface.addFunction(funSpec)
            codeBlock.addStatement("%T::class -> %N(%N)", it.asClassName(), funSpec, "ctx")
        }
        codeBlock.addStatement("else -> throw Exception(\"Unrecognized type\")")
        codeBlock.endControlFlow()

        generatorInterface.addFunction(randomTypeFunction)

        generatorInterface.addFunction(
            FunSpec.builder("generateType")
                .addParameter(ParameterSpec.builder("ctx", Context::class).build())
                .returns(Type::class).addCode(codeBlock.build()).build()
        )
    }

    private fun generateExpressionFunctions() {
        val randomExpressionFunction = FunSpec.builder("selectRandomExpression")
            .addParameter(ParameterSpec.builder("type", Type::class.asClassName()).build())
            .addParameter(ParameterSpec.builder("ctx", Context::class).build())
            .returns(kClassType.parameterizedBy(WildcardTypeName.producerOf(Expression::class.asClassName())))
            .addModifiers(KModifier.ABSTRACT)
            .build()
        val codeBlock = CodeBlock.builder()
            .beginControlFlow("return when(%N(%N, %N))", randomExpressionFunction, "type", "ctx")
        Expression::class.subclasses().filter { it.hasAnnotation<ExpressionGenNode>() }.toSet()
            .forEach {
                val funSpec = FunSpec.builder("generate${it.simpleName}")
                    .addParameter(ParameterSpec.builder("type", Type::class.asClassName()).build())
                    .addParameter(ParameterSpec.builder("ctx", Context::class).build())
                    .returns(it)
                    .addModifiers(KModifier.ABSTRACT)
                    .build()
                generatorInterface.addFunction(funSpec)
                codeBlock.addStatement(
                    "%T::class -> %N(%N, %N)",
                    it.asClassName(),
                    funSpec,
                    "type",
                    "ctx"
                )
            }
        codeBlock.addStatement("else -> throw Exception(\"Unrecognized type\")")
        codeBlock.endControlFlow()

        generatorInterface.addFunction(randomExpressionFunction)

        generatorInterface.addFunction(
            FunSpec.builder("generateExpression")
                .returns(Expression::class)
                .addParameter(ParameterSpec.builder("type", Type::class.asClassName()).build())
                .addParameter(ParameterSpec.builder("ctx", Context::class).build())
                .addCode(codeBlock.build())
                .build()
        )
    }

    fun generate() {
        generateStatementFunctions()
        generateExpressionFunctions()
        generateTypeFunctions()
        val file = FileSpec.builder("", "AbstractASTGenerator")
        file.addType(generatorInterface.build())
        file.build().writeTo(File("src/main/kotlin/com/rustsmith/generation"))
    }
}

fun main() {
    InterfaceGenerator().generate()
}
