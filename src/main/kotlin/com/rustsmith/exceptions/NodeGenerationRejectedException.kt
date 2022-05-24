package com.rustsmith.exceptions

/* Expression Exceptions */

class ExpressionGenerationRejectedException : Exception()

class NoAvailableExpressionException : Exception()

/* Statement Exception */

class StatementGenerationRejectedException : Exception()

class NoAvailableStatementException : Exception()

/* Type Exception */

class TypeGenerationRejectedException() : Exception()

class NoAvailableTypeException : Exception()
