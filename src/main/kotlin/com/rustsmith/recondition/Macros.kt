package com.rustsmith.recondition

sealed interface Macros {
    val macroName: String
    fun toRust(): String
}

object ReconditionedDivision : Macros {
    override val macroName: String = "reconditioned_div"

    override fun toRust(): String {
        return """
            macro_rules! reconditioned_div{
                (${"$"}a:expr,${"$"}b:expr, ${"$"}zero: expr) => {
                    {
                        let denominator = ${"$"}b;
                        if (denominator != ${"$"}zero) {(${"$"}a / denominator)} else {${"$"}zero}
                    }
                }
            }
        """.trimIndent()
    }
}

object ReconditionedMod : Macros {
    override val macroName: String = "reconditioned_mod"

    override fun toRust(): String {
        return """
            macro_rules! reconditioned_mod{
                (${"$"}a:expr,${"$"}b:expr, ${"$"}zero: expr) => {
                    {
                        let denominator = ${"$"}b;
                        if (denominator != ${"$"}zero) {(${"$"}a % denominator)} else {${"$"}zero}
                    }
                }
            }
        """.trimIndent()
    }
}