package com.grappenmaker.coachtaal.cli

class DefaultParameter<T>(
    override val index: Int,
    override val name: String,
    val default: String,
    private val delegate: Parameter<T>
) : Parameter<T> {
    // hacky hacky
    override val optional = true
    override fun get(on: List<String>) = delegate[on]
}

fun <T> Parameter<T>.withDefault(default: String) = DefaultParameter(index, name, default, this)

class StringParameter(override val index: Int, override val name: String) : Parameter<String> {
    override fun get(on: List<String>) = on[index]
}

class IntParameter(override val index: Int, override val name: String) : Parameter<Int> {
    override fun get(on: List<String>) = on[index].toInt()
}

class FloatParameter(override val index: Int, override val name: String) : Parameter<Float> {
    override fun get(on: List<String>) = on[index].toFloat()
}

class BooleanParameter(override val index: Int, override val name: String) : Parameter<Boolean> {
    override fun get(on: List<String>) = on[index] == "true"
}

class EnumParameter<T : Enum<T>>(
    override val index: Int,
    override val name: String,
    override val usage: String,
    private val impl: (String) -> T,
) : Parameter<T> {
    override val optional = true
    override fun get(on: List<String>) = impl(on[index])
}