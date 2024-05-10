package com.grappenmaker.coachtaal

import java.io.File
import java.io.InputStream
import java.io.Reader
import java.net.URI
import java.nio.charset.Charset
import java.text.SimpleDateFormat
import java.util.*
import java.util.function.BiConsumer
import java.util.function.BiFunction
import java.util.function.Function
import kotlin.io.path.Path
import kotlin.properties.PropertyDelegateProvider
import kotlin.properties.ReadOnlyProperty
import kotlin.properties.ReadWriteProperty
import kotlin.reflect.KProperty

class FrozenProperties(private val backing: Properties) : Properties(), MutableMap<Any?, Any?> by backing {
    private fun illegal(): Nothing = throw NotImplementedError("This Properties instance is frozen!")

    override fun keys() = backing.keys()
    override fun elements() = backing.elements()
    override fun getOrDefault(key: Any?, defaultValue: Any?) = backing.getOrDefault(key, defaultValue)
    override fun forEach(action: BiConsumer<in Any?, in Any?>) = backing.forEach(action)
    override fun contains(value: Any?) = backing.contains(value)
    override fun getProperty(key: String?) = backing.getProperty(key)
    override fun getProperty(key: String?, defaultValue: String?) = backing.getProperty(key, defaultValue)

    override fun setProperty(key: String?, value: String?) = illegal()
    override fun clear() = illegal()
    override fun compute(key: Any?, remappingFunction: BiFunction<in Any?, in Any?, out Any?>) = illegal()
    override fun computeIfAbsent(key: Any?, mappingFunction: Function<in Any?, out Any?>) = illegal()
    override fun computeIfPresent(key: Any?, remappingFunction: BiFunction<in Any?, in Any, out Any?>) = illegal()
    override fun merge(key: Any?, value: Any, remappingFunction: BiFunction<in Any, in Any, out Any?>) = illegal()
    override fun put(key: Any?, value: Any?) = illegal()
    override fun putAll(from: Map<out Any?, Any?>) = illegal()
    override fun putIfAbsent(key: Any?, value: Any?) = illegal()
    override fun remove(key: Any?) = illegal()
    override fun remove(key: Any?, value: Any?) = illegal()
    override fun replace(key: Any?, oldValue: Any?, newValue: Any?) = illegal()
    override fun replace(key: Any?, value: Any?) = illegal()
    override fun replaceAll(function: BiFunction<in Any?, in Any?, out Any?>) = illegal()
    override fun clone() = illegal()
    override fun load(reader: Reader?) = illegal()
    override fun load(inStream: InputStream?) = illegal()
    override fun loadFromXML(`in`: InputStream?) = illegal()

    override fun containsKey(key: Any?) = backing.containsKey(key)
    override fun containsValue(value: Any?) = backing.containsValue(value)
    override val entries get() = backing.entries
    override fun get(key: Any?) = backing[key]
    override fun isEmpty() = backing.isEmpty
    override val keys: MutableSet<Any?> get() = backing.keys
    override val size: Int get() = backing.size
    override val values: MutableCollection<Any?> get() = backing.values

    override fun toString() = "FrozenProperties($backing)"
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as FrozenProperties
        return backing == other.backing
    }

    override fun hashCode() = backing.hashCode()
}

fun Properties.freeze(): Properties = FrozenProperties(this)

abstract class TypedProperties(provider: () -> Properties) : Map<String, Any?> {
    protected val properties by lazy(provider)

    inner class Provider<T>(
        private val mapper: (String) -> T
    ) : PropertyDelegateProvider<Any?, ReadOnlyProperty<Any?, T?>> {
        override fun provideDelegate(thisRef: Any?, property: KProperty<*>): ReadOnlyProperty<Any?, T?> {
            val value by lazy { properties.getProperty(property.name.replace(' ', '.'))?.let(mapper) }
            return ReadOnlyProperty { _, _ -> value }
        }
    }

    protected fun <T> prop(mapper: (String) -> T): PropertyDelegateProvider<Any?, ReadOnlyProperty<Any?, T?>> =
        Provider(mapper)

    protected fun string() = prop { it }
    protected fun int() = prop { it.toInt() }
    protected fun float() = prop { it.toFloat() }
    protected fun double() = prop { it.toDouble() }
    protected fun short() = prop { it.toShort() }
    protected fun long() = prop { it.toLong() }
    protected fun boolean() = prop { it.toBoolean() }
    protected fun char() = prop { it.single() }

    protected open fun <T> mut(
        encode: (T) -> String,
        decode: (String) -> T,
    ) = object : ReadWriteProperty<Any?, T?> {
        override fun getValue(thisRef: Any?, property: KProperty<*>) =
            properties.getProperty(property.name.replace(' ', '.'))?.let(decode)

        override fun setValue(thisRef: Any?, property: KProperty<*>, value: T?) {
            properties.setProperty(property.name.replace(' ', '.'), value?.let(encode))
        }
    }

    protected fun mutString() = mut(
        encode = { it },
        decode = { it }
    )

    protected fun mutInt() = mut(
        encode = { it.toString() },
        decode = { it.toInt() }
    )

    protected fun mutFloat() = mut(
        encode = { it.toString() },
        decode = { it.toFloat() }
    )

    protected fun mutDouble() = mut(
        encode = { it.toString() },
        decode = { it.toDouble() }
    )

    protected fun mutShort() = mut(
        encode = { it.toString() },
        decode = { it.toShort() }
    )

    protected fun mutLong() = mut(
        encode = { it.toString() },
        decode = { it.toLong() }
    )

    protected fun mutBoolean() = mut(
        encode = { it.toString() },
        decode = { it.toBoolean() }
    )

    protected fun mutChar() = mut(
        encode = { it.toString() },
        decode = { it.single() }
    )

    override val entries: Set<Map.Entry<String, Any?>>
        get() = properties.entries.filterIsInstanceTo<Map.Entry<String, Any?>, _>(hashSetOf())

    override val keys: Set<String>
        get() = properties.keys.filterIsInstanceTo<String, _>(hashSetOf())

    override val size get() = properties.size
    override val values get() = properties.values

    override fun containsKey(key: String) = key in properties
    override fun containsValue(value: Any?) = properties.containsValue(value)
    override fun forEach(action: BiConsumer<in String, in Any?>) =
        properties.forEach { (k, v) -> if (k is String) action.accept(k, v) }

    override fun get(key: String) = properties[key]
    override fun getOrDefault(key: String, defaultValue: Any?): Any? = properties.getOrDefault(key, defaultValue)
    override fun isEmpty() = properties.isEmpty
}

abstract class FrozenTypedProperties(provider: () -> Properties) : TypedProperties({ provider().freeze() }) {
    override fun <T> mut(
        encode: (T) -> String,
        decode: (String) -> T,
    ): Nothing = throw NotImplementedError("This TypedProperties instance is frozen!")
}

abstract class ResourceProperties(
    path: String,
    loader: ClassLoader = ClassLoader.getSystemClassLoader()
) : FrozenTypedProperties({ Properties().apply { loader.getResourceAsStream(path)?.use { load(it) } } })

@Suppress("ObjectPropertyName")
data object SystemProperties : TypedProperties({ System.getProperties() }) {
    val `file encoding` by prop(Charset::forName)
    val `file separator` by char()
    val `java class path` by prop { it.split(File.pathSeparatorChar).map(::Path) }
    val `java class version` by string()
    val `java home` by prop(::Path)
    val `java io tmpdir` by prop(::Path)
    val `java library path` by prop { it.split(File.pathSeparatorChar).map(::Path) }
    val `java runtime name` by string()
    val `java runtime version` by string()
    val `java specification name` by string()
    val `java specification vendor` by string()
    val `java specification version` by int()
    val `java vendor url bug` by prop(::URI)
    val `java vendor url` by prop(::URI)
    val `java vendor version` by string()
    val `java vendor` by string()
    val `java version date` by prop { SimpleDateFormat("yyyy-MM-dd").parse(it) }
    val `java version` by int()
    val `java vm compressedOopsMode` by string()
    val `java vm info` by string()
    val `java vm name` by string()
    val `java vm specification name` by string()
    val `java vm specification vendor` by string()
    val `java vm specification version` by int()
    val `java vm vendor` by string()
    val `java vm version` by string()
    val `jdk debug` by string()
    val `line separator` by string()
    val `native encoding` by prop(Charset::forName)
    val `os arch` by string()
    val `os name` by string()
    val `os version` by string()
    val `path separator` by char()
    val `stderr encoding` by prop(Charset::forName)
    val `stdout encoding` by prop(Charset::forName)
    val `sun arch data model` by int()
    val `sun boot library path` by prop { it.split(File.pathSeparatorChar).map(::Path) }
    val `sun cpu endian` by string()
    val `sun cpu isalist` by string()
    val `sun io unicode encoding` by string()
    val `sun java command` by string()
    val `sun java launcher` by string()
    val `sun jnu encoding` by string()
    val `sun management compiler` by string()
    val `sun os patch level` by string()
    val `sun stderr encoding` by string()
    val `sun stdout encoding` by string()
    val `user country` by string()
    val `user dir` by prop(::Path)
    val `user home` by prop(::Path)
    val `user language` by prop(Locale::forLanguageTag)
    val `user name` by string()
    val `user script` by string()
    val `user variant` by string()
}

val cwd get() = SystemProperties.`user dir` ?: Path("").toAbsolutePath()