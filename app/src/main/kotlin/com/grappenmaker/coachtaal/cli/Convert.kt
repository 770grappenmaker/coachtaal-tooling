package com.grappenmaker.coachtaal.cli

import com.grappenmaker.coachtaal.*
import com.grappenmaker.coachtaal.Project
import org.w3c.dom.Document
import org.w3c.dom.Node
import org.w3c.dom.NodeList
import java.io.ByteArrayInputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder
import javax.xml.parsers.DocumentBuilderFactory
import kotlin.io.path.Path
import kotlin.io.path.exists
import kotlin.io.path.readBytes
import kotlin.io.path.writeText
import kotlin.math.roundToInt

sealed interface CMALanguage {
    val asCoach: Language?

    data class Unknown(val representation: String) : CMALanguage {
        override val asCoach = null
    }

    data class Matched(override val asCoach: Language) : CMALanguage

    companion object {
        fun parse(representation: String) = when (representation) {
            "EN" -> Matched(EnglishLanguage)
            "NL" -> Matched(DutchLanguage)
            else -> Unknown(representation)
        }
    }
}

data class CMAFile(
    val parts: Map<String, ByteArray>,
    val modelXML: Document? = null,
    val modelBody: String? = null,
    val modelInit: String? = null,
    val metadata: Map<String, String>? = null,
    val language: CMALanguage?,
)

object Convert : Command() {
    override val name = "convert"
    override val aliases = setOf("cv")

    val file by string()
    val target by string()

    override fun CommandContext.invoke() {
        val toRead = Path(file[this])
        if (!toRead.exists()) {
            println("File $toRead does not exist")
            return
        }

        val cma = ByteBuffer.wrap(toRead.readBytes()).parseCMA()
        val language = cma.language?.asCoach ?: DutchLanguage
        val maxIters = cma.metadata?.let { meta ->
            val start = meta["start"]?.let(language::parseConstant) ?: return@let null
            val stop = meta["stop"]?.let(language::parseConstant) ?: return@let null
            // step is only relevant for the automatic solvers, which do not exist
            // in textual models, hence this is out of scope for this piece of trash software
            (stop - start).roundToInt()
        }

        val newProject = Project(
            Path(target[this]), ProjectConfig(
                language = language.toEnum(),
                maxIterations = maxIters ?: -1
            )
        ).apply { init() }

        cma.modelInit?.let { newProject.initScriptPath.writeText(it) }
        cma.modelBody?.let { newProject.iterScriptPath.writeText(it) }
    }
}

fun Node.isValueNode() =
    hasChildNodes() && childNodes.length == 1 && childNodes.item(0).nodeType == Node.TEXT_NODE

fun NodeList.asList() = buildList<Node> {
    for (i in 0..<length) {
        add(item(i))
    }
}

fun ByteBuffer.parseCMA(): CMAFile {
    order(ByteOrder.LITTLE_ENDIAN)
    if (getInt() != 541150531) error("Invalid CMA file magic")

    val parts = hashMapOf<String, ByteArray>()

    position(0x29)
    while (hasRemaining()) {
        val length = getInt()
        mark()

        val isData = getInt() == 0 && get() == 0x0e.toByte() && getInt() == 0
        if (!isData) {
            reset()
            if (getInt() != 0x0b) {
                // don't know yet what this is
                position(position() + length - 8)
                continue
            }
        }

        val tagBytes = ByteArray(get().toInt() and 0xff)
        val contents = ByteArray(length - (if (isData) 14 else 11) - tagBytes.size)

        if (!isData) {
            require(get() == 0.toByte()) { "Invalid padding" } // seems to be padding
            get() // don't know what this means yet
        }

        get(tagBytes)
        get(contents)

        parts[tagBytes.decodeToString()] = contents
    }

    fun parseUTFBlock(tag: String) = parts[tag]?.let { block ->
        try {
            val smallLen = block.readIntLE(0)
            val len = block.readIntLE(smallLen)
            require(block.readIntLE(smallLen + 4) == 0x0b)

            val bytes16 = block.copyOfRange(smallLen + block[smallLen + 8] + 15, smallLen + len)
            String(bytes16, Charsets.UTF_16LE)
        } catch (e: IndexOutOfBoundsException) {
            block.copyOfRange(15, block.readIntLE(0)).decodeToString()
        }
    }

    val xml = parseUTFBlock("ModelXML")?.let {
        DocumentBuilderFactory.newInstance()
            .apply { isIgnoringElementContentWhitespace = true }
            .newDocumentBuilder()
            .parse(ByteArrayInputStream(it.encodeToByteArray()))
    }

    return CMAFile(
        parts = parts,
        modelXML = xml,
        modelBody = parseUTFBlock("ModelBody"),
        modelInit = parseUTFBlock("ModelInit"),
        metadata = xml?.childNodes?.item(0)?.childNodes?.asList()
            ?.filter { it.isValueNode() }
            ?.associate { it.nodeName to it.textContent },
        language = parts["Language"]?.let { CMALanguage.parse(it.decodeToString()) }
    )
}

fun ByteArray.readIntLE(offset: Int) = (this[offset].toInt() and 0xff) or
        (this[offset + 1].toInt() and 0xff shl 8) or
        (this[offset + 2].toInt() and 0xff shl 16) or
        (this[offset + 3].toInt() and 0xff shl 24)