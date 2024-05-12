package com.grappenmaker.coachtaal.lsp

import com.grappenmaker.coachtaal.*
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.future.future
import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.CompletionItemKind.*
import org.eclipse.lsp4j.CompletionItemKind.Function
import org.eclipse.lsp4j.jsonrpc.messages.Either
import org.eclipse.lsp4j.launch.LSPLauncher
import org.eclipse.lsp4j.services.*
import java.net.URI
import java.util.concurrent.CompletableFuture
import kotlin.io.path.exists
import kotlin.io.path.readLines
import kotlin.io.path.toPath

fun main() {
    val launcher = LSPLauncher.createServerLauncher(CoachLanguageServer, System.`in`, System.out)
    CoachLanguageServer.connect(launcher.remoteProxy)
    launcher.startListening()
}

val serverVersion by lazy {
    ClassLoader.getSystemResourceAsStream("server-version")?.readBytes()?.decodeToString() ?: "unknown"
}

object CoachLanguageServer : LanguageServer, LanguageClientAware {
    val scope = CoroutineScope(Dispatchers.IO + SupervisorJob())
    private const val serverName = "Coach Language Server"
    lateinit var client: LanguageClient

    override fun initialize(params: InitializeParams) = scope.future {
        InitializeResult(
            ServerCapabilities().apply {
                textDocumentSync = Either.forLeft(TextDocumentSyncKind.Incremental)
                positionEncoding = PositionEncodingKind.UTF16
                documentFormattingProvider = Either.forLeft(true)
                completionProvider = CompletionOptions(false, emptyList())
//                hoverProvider = Either.forLeft(true)
//                signatureHelpProvider = SignatureHelpOptions(listOf("(", ";"))
//                definitionProvider = Either.forLeft(true)
//                documentSymbolProvider = Either.forLeft(true)
            },
            ServerInfo(serverName, serverVersion)
        )
    }

    override fun shutdown(): CompletableFuture<Any> = CompletableFuture.completedFuture(null)
    override fun exit() {}

    override fun getTextDocumentService() = CoachTextDocuments
    override fun getWorkspaceService() = CoachWorkspaces

    override fun connect(client: LanguageClient) {
        if (::client.isInitialized) error("LSP can only be connected to and be initialized once in its lifetime!")
        this.client = client
        client.showMessage(MessageParams(MessageType.Info, "$serverName connected successfully!"))
    }
}

object CoachWorkspaces : WorkspaceService {
    override fun didChangeConfiguration(params: DidChangeConfigurationParams) {
    }

    override fun didChangeWatchedFiles(params: DidChangeWatchedFilesParams) {
        val configChange = params.changes.find { it.uri.endsWith(coachProjectFileName) } ?: return
        forceUpdateURI(configChange.uri)
    }
}

fun forceUpdateURI(uri: String) = DocumentState(uri, URI(uri).toPath().readLines(), 0, false).register()
fun forceUpdateURI(uri: URI) = DocumentState(uri.toString(), uri.toPath().readLines(), 0, false).register()
fun String.uriPathPart() = URI(this).path

data class DocumentState(
    val uri: String,
    val lines: List<String>,
    val version: Int,
    val coach: Boolean = uri.uriPathPart().endsWith(".$coachExtension")
) {
    inline fun update(block: DocumentState.() -> DocumentState) = block(this).register()

    fun register() {
        CoachTextDocuments.sources[uri] = this

        when {
            coach -> {
                val potConfig = URI(uri).toPath().resolveSibling(coachProjectFileName)
                if (potConfig.exists()) forceUpdateURI(potConfig.toUri())

                sendDiagnostics()
            }
            uri.uriPathPart().endsWith(coachProjectFileName) -> CoachTextDocuments.updateConfig(this)
        }
    }

    fun sendDiagnostics() = CoachLanguageServer.client.publishDiagnostics(
        PublishDiagnosticsParams(uri, diagnostics(), version)
    )

    fun tryParse() = runCatching {
        parseProgram(lines.joinToString("\n"), CoachTextDocuments.config.language.underlying)
    }.map { TryParseResult.Success(it) }.getOrElse {
        if (it is ParseFailedException) TryParseResult.ParseFailed(it) else TryParseResult.UnknownError(it)
    }

    fun diagnostics(): List<Diagnostic> = when (val res = tryParse()) {
        is TryParseResult.Success -> emptyList()
        is TryParseResult.ParseFailed -> res.ex.suppressedExceptions.filterIsInstance<ParseException>().map {
            Diagnostic(
                // messy
                Range(
                    Position(
                        it.token.line - 1,
                        it.token.column - 1,
                    ),
                    Position(
                        it.token.line - 1,
                        it.token.column - 1 + it.token.lexeme.length,
                    )
                ),
                it.message ?: "Unexpected token",
                DiagnosticSeverity.Error,
                "coach-lsp"
            )
        }

        is TryParseResult.UnknownError -> listOf(
            Diagnostic(
                // messy
                fullRange(),
                res.ex.message ?: "Unknown error",
                DiagnosticSeverity.Error,
                "coach-lsp"
            )
        )
    }

    private fun fullRange() = Range(
        Position(0, 0),
        Position(lines.lastIndex, lines.last().length)
    )

    fun format() =
        if (coach) tryParse().getOrNull()?.asText()?.let { listOf(TextEdit(fullRange(), it)) } ?: emptyList()
        else null
}

fun List<String>.relevantParts(range: Range) = this[range.start.line] to this[range.end.line]

fun TextDocumentContentChangeEvent.apply(to: List<String>) = when (range) {
    null -> text.lines()
    else -> buildList {
        val (first, last) = to.relevantParts(range)
        addAll(to.take(range.start.line))

        addAll(buildString {
            append(first.take(range.start.character))
            append(text)
            append(last.drop(range.end.character))
        }.lines())

        addAll(to.drop(range.end.line + 1))
    }
}

fun List<String>.applyChanges(changes: List<TextDocumentContentChangeEvent>) =
    changes.fold(this) { acc, curr -> curr.apply(acc) }

sealed interface TryParseResult {
    data class Success(val program: ParsedProgram) : TryParseResult
    data class ParseFailed(override val ex: ParseFailedException) : TryParseFailure<ParseFailedException>
    data class UnknownError(override val ex: Throwable) : TryParseFailure<Throwable>

    sealed interface TryParseFailure<T : Throwable> : TryParseResult {
        val ex: T
    }

    fun getOrNull(): ParsedProgram? = when (this) {
        is Success -> program
        is TryParseFailure<*> -> null
    }

    fun format() = when (this) {
        is Success -> program.toString()
        is ParseFailed -> ex.message ?: "Unexpected token, no details available"
        is UnknownError -> ex.stackTraceToString()
    }
}

object CoachTextDocuments : TextDocumentService {
    val sources = mutableMapOf<String, DocumentState>()
    var config = ProjectConfig()

    override fun didOpen(params: DidOpenTextDocumentParams) {
        DocumentState(
            params.textDocument.uri,
            params.textDocument.text.lines(),
            params.textDocument.version
        ).register()
    }

    override fun didChange(params: DidChangeTextDocumentParams) {
        val state = getState(params.textDocument.uri)
        if (state.version > params.textDocument.version) return

        state.update {
            copy(
                version = params.textDocument.version,
                lines = lines.applyChanges(params.contentChanges)
            )
        }
    }

    override fun didClose(params: DidCloseTextDocumentParams) {
        sources -= params.textDocument.uri
    }

    override fun didSave(params: DidSaveTextDocumentParams) {
        val state = getState(params.textDocument.uri)
        if (params.text != null) state.update { copy(lines = params.text.lines()) }
    }

    private fun getState(uri: String) = sources[uri] ?: error("Invalid document $uri")

    override fun formatting(params: DocumentFormattingParams): CompletableFuture<List<TextEdit>> =
        CoachLanguageServer.scope.future { getState(params.textDocument.uri).format() ?: emptyList() }

    fun updateConfig(state: DocumentState) {
        config = state.lines.joinToString("\n").decodeProjectConfig()
        sources.values.filter { it.coach }.forEach { it.sendDiagnostics() }
    }

    private fun snippet(label: String, text: String) = CompletionItem(label).apply {
        kind = Snippet
        insertTextFormat = InsertTextFormat.Snippet
        insertTextMode = InsertTextMode.AdjustIndentation
        insertText = text
    }

    override fun completion(
        position: CompletionParams
    ): CompletableFuture<Either<List<CompletionItem>, CompletionList>> = CoachLanguageServer.scope.future {
        val lang = config.language.underlying

        val snippets = listOf(
            snippet(
                "if-statement", """
                    ${lang.ifStatement} ${'$'}{1:condition} then
                      ${'$'}{0}
                    ${lang.endIfStatement}
                """.trimIndent()
            ),
            snippet(
                "if-else", """
                    ${lang.ifStatement} ${'$'}{1:condition} then
                      ${'$'}{2}
                    ${lang.elseStatement}
                      ${'$'}{0}
                    ${lang.endIfStatement}
                """.trimIndent()
            ),
            snippet(
                "repeat-until", """
                    ${lang.doWhileStatement}
                      ${'$'}{0}
                    ${lang.doWhileUntil} ${'$'}{1:condition}
                """.trimIndent()
            ),
            snippet(
                "function", """
                    ${lang.startFunction} ${'$'}{1:name}
                      ${'$'}{0}
                    ${lang.endFunction}
                """.trimIndent()
            ),
            snippet(
                "redo-statement", """
                    ${lang.redoStatement} ${'$'}{1:n}
                      ${'$'}{0}
                    ${lang.endRedo}
                """.trimIndent()
            ),
            snippet(
                "while-statement", """
                    ${lang.whileStatement} ${'$'}{1:condition} ${lang.startDo}
                      ${'$'}{0}
                    ${lang.endDo}
                """.trimIndent()
            )
        )

        val state = getState(position.textDocument.uri)
        val parsed = state.tryParse().getOrNull()

        val vars = parsed?.analyzeVariables()?.let { analysis ->
            val constant = (analysis.constants.keys.map { it.value }.toSet() - lang.builtinConstants)
                .map { CompletionItem(it).apply { kind = Constant } }

            val nonConstant = (analysis.assignments - analysis.constants.keys)
                .map { CompletionItem(it.value).apply { kind = Variable } }

            constant + nonConstant
        } ?: listOf()

        val functions = parsed?.functions?.map { CompletionItem(it.name.value).apply { kind = Function } } ?: listOf()
        val builtinFunctions = lang.builtinFunctions.map { CompletionItem(it).apply { kind = Function } }
        val builtinConstants = lang.builtinConstants.map { CompletionItem(it).apply { kind = Constant } }
        val keywords = lang.keywords.map { CompletionItem(it).apply { kind = Keyword } }
        Either.forLeft(vars + builtinFunctions + keywords + builtinConstants + functions + snippets)
    }
}