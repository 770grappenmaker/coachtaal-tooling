package com.grappenmaker.coachtaal.idea

import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.openapi.project.Project
import com.intellij.openapi.projectRoots.JavaSdkType
import com.intellij.openapi.projectRoots.ProjectJdkTable
import com.intellij.openapi.projectRoots.impl.SdkVersionUtil
import com.intellij.openapi.util.io.toCanonicalPath
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.platform.lsp.api.LspCommunicationChannel
import com.intellij.platform.lsp.api.LspServerSupportProvider
import com.intellij.platform.lsp.api.ProjectWideLspServerDescriptor
import com.intellij.platform.lsp.api.customization.LspFormattingSupport
import java.nio.file.Files
import kotlin.io.path.Path
import kotlin.io.path.deleteIfExists
import kotlin.io.path.exists

class CoachLspServerSupportProvider : LspServerSupportProvider {
    override fun fileOpened(
        project: Project,
        file: VirtualFile,
        serverStarter: LspServerSupportProvider.LspServerStarter
    ) {
        if (file.fileType != CoachFileType) return
        serverStarter.ensureServerStarted(CoachLspServerDescriptor(project))
    }
}

class CoachLspServerDescriptor(project: Project) : ProjectWideLspServerDescriptor(project, "Coach") {
    override fun isSupportedFile(file: VirtualFile) = file.fileType == CoachFileType
    override val lspGoToDefinitionSupport = false
    override val lspHoverSupport = false
    override val lspCommunicationChannel = LspCommunicationChannel.StdIO
    override val lspFormattingSupport = object : LspFormattingSupport() {
        override fun shouldFormatThisFileExclusivelyByServer(
            file: VirtualFile,
            ideCanFormatThisFileItself: Boolean,
            serverExplicitlyWantsToFormatThisFile: Boolean
        ) = true // TODO
    }

    private fun extractLSP(): String {
        val resource = javaClass.classLoader.getResourceAsStream("lsp.jar") ?: error("No LSP found?")
        val target = Path(System.getProperty("user.home"), ".temp-coach-lsp.jar")
        runCatching {
            target.deleteIfExists()
            Files.copy(resource, target)
        }

        return target.toCanonicalPath()
    }

    override fun createCommandLine(): GeneralCommandLine {
        val jdks = ProjectJdkTable.getInstance().allJdks
        val selected = jdks.filter {
            if (it.sdkType !is JavaSdkType) return@filter false

            val info = SdkVersionUtil.getJdkVersionInfo(it.homePath ?: return@filter false) ?: return@filter false
            info.version.feature >= 8
        }
            .map { (it.sdkType as JavaSdkType).getVMExecutablePath(it) }
            .find { runCatching { Path(it).exists() }.getOrElse { false } } ?: error("Could not find appropriate JDK")

        return GeneralCommandLine(selected, "-jar", extractLSP())
    }
}