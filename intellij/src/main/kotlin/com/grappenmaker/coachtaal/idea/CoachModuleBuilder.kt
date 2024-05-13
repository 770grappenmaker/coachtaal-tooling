package com.grappenmaker.coachtaal.idea

import com.grappenmaker.coachtaal.EnumLanguage
import com.grappenmaker.coachtaal.Project
import com.grappenmaker.coachtaal.ProjectConfig
import com.grappenmaker.coachtaal.init
import com.intellij.ide.util.projectWizard.ModuleBuilder
import com.intellij.ide.util.projectWizard.ModuleWizardStep
import com.intellij.ide.util.projectWizard.WizardContext
import com.intellij.openapi.module.Module
import com.intellij.openapi.module.ModuleType
import com.intellij.openapi.module.ModuleTypeManager
import com.intellij.openapi.roots.ModifiableRootModel
import com.intellij.openapi.roots.ui.configuration.ModulesProvider
import com.intellij.openapi.vfs.LocalFileSystem
import com.intellij.openapi.vfs.VirtualFileManager
import net.miginfocom.layout.Grid
import java.awt.FlowLayout
import java.awt.GridBagConstraints
import java.awt.GridBagLayout
import javax.swing.*
import kotlin.io.path.Path


class CoachModuleBuilder : ModuleBuilder() {
    var config = ProjectConfig()

    override fun getModuleType() = CoachModule.getInstance()
    override fun createWizardSteps(
        wizardContext: WizardContext,
        modulesProvider: ModulesProvider
    ): Array<ModuleWizardStep> = arrayOf(CoachModuleStep(::config, ::config.setter))

    init {
        addModuleConfigurationUpdater(object : ModuleConfigurationUpdater() {
            override fun update(module: Module, rootModel: ModifiableRootModel) {
                val base = module.project.basePath!!
                val root = VirtualFileManager.constructUrl(LocalFileSystem.PROTOCOL, base)
                rootModel.addContentEntry(root)
                Project(Path(base), config).init()
            }
        })
    }
}

class CoachModulePanel : JPanel() {
    private val languageButtons = enumValues<EnumLanguage>().associateWith { JRadioButton(it.name) }
    private val modelLanguages = languageButtons.map { (k, v) -> v.model to k }.toMap()
    private val languageSelection = ButtonGroup().apply {
        val e = languageButtons.values
        e.forEach { add(it) }
        setSelected(e.first().model, true)
    }

    val selectedLanguage get() = modelLanguages.getValue(languageSelection.selection)
    val compilation = JCheckBox("Enable compilation", true)
    val optimization = JCheckBox("Enable optimization", false)
    val compCache = JCheckBox("Enable compilation cache", true)

    init {
        layout = GridBagLayout()
        val const = GridBagConstraints().apply {
            gridwidth = GridBagConstraints.REMAINDER
            anchor = GridBagConstraints.WEST
        }

        add(JLabel("Language"), const)
        add(JPanel(FlowLayout()).apply { languageButtons.values.forEach { add(it, FlowLayout.LEFT) } }, const)

        add(JLabel("Settings"), const)
        add(compilation, const)
        add(optimization, const)
        add(compCache, const)
    }
}

class CoachModuleStep(
    val getter: () -> ProjectConfig,
    val setter: (ProjectConfig) -> Unit,
) : ModuleWizardStep() {
    private val state = CoachModulePanel()

    override fun getComponent() = state
    override fun updateDataModel() {
        setter(getter().copy(
            language = state.selectedLanguage,
            useCompilationCache = state.compCache.isSelected,
            compiled = state.compilation.isSelected,
            optimize = state.optimization.isSelected,
        ))
    }
}

const val coachModuleTypeID = "COACH_MODULE"

class CoachModule : ModuleType<CoachModuleBuilder>(coachModuleTypeID) {
    override fun createModuleBuilder() = CoachModuleBuilder()
    override fun getName() = "Coach (with CLI)"
    override fun getDescription() = "Coach project with coachtaal-tooling CLI"
    override fun getNodeIcon(isOpened: Boolean) = CoachFileType.icon

    companion object {
        fun getInstance() = ModuleTypeManager.getInstance().findByID(coachModuleTypeID) as CoachModule
    }
}