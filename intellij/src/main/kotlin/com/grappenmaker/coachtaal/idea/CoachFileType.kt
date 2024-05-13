package com.grappenmaker.coachtaal.idea

import com.grappenmaker.coachtaal.coachExtension
import com.intellij.lang.Language
import com.intellij.openapi.fileTypes.LanguageFileType
import com.intellij.util.PlatformIcons
import javax.swing.Icon

object CoachLanguage : Language("Coach", "text/plain") {
    private fun readResolve(): Any = CoachLanguage
    override fun getDisplayName() = "CoachTaal"
}

object CoachFileType : LanguageFileType(CoachLanguage) {
    override fun getName(): String = "Coach"
    override fun getDescription(): String = "CoachTaal file format"
    override fun getDefaultExtension() = coachExtension
    override fun getIcon(): Icon = PlatformIcons.PROPERTIES_ICON
}