package fr.unice.i3s.sparks.silk

import org.silkframework.runtime.plugin.PluginModule

class CustomPlugins extends PluginModule {

  override def pluginClasses =
    classOf[TaxrefNormalize] ::
    classOf[TaxrefNormalizeNoDate] ::
    classOf[TaxrefNormalizeAuthor] ::
    Nil
}
