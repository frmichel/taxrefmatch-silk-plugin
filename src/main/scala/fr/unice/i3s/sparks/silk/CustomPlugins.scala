package fr.unice.i3s.sparks.silk

import org.silkframework.runtime.plugin.PluginModule

class CustomPlugins extends PluginModule {

  override def pluginClasses =
    classOf[TaxrefSilkPlugin] :: Nil
}
