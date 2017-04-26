package fr.unice.i3s.sparks.silk

import java.util.logging.Logger
import org.silkframework.runtime.plugin.Plugin
import org.silkframework.rule.input.SimpleTransformer
import org.silkframework.runtime.plugin.PluginModule

@Plugin(
  id = "taxrefNormalize",
  label = "TAXREF-MATCH normalize port",
  description = "Returns a normalized taxon name")
case class TaxrefSilkPlugin() extends SimpleTransformer {

  private val logger = Logger.getLogger(getClass.getName)

  override def evaluate(value: String) = {
    TaxrefMatch.normalize(value, true)
  }
}
