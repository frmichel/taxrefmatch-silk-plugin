package fr.unice.i3s.sparks.silk

import org.silkframework.runtime.plugin.Plugin
import org.silkframework.rule.input.SimpleTransformer
import java.lang.StringBuffer

@Plugin(
  id = "taxrefNormalize",
  label = "TAXREF-MATCH normalize taxon",
  description = "Normalize a taxon name including its authority")
case class TaxrefNormalize() extends SimpleTransformer {
  override def evaluate(value: String) = {
    TaxrefMatch.normalize(value, true)
  }
}

@Plugin(
  id = "taxrefNormalizeNoDate",
  label = "TAXREF-MATCH normalize taxon with no date",
  description = "Normalize a taxon name including its authority but removing the date")
case class TaxrefNormalizeNoDate() extends SimpleTransformer {
  override def evaluate(value: String) = {
    TaxrefMatch.normalize(value, false)
  }
}

