package RFunctionDefiner

trait RWrapper {
  def init(implicit  R : org.ddahl.rscala.RClient): Unit
}
