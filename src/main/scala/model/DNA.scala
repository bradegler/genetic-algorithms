package model

// Simple class to represent DNA as a sequence of genes
case class DNA[A](genes: Seq[A])

// DNA companion object
object DNA {
    // Create a dna object using the supplied initializer to populate its genes
    def initial[A](geneCount: Int, initializer: (Int => A)): DNA[A] = new DNA(0.until(geneCount).map(initializer))
}
