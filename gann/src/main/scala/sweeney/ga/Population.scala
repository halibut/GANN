package sweeney.ga


trait Population[G,I] {
	def testPopulation:Unit
	def createNewGeneration:Population[G,I]
}