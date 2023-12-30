import PortfolioCard from "../components/portfolio-card"

export default function Page() {
  return (
    <>
      <h1 className="text-5xl font-medium mb-10">Portfolio</h1>
      <div className="md:grid md:grid-cols-2 lg:grid-cols-3 gap-8 lg:gap-14">
        <PortfolioCard
          title="Use of Factorial models"
          description="Extraction of natural pigments from the plants Justicia tinctoria and Bixa orellana as an alternative to the use of synthetic dyes in Costa Rica."
          pdf="./Factorial analysis.pdf"
        />
        <PortfolioCard
          title="Simulation theory"
          description="Use of simulation theory to test the impact of different degrees of heteroscedasticity on the assumed normality of the data"
          pdf="./Simulation Heteroscedasticity and Normality.pdf"
        />
        <PortfolioCard
          title="Classification methods"
          description="Use of supervised learning models to classify the Costa Rican population into unemployed or employed categories using data from the Continuous Employment Survey (ECE)"
          pdf="./Classification methods.pdf"
          rFile="./desempleo.R"
        />
        <PortfolioCard
          title="Mixed models"
          description="Use of mixed models to analyze the impact of a nudge on the probability of belonging to the economics career at UCR."
          pdf="./Mixed models-Nudge analysis.pdf"
          rFile="./nudges.R"
        />
      </div>
    </>
  )
}
