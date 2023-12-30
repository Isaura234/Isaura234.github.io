import PortfolioCard from "../components/portfolio-card"

export default function Page() {
  return (
    <>
      <h1 className="text-5xl font-medium mb-10">Portfolio</h1>
      <div className="md:grid md:grid-cols-2 lg:grid-cols-3 gap-8 lg:gap-14">
        <PortfolioCard
          title="Extracción de pigmentos naturales a partir de las plantas Justicia tinctoria y Bixa orellana como una alternativa al uso de tintes sintéticos en Costa Rica"
          description="Los tintes sintéticos son muy populares debido a su bajo costo, aunque su uso se asocia a
problemas de salud y medioambientales..."
          pdf="./Factorial analysis.pdf"
        />
        <PortfolioCard
          title="¿Qué fue primero: la heterocedasticidad o normalidad? Un análisis del efecto de la heterocedasticidad sobre las pruebas de normalidad en un diseño experimental con dos factores sin interacción"
          description="La heterocedasticidad surge cuando los datos no cumplen con el supuesto de homogeneidad de varianzas. El rompimiento de este supuesto puede conllevar a potencias más bajas o un incremento del error tipo I."
          pdf="./Simulation Heteroscedasticity and Normality.pdf"
        />
        <PortfolioCard
          title="Comparación de métodos de clasificación aplicados al mercado laboral costarricense"
          description="El desempleo es uno de los problemas económicos que más afecta a los costarricenses. Al primer trimestre de 2023, la tasa de desempleo se ubicó en 12% y se ha mantenido por encima de 11% en los últimos años."
          pdf="./Classification methods.pdf"
          rFile="./desempleo.R"
        />
        <PortfolioCard
          title="Efecto de un empujón de pertenencia social en estudiantes del curso de Introducción a la Economía de primer año de la carrera de Economía de la Universidad de Costa Rica"
          description="La educación superior costarricense presenta tasas de deserción alarmantes, en el caso de la carrera de Economía de la Universidad de Costa Rica estas tasas han rondado el 50% en la última década, explicado en parte por el rendimiento académico en los cursos introductorios."
          pdf="./Mixed models-Nudge analysis.pdf"
          rFile="./nudges.R"
        />
      </div>
    </>
  )
}
