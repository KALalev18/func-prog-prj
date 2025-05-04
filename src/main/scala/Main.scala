import scala.io.Source
import scala.util.{Try, Success, Failure}
import java.io.{File, PrintWriter, FileWriter}
import java.time.{LocalDateTime, LocalDate}
import java.time.format.DateTimeFormatter
import scala.annotation.tailrec

sealed trait EnergySource
case object Solar extends EnergySource
case object Wind extends EnergySource
case object Hydro extends EnergySource

case class EnergyReading( // Case class for energy readings
    timestamp: LocalDateTime,
    source: EnergySource,
    outputKW: Double,
    status: String
)

case class Alert( // Case class for system alerts
    timestamp: LocalDateTime,
    source: EnergySource,
    message: String,
    severity: Int
)

object REPS { // Renewable Energy Plant System

  def main(args: Array[String]): Unit = {
    println("Starting Renewable Energy Plant System...")

    val dataFile = "energy_readings.csv"
    val alertsFile = "system_alerts.csv"

    if (!new File(dataFile).exists()) {
      initializeDataFile(dataFile)
    }
    mainLoop(dataFile, alertsFile)
  }

  @tailrec
  def mainLoop(dataFile: String, alertsFile: String): Unit = { // Main loop for user interaction
    showMenu()
    val input = scala.io.StdIn.readLine()

    input match {
      case "1" =>
        collectAndStoreData(dataFile)
        mainLoop(dataFile, alertsFile)
      case "2" =>
        viewEnergyData(dataFile)
        mainLoop(dataFile, alertsFile)
      case "3" =>
        analyzeData(dataFile)
        mainLoop(dataFile, alertsFile)
      case "4" =>
        monitorSystemHealth(dataFile, alertsFile)
        mainLoop(dataFile, alertsFile)
      case "0" =>
        println("Thank you for using the program!")
      case _ =>
        println("Not in range. Select again.")
        mainLoop(dataFile, alertsFile)
    }
  }

  def initializeDataFile(filename: String): Unit = { // Initialize the data file with headers
    val pw = new PrintWriter(new File(filename))
    try {
      pw.write("Timestamp,Source,OutputKW,Status\n")
    } finally {
      pw.close()
    }
  }

  def showMenu(): Unit = { // Display the main menu
    println("\nREPS Menu")
    println("1. Collect and store energy data")
    println("2. View energy generation data")
    println("3. Analyze energy data")
    println("4. Monitor system health")
    println("0. Exit")
    print("\nSelect an option: ")
  }

  def collectAndStoreData(filename: String): Unit = { // Collect and store energy data
    println("\n== Energy data collection ==")
    println("1. Solar panel data")
    println("2. Wind turbine data")
    println("3. Hydropower data")
    print("\nSelect energy source: ")

    val source = readEnergySource()

    source match {
      case None =>
        println(
          "Invalid source, going back to main menu."
        ) // Handle invalid input
        return
      case Some(src) =>
        val outputOpt = getValidInput(
          "Enter energy output (kW): ",
          str => Try(str.toDouble).filter(_ >= 0).toOption,
          "Energy output must be a positive number." // Validate energy output
        )

        outputOpt match {
          case None => return
          case Some(output) =>
            val statusOpt = getValidInput(
              "Enter system status (OK, Warning, Error): ", // Validate system status
              str =>
                if (List("OK", "Warning", "Error").contains(str)) Some(str)
                else None,
              "Invalid status. Must be OK, Warning, or Error (case sensitive)."
            )

            statusOpt match {
              case None => return
              case Some(status) =>
                val timestamp = LocalDateTime.now()
                val reading = EnergyReading(timestamp, src, output, status)
                appendReadingToFile(filename, reading)

                println(
                  s"Data collected and saved for ${src} at ${formatTimestamp(timestamp)}" // data is processed
                )
            }
        }
    }
  }

  def getValidInput[A]( // Generic function to get valid input from user
      prompt: String,
      validator: String => Option[A],
      errorMessage: String
  ): Option[A] = {
    print(prompt)
    val input = scala.io.StdIn.readLine()
    validator(input) match {
      case Some(value) => Some(value)
      case None =>
        println(errorMessage)
        None
    }
  }

  def readEnergySource()
      : Option[EnergySource] = { // Read energy source from user input
    scala.io.StdIn.readLine() match {
      case "1" => Some(Solar)
      case "2" => Some(Wind)
      case "3" => Some(Hydro)
      case _   => None
    }
  }
}
