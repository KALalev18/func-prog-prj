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

  // Get source-filtered readings based on user input
  def getSourceFilteredReadings(
      readings: List[EnergyReading]
  ): Option[(List[EnergyReading], String)] = {
    // Ask the user to select an energy source
    println("\nSelect energy source:")
    println("1. All sources")
    println("2. Solar")
    println("3. Wind")
    println("4. Hydro")
    print("\nSelect option: ")

    // Read the user's choice
    val sourceOption = scala.io.StdIn.readLine()

    // Filter readings based on the selected source
    val (filtered, sourceName) =
      sourceOption match {
        case "1" => (readings, "All Sources") // No filtering
        case "2" => (readings.filter(_.source == Solar), "Solar") // Filter Solar
        case "3" => (readings.filter(_.source == Wind), "Wind") // Filter Wind
        case "4" => (readings.filter(_.source == Hydro), "Hydro") // Filter Hydro
        case _ =>
          // Handle invalid input
          println("Invalid option, using all sources.")
          (readings, "All Sources")
      }

    // Check if there are any readings after filtering
    if (filtered.isEmpty) {
      println("No data available for the selected source.")
      None // Return None if no data
    } else {
      Some((filtered, sourceName)) // Return filtered readings and source name
    }
  }

  // Monitor system health and generate alerts
  def monitorSystemHealth(dataFile: String, alertsFile: String): Unit = {
    println("\n== System health monitoring ==")
    // Read energy data from the file
    val readingsOpt = readEnergyData(dataFile)

    readingsOpt match {
      case Success(readings) if readings.nonEmpty =>
        // Group readings by energy source
        val readingsBySource = readings.groupBy(_.source)

        println("\nCurrent system status:")
        // Check the status for each energy source
        List(Solar, Wind, Hydro).foreach { source =>
          readingsBySource
            .get(source) // Get readings for the source
            .flatMap(_.maxByOption(_.timestamp)) // Get the latest reading
            .foreach { latestReading =>
              // Check the energy output and get status and message
              val (status, message) = checkEnergyOutput(
                latestReading,
                readingsBySource.getOrElse(source, List.empty)
              )
              println(s"${source} System: $status") // Print the status
              if (message.nonEmpty) {
                // Create an alert if there is a message
                val alert = Alert(
                  LocalDateTime.now(),
                  source,
                  message,
                  if (status == "Critical") 5 else 3 // Set severity
                )
                saveAlert(alertsFile, alert) // Save the alert
              }
            }
        }
        // Show recent alerts
        displayRecentAlerts(alertsFile)

      case Success(_) =>
        // No readings available
        println("No energy data available for monitoring.")

      case Failure(e) =>
        // Handle error while reading data
        println(s"Error reading energy data: ${e.getMessage}")
    }
  }

  // Display recent alerts from the alerts file
  def displayRecentAlerts(alertsFile: String): Unit = {
    println("\nRecent Alerts:")

    // Read alerts from the file
    val alertsOpt = readAlerts(alertsFile)

    alertsOpt match {
      case Success(alerts) if alerts.nonEmpty =>
        // Sort alerts by timestamp and take the latest 5
        val recentAlerts = alerts
          .sortBy(_.timestamp)(Ordering[LocalDateTime].reverse)
          .take(5)

        // Print each alert
        recentAlerts.foreach { alert =>
          println(
            s"[${formatTimestamp(alert.timestamp)}] ${alert.source}: ${alert.message} (Severity: ${alert.severity})"
          )
        }

      case _ =>
        // No alerts found
        println("No alerts found.")
    }
  }

  // Check energy output and generate alerts if needed
  def checkEnergyOutput(
      current: EnergyReading,
      history: List[EnergyReading]
  ): (String, String) = {
    // If the current status is "Error", return critical status
    if (current.status == "Error") {
      return ("Critical", s"${current.source} system reporting Error status!")
    }

    // If the current status is "Warning", return warning status
    if (current.status == "Warning") {
      return ("Warning", s"${current.source} system reporting Warning status.")
    }

    // Check if there are at least 5 historical readings
    if (history.size >= 5) {
      // Get the 5 most recent readings
      val recentReadings = history
        .sortBy(_.timestamp)(Ordering[LocalDateTime].reverse)
        .take(5)

      // Calculate the average output
      val avgOutput = calculateMean(recentReadings.map(_.outputKW))

      // Check if the current output is significantly lower than the average
      if (current.outputKW < avgOutput * 0.7) {
        return (
          "Warning",
          s"${current.source} output has dropped significantly (${f"${current.outputKW}%.2f"} kW vs avg ${f"$avgOutput%.2f"} kW)"
        )
      }
      if (current.outputKW < avgOutput * 0.3) {
        return (
          "Critical",
          s"${current.source} output critically low (${f"${current.outputKW}%.2f"} kW vs avg ${f"$avgOutput%.2f"} kW)"
        )
      }
    }

    // Set minimum thresholds for each energy source
    val minThreshold =
      current.source match {
        case Solar => 10.0
        case Wind  => 15.0
        case Hydro => 50.0
      }

    // Check if the current output is below the minimum threshold
    if (current.outputKW < minThreshold) {
      return (
        "Warning",
        s"${current.source} output below minimum threshold (${f"${current.outputKW}%.2f"} kW vs ${f"$minThreshold%.2f"} kW)"
      )
    }

    // If all checks pass, return normal status
    ("Normal", "")
  }
}
