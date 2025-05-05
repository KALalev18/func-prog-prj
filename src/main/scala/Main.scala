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
  // 1. Jesse

  def viewEnergyData(filename: String): Unit = { // View energy generation data
    println("\n== Energy generation view ==")

    val readingsOpt = readEnergyData(filename)

    readingsOpt match {
      case Success(readings) if readings.nonEmpty =>
        println("\nTimestamp\t\t| Source\t| Output (kW)\t| Status")
        println("-" * 70)

        readings.foreach { reading => // Display each reading
          println(
            f"${formatTimestamp(reading.timestamp)}\t| ${reading.source}\t| ${reading.outputKW}%.2f\t\t| ${reading.status}"
          )
        }
        val totalEnergy = readings.map(_.outputKW).sum
        val filterBySource = (source: EnergySource) =>
          (reading: EnergyReading) => reading.source == source
        val solarReadings = readings.filter(filterBySource(Solar))
        val windReadings = readings.filter(filterBySource(Wind))
        val hydroReadings = readings.filter(filterBySource(Hydro))
        val solarTotal = solarReadings.map(_.outputKW).sum
        val windTotal = windReadings.map(_.outputKW).sum
        val hydroTotal = hydroReadings.map(_.outputKW).sum

        println("\nTotal energy: " + f"$totalEnergy%.2f kW")
        println(
          f"Solar: $solarTotal%.2f kW, Wind: $windTotal%.2f kW, Hydro: $hydroTotal%.2f kW" // Display total energy by source
        )

      case Success(_) =>
        println("No energy data available.")

      case Failure(e) =>
        println(s"Error reading energy data: ${e.getMessage}")
    }
  }

  def analyzeData(filename: String): Unit = {
    println("\n== Energy data analysis ==")

    val readingsOpt = readEnergyData(filename)

    readingsOpt match {
      case Success(readings) if readings.nonEmpty =>
        val filteredReadings = getTimeFilteredReadings(readings)

        filteredReadings match {
          case Some(timeFiltered) =>
            val sourceFiltered = getSourceFilteredReadings(timeFiltered)

            sourceFiltered match {
              case Some((filtered, sourceName)) =>
                val outputValues = filtered.map(_.outputKW)
                val statistics = calculateStatistics(outputValues)

                println(s"\nStatistical analysis for $sourceName:")
                println(f"Mean: ${statistics.mean}%.2f kW")
                println(f"Median: ${statistics.median}%.2f kW")
                println(f"Mode: ${statistics.mode}%.2f kW")
                println(f"Range: ${statistics.range}%.2f kW")
                println(f"Midrange: ${statistics.midrange}%.2f kW")

              case None =>
            }

          case None =>
        }

      case Success(_) =>
        println("No energy data available for analysis.")

      case Failure(e) =>
        println(s"Error reading energy data: ${e.getMessage}")
    }
  }

  case class Statistics( // Case class for statistical results
      mean: Double,
      median: Double,
      mode: Double,
      range: Double,
      midrange: Double
  )

  def calculateStatistics(values: List[Double]): Statistics = { // Calculate statistics
    if (values.isEmpty) {
      Statistics(0.0, 0.0, 0.0, 0.0, 0.0)
    } else {
      Statistics(
        mean = calculateMean(values),
        median = calculateMedian(values),
        mode = calculateMode(values),
        range = calculateRange(values),
        midrange = calculateMidrange(values)
      )
    }
  }

  def getTimeFilteredReadings(
      readings: List[EnergyReading]
  ): Option[List[EnergyReading]] = {
    println(
      "\nSelect time period for analysis:"
    ) // Higher-order function for filtering
    println("1. Hourly")
    println("2. Daily")
    println("3. Weekly")
    println("4. Monthly")
    print("\nSelect option: ")

    val periodOption = scala.io.StdIn.readLine()

    val now = LocalDateTime.now()

    // Higher-order function for filtering by time
    val filterByTime = (cutoff: LocalDateTime) =>
      (reading: EnergyReading) => reading.timestamp.isAfter(cutoff)

    val filtered = periodOption match {
      case "1" => // last 1 hour
        readings.filter(filterByTime(now.minusHours(1)))

      case "2" => // last 24 hours
        readings.filter(filterByTime(now.minusDays(1)))

      case "3" => // last 7 days
        readings.filter(filterByTime(now.minusDays(7)))

      case "4" => // last 30 days
        readings.filter(filterByTime(now.minusDays(30)))

      case _ =>
        println("Invalid option, using all data.")
        readings
    }

    if (filtered.isEmpty) { //  Check if filtered list is empty
      println("No data available for the selected time period.")
      None
    } else {
      Some(filtered)
    }
  }

// 2. Riaz

  // Get source-filtered readings based on user input
  def getSourceFilteredReadings(
      readings: List[EnergyReading]
  ): Option[(List[EnergyReading], String)] = {
    println("\nSelect energy source:")
    println("1. All sources")
    println("2. Solar")
    println("3. Wind")
    println("4. Hydro")
    print("\nSelect option: ")

    val sourceOption = scala.io.StdIn.readLine()

    val (filtered, sourceName) =
      sourceOption match { // Higher-order function for filtering by source
        case "1" => (readings, "All Sources")
        case "2" => (readings.filter(_.source == Solar), "Solar")
        case "3" => (readings.filter(_.source == Wind), "Wind")
        case "4" => (readings.filter(_.source == Hydro), "Hydro")
        case _ =>
          println("Invalid option, using all sources.")
          (readings, "All Sources")
      }

    if (filtered.isEmpty) {
      println("No data available for the selected source.")
      None
    } else {
      Some((filtered, sourceName))
    }
  }

  // Function to monitor system health and generate alerts
  def monitorSystemHealth(dataFile: String, alertsFile: String): Unit = {
    println("\n== System health monitoring ==")
    val readingsOpt = readEnergyData(dataFile)

    readingsOpt match {
      case Success(readings)
          if readings.nonEmpty => // Check if readings are available
        val readingsBySource = readings.groupBy(_.source)

        println("\nCurrent system status:")
        List(Solar, Wind, Hydro).foreach { source => // Iterate over each source
          readingsBySource
            .get(source)
            .flatMap(_.maxByOption(_.timestamp))
            .foreach { latestReading =>
              val (status, message) = checkEnergyOutput(
                latestReading,
                readingsBySource.getOrElse(source, List.empty)
              )
              println(s"${source} System: $status") // Display current status
              if (message.nonEmpty) {
                val alert = Alert(
                  LocalDateTime.now(),
                  source,
                  message,
                  if (status == "Critical") 5 else 3
                )
                saveAlert(alertsFile, alert)
              }
            }
        }
        displayRecentAlerts(alertsFile)

      case Success(_) =>
        println("No energy data available for monitoring.")

      case Failure(e) =>
        println(s"Error reading energy data: ${e.getMessage}")
    }
  }

  def displayRecentAlerts(alertsFile: String): Unit = { // Display recent alerts
    println("\nRecent Alerts:")

    val alertsOpt = readAlerts(alertsFile)

    alertsOpt match {
      case Success(alerts) if alerts.nonEmpty =>
        val recentAlerts = alerts
          .sortBy(_.timestamp)(Ordering[LocalDateTime].reverse)
          .take(5)

        recentAlerts.foreach { alert =>
          println(
            s"[${formatTimestamp(alert.timestamp)}] ${alert.source}: ${alert.message} (Severity: ${alert.severity})"
          )
        }

      case _ => println("No alerts found.")
    }
  }

  def checkEnergyOutput( // Check energy output and generate alerts
      current: EnergyReading,
      history: List[EnergyReading]
  ): (String, String) = {
    if (current.status == "Error") {
      return ("Critical", s"${current.source} system reporting Error status!")
    }

    if (current.status == "Warning") {
      return ("Warning", s"${current.source} system reporting Warning status.")
    }

    if (history.size >= 5) { // Check if there are enough historical readings
      val recentReadings = history
        .sortBy(_.timestamp)(Ordering[LocalDateTime].reverse)
        .take(5)

      val avgOutput = calculateMean(recentReadings.map(_.outputKW))

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

    val minThreshold =
      current.source match { // Set minimum thresholds based on source
        case Solar => 10.0
        case Wind  => 15.0
        case Hydro => 50.0
      }

    if (current.outputKW < minThreshold) {
      return (
        "Warning",
        s"${current.source} output below minimum threshold (${f"${current.outputKW}%.2f"} kW vs ${f"$minThreshold%.2f"} kW)"
      )
    }

    // All checks passed
    ("Normal", "")
  }

  // 3. Shafim

  // Statistical functions
  def calculateMean(values: List[Double]): Double = {
    if (values.isEmpty) 0.0 else values.sum / values.length
  }

  def calculateMedian(values: List[Double]): Double = { // Calculate median
    if (values.isEmpty) return 0.0

    val sortedValues = values.sorted
    val n = sortedValues.length

    if (n % 2 == 0) {
      val middle1 = sortedValues(n / 2 - 1)
      val middle2 = sortedValues(n / 2)
      (middle1 + middle2) / 2.0
    } else {
      sortedValues(n / 2)
    }
  }

  def calculateMode(values: List[Double]): Double = {
    if (values.isEmpty) return 0.0

    val frequencies = values.foldLeft(Map.empty[Double, Int]) {
      (map, value) => // Count frequencies
        map + (value -> (map.getOrElse(value, 0) + 1))
    }

    // Find the max frequency
    val maxFrequency = frequencies.values.max

    // Find all values with the max frequency
    val modes = frequencies.filter(_._2 == maxFrequency).keys.toList

    // Return the smallest mode if there are multiple
    modes.min
  }

  // Range calculation
  def calculateRange(values: List[Double]): Double = {
    if (values.isEmpty) 0.0 else values.max - values.min
  }

  // Midrange calculation
  def calculateMidrange(values: List[Double]): Double = {
    if (values.isEmpty) 0.0 else (values.max + values.min) / 2.0
  }

  // File I/O functions - this is the non-functional part as specified in requirements
  def appendReadingToFile(filename: String, reading: EnergyReading): Unit = {
    val fw = new FileWriter(filename, true)
    try {
      val line =
        s"${formatTimestamp(reading.timestamp)},${reading.source},${reading.outputKW},${reading.status}\n"
      fw.write(line)
    } finally {
      fw.close()
    }
  }

  def saveAlert(filename: String, alert: Alert): Unit = { // Save alert to file
    val file = new File(filename)
    val exists = file.exists()

    val fw = new FileWriter(file, true)
    try {
      if (!exists) {
        fw.write("Timestamp,Source,Message,Severity\n")
      }
      val line =
        s"${formatTimestamp(alert.timestamp)},${alert.source},${alert.message},${alert.severity}\n"
      fw.write(line)
    } finally {
      fw.close()
    }
  }

  def readEnergyData(filename: String): Try[List[EnergyReading]] = { // Read energy data from file
    Try {
      val file = new File(filename)
      if (!file.exists()) {
        return Success(List.empty)
      }

      val lines = Source.fromFile(filename).getLines().toList

      if (lines.size <= 1) {
        return Success(List.empty)
      }

      val dataLines = lines.tail

      // Parse each line into an EnergyReading using higher-order function (flatMap)
      val readings = dataLines.flatMap { line =>
        val fields = line.split(",")
        if (fields.length < 4) None
        else {
          Try {
            val timestamp = parseTimestamp(fields(0))
            val source = fields(1) match {
              case "Solar" => Solar
              case "Wind"  => Wind
              case "Hydro" => Hydro
              case _ =>
                throw new Exception(s"Unknown energy source: ${fields(1)}")
            }
            val outputKW = fields(2).toDouble
            val status = fields(3)

            EnergyReading(timestamp, source, outputKW, status)
          }.toOption
        }
      }

      readings
    }
  }

  def readAlerts(filename: String): Try[List[Alert]] = { // Read alerts from file
    Try {
      val file = new File(filename)
      if (!file.exists()) {
        return Success(List.empty)
      }

      val lines = Source.fromFile(filename).getLines().toList

      if (lines.size <= 1) {
        return Success(List.empty)
      }

      val dataLines = lines.tail

      val alerts = dataLines.flatMap { line =>
        val fields =
          line.split(",", 4)
        if (fields.length < 4) None
        else {
          Try {
            val timestamp = parseTimestamp(fields(0))
            val source = fields(1) match {
              case "Solar" => Solar
              case "Wind"  => Wind
              case "Hydro" => Hydro
              case _ =>
                throw new Exception(s"Unknown energy source: ${fields(1)}")
            }
            val message = fields(2)
            val severity = fields(3).toInt

            Alert(timestamp, source, message, severity)
          }.toOption
        }
      }

      alerts
    }
  }
  def formatTimestamp(timestamp: LocalDateTime): String = { // Format timestamp to string
    timestamp.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
  }

  def parseTimestamp(str: String): LocalDateTime = { // Parse string to LocalDateTime
    LocalDateTime.parse(str, DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
  }
}
