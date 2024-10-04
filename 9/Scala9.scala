object Main {
    def main(args: Array[String]): Unit = {
    }
    // 9.4
    object model {
        opaque type Currency = String
        object Currency {
            def apply(name: String): Currency = name
            extension (currency: Currency) def name: String = currency
        }
    }
    import model._
    import ch09_CurrencyExchange.exchangeRatesTableApiCall

    // 9.6 practice
    val m1: Map[String, String] = Map("key" -> "value")
    val m2: Map[String, String] = m1.updated("key2", "value2")
    val m3: Map[String, String] = m2.updated("key2", "another2")
    val m4: Map[String, String] = m3.removed("key")
    val valueFrom3: Option[String] = m3.get("key")
    val valueFrom4: Option[String] = m4.get("key")

    // 9.12
    def treading(rates: List[BigDecimal]): Boolean = {
        rates.size > 1 &&
        rates.zip(rates.drop(1))
             .forall(ratePair => ratePair match {
                case (previousRate, rate) => rate > previousRate
             })
    }

    // 9.13 caffee break
    val usdExchangeTables = List(
        Map(Currency("EUR") -> BigDecimal(0.88)),
        Map(Currency("EUR") -> BigDecimal(0.89),
            Currency("JPY") -> BigDecimal(114.62)),
        Map(Currency("JPY") -> BigDecimal(114))
    )
    def extractSingleCurrencyRate(currencyToExtract: Currency)(table: Map[Currency, BigDecimal]): Option[BigDecimal] =
        table.get(currencyToExtract)
    usdExchangeTables.map(extractSingleCurrencyRate(Currency("EUR")))

    // 9.15
    def exchangeTable(from: Currency): IO[Map[Currency, BigDecimal]] = {
        IO.delay(exchangeRatesTableApiCall(from.name)).map(table =>
            table.map(kv =>
                kv match {
                    case (currencyName, rate) => (Currency(currencyName), rate)
                }
            )
        )
    }

    // 9.16
    import ch08_SchedulingMeetings.retry
    def lastRates(from: Currency, to: Currency): IO[List[BigDecimal]] = {
        for {
            table1 <- retry(exchangeTable(from), 10)
            table2 <- retry(exchangeTable(from), 10)
            table3 <- retry(exchangeTable(from), 10)
            lastTables = List(table1, table2, table3)
        } yield lastTables.flatMap(extractSingleCurrencyRate(to))
    }
    def exchangeIfTreading(amount: BigDecimal, from: Currency, to: Currency): IO[Option[BigDecimal]] = {
        lastRates(from, to).map(rates =>
            if (treading(rates)) Some(amount * rates.last)
            else None
        )
    }

    // 9.18
    def exchangeIfTreading(amount: BigDecimal, from: Currency, to: Currency): IO[Option[BigDecimal]] = {
        for {
            rates <- lastRates(from, to)
            result <- if (treading(rates)) IO.pure(Some(amount * rates.last))
                      else exchangeIfTreading(amount, from, to)
        } yield result
    }

    // 9.21
    def exchangeIfTreading(amount: BigDecimal, from: Currency, to: Currency): IO[BigDecimal] = {
        for {
            rates <- lastRates(from, to)
            result <- if (treading(rates)) IO.pure(amount * rates.last)
                      else exchangeIfTreading(amount, from, to)
        } yield result
    }

    // 9.23 caffee break
    def currencyRate(from: Currency, to: Currency): IO[BigDecimal] = {
        for {
            table <- retry(exchangeTable(from), 10)
            rate  <- extractSingleCurrencyRate(to)(table) match {
                case Some(value) => IO.pure(value)
                case None        => currencyRate(from, to)
            }
        } yield rate
    }

    // 9. 33
    val numbers = Stream(1, 2, 3)
    val oddNumbers = numbers.filter(_ % 2 != 0)
    oddNumbers.toList
    numbers.toList
    oddNumbers.map(_ + 17).take(1).toList

    // 9.37
    import ch08_CastingDie.castTheDieImpure
    def castTheDie(): IO[Int] = IO.delay(castTheDieImpure())
    val dieCast: Stream[IO, Int] = Stream.eval(castTheDie())
    val oneDieCastProgram: IO[List[Int]] = dieCast.compile.toList
    oneDieCastProgram.unsafeRunSync()

    // 9.39
    val infiniteDieCasts: Stream[IO, Int] = Stream.eval(castTheDie()).repeat
    val firstThreeCasts: IO[List[Int]] = infiniteDieCasts.take(3).compile.toList
    firstThreeCasts.unsafeRunSync()
    val six: IO[List[Int]] = infiniteDieCasts.filter(_ == 6).take(1).compile.toList
    six.unsafeRunSync()

    // 9.40 caffee break
    val firstThreeOddNumbers: IO[List[Int]] = infiniteDieCasts.filter(_ % 2 != 0).take(3).compile.toList
    firstThreeOddNumbers.unsafeRunSync()
    val firstFiveMultiSix: IO[List[Int]] = infiniteDieCasts.take(5).map(x => if(x==6) 12 else x).compile.toList
    firstFiveMultiSix.unsafeRunSync()
    val firstThreeTotal: IO[Int] = infiniteDieCasts.take(3).compile.toList.map(_.sum)
    firstThreeTotal.unsafeRunSync()
    val firstFifthThenTwoTake: IO[List[Int]] = infiniteDieCasts.filter(_==5).take(1).append(infiniteDieCasts.take(2)).compile.toList
    firstFifthThenTwoTake.unsafeRunSync()
    infiniteDieCasts.take(100).compile.drain.unsafeRunSync()
    val firstThreeThenTriThree: IO[List[Int]] = infiniteDieCasts.take(3).append(infiniteDieCasts.take(3).map(_*3)).compile.toList
    firstThreeThenTriThree.unsafeRunSync()
    val castSixInRow: IO[List[Int]] = infiniteDieCasts.scan(0)((sixesInRow, current) => if (current == 6) sixesInRow + 1 else 0)
                                                        .filter(_ == 2).take(1).compile.toList
    castSixInRow.unsafeRunSync()

    // 9.42
    def rates(from: Currency, to:Currency): Stream[IO, BigDecimal] = {
        Stream.eval(exchangeTable(from))
              .repeat
              .map(extractSingleCurrencyRate(to))
              .unNone
              .orElse(rates(from, to))
    }
    val firstThreeRates = rates(Currency("USD"), Currency("EUR")).take(3).compile.toList

    // 9.45
    def exchangeIfTreading(amount: BigDecimal, from: Currency, to: Currency): IO[BigDecimal] = {
        rates(from, to)
            .sliding(3)
            .map(_.toList)
            .filter(treading)
            .map(_.last)
            .take(1)
            .compile
            .lastOrError
            .map(_ * amount)
    }

    // 9.46
    val delay: FiniteDuration = FiniteDuration(1, TimeUnit.SECONDS)
    val ticks: Stream[IO, Unit] = Stream.fixedRate[IO](delay)
    val firstThreeRates: IO[List[(BigDecimal, Unit)]] =
        rates(Currency("USD"), Currency("EUR")).zip(ticks).take(3).compile.toList
    firstThreeRates.unsafeRunSync()

    // 9.47
    val firstThreeRates: IO[List[BigDecimal]] =
        rates(Currency("USD"), Currency("EUR")).zipLeft(ticks).take(3).compile.toList
    firstThreeRates.unsafeRunSync()
    def exchangeIfTreading(amount: BigDecimal, from: Currency, to: Currency): IO[BigDecimal] = {
        rates(from, to)
            .zipLeft(ticks)
            .sliding(3)
            .map(_.toList)
            .filter(treading)
            .map(_.last)
            .take(1)
            .compile
            .lastOrError
            .map(_ * amount)
    }
}
