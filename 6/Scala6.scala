object Main {
  def main(args: Array[String]): Unit = {
    println("6.29 practice")
    val a = "A (1992-)"
    val b = "B (2002)"
    val c = "C (-2012)"
    val d = "(2022)"
    val e = "E (-)"
    println(extractSingleYearOrYearEnd(a))
    println(extractSingleYearOrYearEnd(b))
    println(extractSingleYearOrYearEnd(c))
    println(extractSingleYearOrYearEnd(d))
    println(extractSingleYearOrYearEnd(e))
    println(extractAnyYear(a))
    println(extractAnyYear(b))
    println(extractAnyYear(c))
    println(extractAnyYear(d))
    println(extractAnyYear(e))
    println(extractSingleYearIfNameExists(a))
    println(extractSingleYearIfNameExists(b))
    println(extractSingleYearIfNameExists(c))
    println(extractSingleYearIfNameExists(d))
    println(extractSingleYearIfNameExists(e))
    println(extractAnyYearIfNameExists(a))
    println(extractAnyYearIfNameExists(b))
    println(extractAnyYearIfNameExists(c))
    println(extractAnyYearIfNameExists(d))
    println(extractAnyYearIfNameExists(e))

    println("6.47 practice")
    println(parseShow2("The Wire (-)"))
    println(parseShow2("(2002-2008)"))
    println(parseShow2("The Wire (2002-2008)"))

    println("6.49 caffee break")
    println(parseShows2(List("The Wire (2002-2008)", "[2019]")))
    println(parseShows2(List("The Wire (-)", "Chernobyl (2019)")))
    println(parseShows2(List("The Wire (2002-2008)", "Chernobyl (2019)")))
  }
  case class TvShow(title: String, start: Int, end: Int)

  def parseShow(rawShow: String): Option[TvShow] = {
    for {
        name      <- extractName(rawShow)
        yearStart <- extractYearStart(rawShow)
        yearEnd   <- extractYearEnd(rawShow)
    } yield TvShow(name, yearStart, yearEnd)
  }

  def extractName(rawShow: String): Option[String] = {
    val bracketOpen = rawShow.indexOf('(')
    if (bracketOpen > 0)
        Some(rawShow.substring(0, bracketOpen).trim)
    else None
  }

  def extractYearStart(rawShow: String): Option[Int] = {
    val bracketOpen = rawShow.indexOf('(')
    val dash        = rawShow.indexOf('-')
    for {
        yearStr <- if (bracketOpen != -1 && dash > bracketOpen + 1)
                        Some(rawShow.substring(bracketOpen + 1, dash))
                    else None
        year <- yearStr.toIntOption
    } yield year
  }

  def extractYearEnd(rawShow: String): Option[Int] = {
    val dash        = rawShow.indexOf('-')
    val bracketClose = rawShow.indexOf(')')
    for {
        yearStr <- if (dash != -1 && bracketClose > dash + 1)
                        Some(rawShow.substring(dash + 1, bracketClose))
                    else None
        year <- yearStr.toIntOption
    } yield year
  }

  def extractSingleYear(rawShow: String): Option[Int] = {
    val bracketOpen  = rawShow.indexOf('(')
    val dash         = rawShow.indexOf('-')
    val bracketClose = rawShow.indexOf(')')
    for {
        yearStr <- if (dash == -1 && bracketOpen != -1 && bracketClose > bracketOpen + 1)
                        Some(rawShow.substring(bracketOpen + 1, bracketClose))
                    else None
        year <- yearStr.toIntOption
    } yield year
  }

  // 6.29 practice
  def extractSingleYearOrYearEnd(rawShow: String): Option[Int] =
      extractSingleYear(rawShow).orElse(extractYearEnd(rawShow))

  def extractAnyYear(rawShow: String): Option[Int] =
      extractYearStart(rawShow).orElse(extractYearEnd(rawShow)).orElse(extractSingleYear(rawShow))

  def extractSingleYearIfNameExists(rawShow: String): Option[Int] =
      extractName(rawShow).flatMap(name => extractSingleYear(rawShow))

  def extractAnyYearIfNameExists(rawShow: String): Option[Int] =
      extractName(rawShow).flatMap(name => extractAnyYear(rawShow))

  // 6.37 caffee break
  def addOrResign(parsedShows: Option[List[TvShow]], newParsedShow: Option[TvShow]): Option[List[TvShow]] = {
    for {
      shows      <- parsedShows
      parsedShow <- newParsedShow
    } yield shows.appended(parsedShow)
  }

  // 6.47 practice
  def extractName2(rawShow: String): Either[String, String] = {
    val bracketOpen = rawShow.indexOf('(')
    if (bracketOpen > 0)
        Right(rawShow.substring(0, bracketOpen).trim)
    else Left(s"Can't extract name from $rawShow")
  }

  def extractYearStart2(rawShow: String): Either[String, Int] = {
    val bracketOpen = rawShow.indexOf('(')
    val dash = rawShow.indexOf('-')
    for {
      yearStr <-  if (bracketOpen != -1 && dash > bracketOpen + 1)
                    Right(rawShow.substring(bracketOpen + 1, dash))
                  else Left(s"Can't extract start year from $rawShow")
      year <- yearStr.toIntOption.toRight(s"Can't parse $yearStr")
    } yield year
  }

  def extractYearEnd2(rawShow: String): Either[String, Int] = {
    val dash         = rawShow.indexOf('-')
    val bracketClose = rawShow.indexOf(')')
    for {
        yearStr <-  if (dash != -1 && bracketClose > dash + 1)
                        Right(rawShow.substring(dash + 1, bracketClose))
                    else Left(s"Can't extract end year from $rawShow")
        year <- yearStr.toIntOption.toRight(s"Can't parse $yearStr")
    } yield year
  }

  def extractSingleYear2(rawShow: String): Either[String, Int] = {
    val bracketOpen  = rawShow.indexOf('(')
    val dash         = rawShow.indexOf('-')
    val bracketClose = rawShow.indexOf(')')
    for {
        yearStr <- if (dash == -1 && bracketOpen != -1 && bracketClose > bracketOpen + 1)
                        Right(rawShow.substring(bracketOpen + 1, bracketClose))
                    else Left(s"Can't extract single year from $rawShow")
        year <- yearStr.toIntOption.toRight(s"Can't parse $yearStr")
    } yield year
  }

  def parseShow2(rawShow: String): Either[String, TvShow] = {
    for {
        name      <- extractName2(rawShow)
        yearStart <- extractYearStart2(rawShow).orElse(extractSingleYear2(rawShow))
        yearEnd   <- extractYearEnd2(rawShow).orElse(extractSingleYear2(rawShow))
    } yield TvShow(name, yearStart, yearEnd)
  }

  // 6.49 caffee break
  def parseShows2(rawShows: List[String]): Either[String, List[TvShow]] = {
    val initialResult: Either[String, List[TvShow]] = Right(List.empty)
    rawShows.map(parseShow2)
            .foldLeft(initialResult)(addOrResign2)
  }

  def addOrResign2(parsedShows: Either[String, List[TvShow]], newParsedShow: Either[String, TvShow]): Either[String, List[TvShow]] = {
    for {
      shows      <- parsedShows
      parsedShow <- newParsedShow
    } yield shows.appended(parsedShow)
  }

}
