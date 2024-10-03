object Main {
    def main(args: Array[String]): Unit = {
    }

    // 11.2
    object model {
        opaque type LocationId = String
        object LocationId {
            def apply(value: String): LocationId = value
            extension (a: LocationId) def value: String = a
        }
        case class Location(id: LocationId, name: String, population: Int)
        case class Attraction(name: String, description: Option[String], location: Location)
        enum PopCultureSubject {
            case Artist(name: String, followers: Int)
            case Movie(name: String, boxOffice: Int)
        }
        case class TravelGuide(attraction: Attraction, subjects: List[PopCultureSubject])
    }
    import model._, model.PopCultureSubject._

    // 11.4
    trait DataAccess {
        def findArtistsFromLocation(locationId: LocationId, limit: Int): IO[List[Artist]]
        def findMoviesAboutLocation(locationId: LocationId, limit: Int): IO[List[Movie]]
        def findAttractions(name: String, ordering: AttractionOrdering, limit: Int): IO[List[Attraction]]
    }
    enum AttractionOrdering {
        case ByName
        case ByLocationPopulation
    }
    import AttractionOrdering._

    // 11.6
    def travelGuide(data: DataAccess, attractionName: String): IO[Option[TravelGuide]] = {
        for {
            attractions <- data.findAttractions(attractionName, ByLocationPopulation, 1)
            guide       <- attractions.headOption match {
                                case None             => IO.pure(None)
                                case Some(attraction) =>
                                    for {
                                        artists <- data.findArtistsFromLocation(attraction.location.id, 2)
                                        movies  <- data.findMoviesAboutLocation(attraction.location.id, 2)
                                    } yield Some(TravelGuide(attraction, artists.appendedAll(movies)))
                           }
        } yield guide
    }

    // 11.8
    //Java===============================================================
    String query =
        "PREFIXf wd: <http://www.wikidata.org/entity/>\n" +
        "PREFIX wdt: <http://www.wikidata.org/prop/direct/>\n" +
        "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n" +
        "SELECT DISTINCT ?attraction ?label WHERE {\n" +
        " ?attraction wdt:P31 wd:Q570116;\n" +
        " rdfs:label ?label.\n" +
        " FILTER(LANG(?label) = \"en\").\n" +
        "} LIMIT 3";
    RDFConnection connection = RDFConnectionRemote.create()
        .dectination("https://query.wikidata.org/")
        .queryEndpoint("sparql")
        .build();
    QueryExecution exection = connection.query(QueryFactory.create(query));
    Iterator<QuerySolution> solutions = execution.execSelect();
    solutions.forEachRemaining(solution -> {
        String id = solution.getResource("attraciton").getLocalName();
        String label = solution.getLiteral("label").getString();
        System.out.printf("Got attraction %s (id = %s)%n", label, id)
    });
    execution.close();
    connection.clone();
    //Java===============================================================

    // 11.10
    val getConnection: IO[RDFConnection] = IO.delay(
        RDFConnectionRemote.create
                           .destination("https://query.wikidata.org/")
                           .queryEndpoint("sparql").build
    )
    val orderBy = ordering match {
        case ByName => "?attractionLabel"
        case ByLocationPopulation => "DESC(?population)"
    }
    val query = s"""... SELECT DISTINCT ?attraction ?attractionLabel ?description ?location ?locationLabel ?population WHERE {
        ...
    } ORDER BY $orderBy LIMIT $limit"""
    def execQuery(getConnection: IO[RDFConnection], query: String): IO[List[QuerySolution]] = {
        getConnection.flatMap(c => IO.delay(
            asScala(c.query(QueryFactory.create(query)).execSelect()).toList
        ))
    }
    def parseAttraction(s: QuerySolution): IO[Attraction] = {
        IO.delay(Attraction(
            name = s.getLiteral("attractionLabel").getString,
            description =
                if (s.contains("description"))
                    Some(s.getLiteral("description").getString)
                else None,
            location = Location(
                id = LocationId(s.getResource("location").getLocalName),
                name = s.getLiteral("locationLabel").getString,
                population = s.getLiteral("population").getInt
            )
        ))
    }

    // 11.11
    def findAttractions(name: String, ordering: AttractionOedering, limit: Int): IO[List[Attraction]] = {
        val orderBy = ordering match {
            case ByName => "?attractionLabel"
            case ByLocationPopulation => "DESC(?population)"
        }
        val query = s"""... SELECT DISTINCT ?attraction ?attractionLabel ?description ?location ?locationLabel ?population WHERE {
            ...
        } ORDER BY $orderBy LIMIT $limit"""
        for {
            solutions <- execQuery(getConnection, query)
            attractions <- solutions.traverse(parseAttraction)
            // attractions <- map(parseAttraction).sequence
        } yield attractions
    }

    // 11.14
    import ch11_TravelGuide._, model._, PopCultureSubject._
    import AttractionOrdering._
    import Version1.travelGuide
    import ch11_WikidataDataAccess.getSparqlDataAccess
    def execQuery(connection: RDFConnection)(query: String): IO[List[QuerySolution]] =
        IO.blocking(asScala(connection.query(QueryFactory.create(query)).execSelect()).toList)
    val connection = RDFConnectionRemote.create
        .destination("https://query.wikidata.org/")
        .queryEndpoint("sparql")
        .build
    val wikidata = getSparqlDataAccess(execQuery(connection))
    travelGuide(wikidata, "Yosemite").unsafeRunSync()

    // 11.16
    def guideScore(guide: TravelGuide): Int = {
        val descriptionScore = guide.attraction.description.map(_ => 30).getOrElse(0)
        val quantityScore = Math.min(40, guide.subjects.size * 10)
        val totalFollowers = guide.subjects
                .map(_ match {
                    case Artist(_, followers) => followers
                    case _                    => 0
                })
                .sum
        val totalBoxOffice = guide.subjects
                .map(_ match {
                    case Movie(_, boxOffice) => boxOffice
                    case _                   => 0
                })
                .sum
        val followersScore = Math.min(15, totalFollowers / 100_000)
        val boxOfficeScore = Math.min(15, totalBoxOffice / 10_000_000)
            descriptionScore + quntityScore + followersScore + boxOfficeScore
    }
    def travelGuide(data: DataAccess, attractionName: String): IO[Option[TravelGuide]] = {
        for {
            attractions <- data.findAttractions(attractionName, ByLocationPopulation, 3)
            guides      <- attractions.map(attraction =>
                                    for {
                                        artists <- data.findArtistsFromLocation(attraction.location.id, 2)
                                        movies  <- data.findMoviesAboutLocation(attraction.location.id, 2)
                                    } yield TravelGuide(attraction, artists.appendedAll(movies))
                            )
                            .sequence
        } yield guides.sortBy(guideScore).reverse.headOption
    }

    // 11.18
    def createExecution(connection: RDFConnection, query: String): IO[QueryExecution] =
        IO.blocking(connection.query(QueryFactory.create(query)))
    def closeExecution(execution: QueryExecution): IO[Unit] =
        IO.blocking(execution.close())
    def execQuery(connection: RDFConnection)(query: String): IO[List[QuerySolution]] = {
        for {
            exectuion <- createExecution(connection, query)
            solutions <- IO.blocking(asScala(execution.execSelect()).toList)
            _         <- closeExecution(execution)
        } yield solutions
    }

    // 11.19
    def execQuery(connection: RDFConnection)(query: String): IO[List[QuerySolution]] = {
        val executionResource: Resource[IO, QueryExecution] =
            Resource.make(createExecution(connection, query))(closeExecution)
        executionResource.use(execution =>
            IO.blocking(asScala(execution.execSelect()).toList))
    }
    val connectionResource: Resource[IO, RDFConnection] =
        Resource.make(IO.blocking(
            RDFConnectionRemote.create
                .destination("https://query.wikidata.org/")
                .queryEndpoint("sparql")
                .build
        ))(connection => IO.blocking(connection.close()))
    val program: IO[Option[TravelGuide]] =
        connectionResource.use(connection => {
            val wikidata = getSparqlDataAccess(execQuery(connection))
            travelGuide(wikidata, "Yellowstone")
        })
    program.unsafeRunSync()
}
