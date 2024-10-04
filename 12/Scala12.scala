import org.scalatest.funsuite.AbyFunSuite
class ch12_BasicTest extends AnyFunSuite {
    test("2 times 2 should always be 4") {
        assert(2 * 2 == 4)
    }

    // 12.4
    test("score of a guide with a description, 0 artists, and 2 monies should be 65") {
        val guide = TravelGuide(
            Attraction(
                "Yellowstone National Park",
                Some("first national park in the world"),
                Location(LocaationId("Q1214"), "Wyoming", 586107)
            ),
            List(Movie("The Hateful Eight", 155760117), Movie("Heaven's Gate", 3484331))
        )
        // 30 (description) + 0 (0 artists) + 20 (2 movies) + 15 (159 million box office)
        assert(guideScore(guide) == 65)
    }

    // 12.5 practice
        // case class Attraction(name: String, description: Option[String], location: Location)
        // enum PopCultureSubject {
        //     case Artist(name: String, followers: Int)
        //     case Movie(name: String, boxOffice: Int)
        // }
        // case class TravelGuide(attraction: Attraction, subjects: List[PopCultureSubject])
    test("score of a guide with no description, 0 artists, and 0 movies should be 0") {
        val guide = TravelGuide(
            Attraction(
                "Yellowstone National Park",
                None,
                Location(LocationId("Q1214"), "Wyoming", 586107)
            ),
            List.empty
        )
        // 0 (description) + 0 (0 artists) + 0 (0 movies)
        assert(guideScore(guide) == 0)
    }
    test("score of a guide with no description, 0 artists,
        and 2 movies with no box office earnings should be 20") {
        val guide = TravelGuide(
            Attraction(
                "Yellowstone National Park",
                None,
                Location(LocationId("Q1214"), "Wyoming", 586107)
            ),
            List(Movie("The Hateful Eight", 0), Movie("Heaven's Gate", 0))
        )
        // 0 (description) + 0 (0 artists) + 20 (2 movies) + 0 (0 million box office)
        assert(guideScore(guide) == 20)
    }
}
