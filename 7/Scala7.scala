object Main {
	def main(args: Array[String]): Unit = {
		val artists = List(
			Artist0("Metallica", "Heavy Metal", "U.S.", 1981, true, 0),
			Artist0("Led Zepelin", "Hard Rock", "England", 1968, false, 1980),
			Artist0("Bee Gees", "Pop", "England", 1958, false, 2003),
		)

		println("// 7.5 caffee break---")
		println(searchArtists(artists, List("Pop"), List("England"),true,1950,2022))
		println(searchArtists(artists, List.empty, List("England"),true,1950,2022))
		println(searchArtists(artists, List.empty, List.empty, true, 1950, 1979))
		println(searchArtists(artists, List.empty, List.empty, true, 1981, 1984))
		println(searchArtists(artists, List("Heavy Metal"),List.empty,true,2019,2022))
		println(searchArtists(artists, List.empty, List("U.S."), true, 1950, 1959))
		println(searchArtists(artists, List.empty, List.empty, false, 2019, 2022))

		println("// 7.18 caffee break")
		 val users = List(
			User("Alice", Some("Melbourne"), List("Bee Gees")),
			User("Bob", Some("Lagos"), List("Bee Gees")),
			User("Eve", Some("Tokyo"), List.empty),
			User("Mallory", None, List("Metallica", "Bee Gees")),
			User("Trent", Some("Buenos Aires"), List("Led Zeppelin"))
		)
		println(f1(users).map(_.name))
		println(f2(users).map(_.name))
		println(f3(users).map(_.name))
		println(f4(users).map(_.name))
		println(f5(users).map(_.name))
		println(f6(users).map(_.name))

		println("// 7.29 practice")
		println(activeLength(Artist4("Metallica", MusicGenre4.HeavyMetal, Location4("U.S."), YearsActive4.StillActive(1981)), 2022))
		println(activeLength(Artist4("Led Zeppelin", MusicGenre4.HardRock, Location4("England"), YearsActive4.ActiveBetween(1968, 1980)), 2022))
		println(activeLength(Artist4("Bee Gees", MusicGenre4.Pop, Location4("England"), YearsActive4.ActiveBetween(1958, 2003)), 2022))

		println("// 7.32 caffee break")
		val fooFighters = Artist("Foo Fighters")
		val playlist1 = Playlist("This is Foo Fighters", BasedOnArtist(fooFighters),
			List(Song(fooFighters, "Breakout"), Song(fooFighters, "Learn To Fly")))

		val playlist2 = Playlist("Deep Focus", BasedOnGenres(Set(House, Funk)),
			List(Song(Artist("Daft Punk"), "One More Time"), Song(Artist("The Chemical Brothers"), "Hey Boy Hey Girl")))

		val playlist3 = Playlist("My Playlist", CuratedByUser(User("Michał Płachta")),
			List(Song(fooFighters, "My Hero"), Song(Artist("Iron Maiden"), "Trooper")))
	}

	case class Artist0(name: String, genre: String, origin: String, yearActiveStart: Int, isActive: Boolean, yearActiveEnd: Int)

	// 7.5 caffee break
	def searchArtists(
		artists: List[Artist0],
		genres: List[String],
		locations: List[String],
		searchByActiveYears: Boolean,
		activeAfter: Int,
		activeBefore: Int
	): List[Artist0] = {
		artists.filter(artist =>
			(genres.isEmpty || genres.contains(artist.genre)) &&
			(locations.isEmpty || locations.contains(artist.origin)) &&
			(!searchByActiveYears || ((artist.isActive || artist.yearActiveEnd >= activeAfter) && (artist.yearActiveStart <= activeBefore)))
		)
	}

	// 7.11 practice
	object model {
		opaque type Location = String
		object Location {
			def apply(value: String): Location = value
			extension(a: Location) def name: String = a
		}
		opaque type Genre = String
		object Genre {
			def apply(value: String): Genre = value
			extension(a: Genre) def name: String = a
		}
		opaque type YearActiveStart = Int
		object YearActiveStart {
			def apply(value: Int): YearActiveStart = value
			extension(a: YearActiveStart) def value: Int = a
		}
		opaque type YearActiveEnd = Int
		object YearActiveEnd {
			def apply(value: Int): YearActiveEnd = value
			extension(a: YearActiveEnd) def value: Int = a
		}

		case class Artist1(name: String,
							genre: Genre,
							origin: Location,
							yearActiveStart: YearActiveStart,
							isActive: Boolean, yearActiveEnd: YearActiveEnd
		)
	}
	import model._

	def searchArtists1(
		artists: List[Artist1],
		genres: List[String],
		locations: List[String],
		searchByActiveYears: Boolean,
		activeAfter: Int,
		activeBefore: Int
	): List[Artist1] = {
		artists.filter(artist =>
			(genres.isEmpty || genres.contains(artist.genre.name)) &&
			(locations.isEmpty || locations.contains(artist.origin.name)) &&
			(!searchByActiveYears || ((artist.isActive || artist.yearActiveEnd.value >= activeAfter) &&
			(artist.yearActiveStart.value <= activeBefore)))
		)
	}

	// 7.18 caffee break
	case class User(name: String, city: Option[String], favoriteArtists: List[String])

	def f1(users: List[User]): List[User] = {
		users.filter(_.city.forall(_ == "Melbourne"))
	}
	def f2(users: List[User]): List[User] = {
		users.filter(_.city.exists(_ == "Lagos"))
	}
	def f3(users: List[User]): List[User] = {
		users.filter(_.favoriteArtists.exists(_ == "Bee Gees"))
	}
	def f4(users: List[User]): List[User] = {
		users.filter(_.city.exists(_.startsWith("T")))
	}
	def f5(users: List[User]): List[User] = {
		users.filter(_.favoriteArtists.forall(_.length > 8))
	}
	def f6(users: List[User]): List[User] = {
		users.filter(_.favoriteArtists.exists(_.startsWith("M")))
	}

	// 7.20
	case class PeriodInYears(start: Int, end: Option[Int])
	case class Artist2(name: String, genre: String, origin: Location, yearsActive: PeriodInYears)

	// 7.21
	enum MusicGenre {
		case HeavyMetal
		case Pop
		case HardRock
	}
	case class Artist3(name: String, genre: MusicGenre, origin: Location, yearsActive: PeriodInYears)

	// 7.23
	enum YearsActive {
		case StillAcrive(since: Int)
		case AcriveBetween(start: Int, end: Int)
	}

	// 7.24
	enum MusicGenre4 {
		case HeavyMetal
		case Pop
		case HardRock
	}
	opaque type Location4 = String
	object Location4 {
		def apply(value: String): Location4 = value
		extension(a: Location4) def name: String = a
	}
	enum YearsActive4 {
		case StillAcrive(since: Int)
		case ActiveBetween(start: Int, end: Int)
	}
	case class Artist4(name: String, genre: MusicGenre4, origin: Location4, yearsActive: YearsActive4)

	// 7.29
	def activeLength(artist: Artist4, currentYear: Int): Int =
		artist.yearsActive match {
			case YearsActive4.ActiveBetween(start, end) => end - start
			case YearsActive4.StillAcrive(since) => currentYear - since
		}

	// 7.32 caffee break
	object model {
		opaque type User = String
		object User {
			def apply(value: String): User = value
			extension(a: User) def name: String = a
		}
		opaque type Artist = String
		object Artist {
			def apply(value: String): Artist = value
			extension(a: Artist) def name: String = a
		}
		case class Song(artist: Artist, title: String)
		enum MusicGenre {
			case House
			case Funk
			case HipHop
		}
		enum PlaylistKind {
			case CuratedByUser(user: User)
			case BasedOnArtist(artist: Artist)
			case BasedOnGenres(genres: Set[MusicGenre])
		}
		case class Playlist(name: String, kind: PlaylistKind, songs: List[Song])
	}
	import model._, model.MusicGenre._, model.PlaylistKind._

	def gatherSongs(playlists: List[Playlist], artist: Artist, genre: MusicGenre): List[Song] =
		playlists.foldLeft(List.empty[Song])((songs, playlist) =>
			val matchingSongs = playlist.king match {
				case CuratedByUser(user)            => playlist.songs.filter(_.artist == artist)
				case BasedOnArtist(playlistaArtist) => if (playlistaArtist == artist) playlist.songs
														else List.empty
				case BasedOnGenres(genres)          => if (genres.contains(gerne)) playlist.songs
														else List.empty
			}
			songs.appendedAll(matchingSongs)
		)

	// 7.35
	enum SearchCondition {
		case SearchByGenre(genres: List[MusicGenre])
		case SearchByOrigin(locations: List[Location])
		case SearchByActiveYears(start: Int, end: Int)
	}
	def searchArtists2(artists: List[Artist], requiredConditions: List[SearchCondition]): List[Artist] =
		artists.filter(artist =>
			requiredConditions.forall(condition =>
				donfition match {
					case SearchByGenre(genres)           => genres.contains(artist.genre)
					case SearchByOrigin(locations)       => locations.contains(artist.origin)
					case SearchByActiveYears(start, end) => wasArtistActive(artist, start, end)
				}
			)
		)

	// 7.37 caffee break
	case class YearsNonactive(start: Int, end: Int)
	case class Artist5(name: String, genre: MusicGenre4,
		origin: Location4, yearsActive: YearsActive4, yearsNonacrtives: List[YearsNonactive])
	enum SearchCondition5 {
		case SearchByGenre(genres: List[MusicGenre])
		case SearchByOrigin(locations: List[Location])
		case SearchByActiveYears(start: Int, end: Int)
		case SearchByDuration(years: Int)
	}
	def searchArtists5(artists: List[Artist5], requiredConditions: List[SearchCondition5]): List[Artist5] =
		artists.filter(artist =>
			requiredConditions.forall(condition =>
				donfition match {
					case SearchByGenre(genres)           => genres.contains(artist.genre)
					case SearchByOrigin(locations)       => locations.contains(artist.origin)
					case SearchByActiveYears(start, end) => wasArtistActive(artist, start, end)
					case SearchByDuration(years)		 => years
				}
			)
		)

}
