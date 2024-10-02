import ch08_SchedulingMeetings.calendarEntriesApiCall
import cats.syntax.all._
object Main {
    def main(args: Array[String]): Unit = {
        println("// 8.11")
        println(castTheDie())
    }
    case class MeetingTime(startHour: Int, endHour: Int)
    def calendarEntriesApiCall(name: String): List[MeetingTime] = {
        static List<MeetingTime> calendarEntriesApiCall(String name) {
            Random rand = new Random();
            if (rand.nextFloat() < 0.25) throw new java.lang.RuntimeException("Connection error");
            if (name.equals("Alice"))
                    return List.of(new MeetingTime(8, 10), new MeetingTime(11, 12));
            else if (name.equals("Bob"))
                    return List.of(new MeetingTime(9, 10));
            else return List.of(new MeetingTime(rand.nextInt(5) + 8, rand.nextInt(4) + 13));
        }
    }
    def createMeetingApiCall(names: List[String], meetingTime: MeetingTime): Unit = {
        static void createMeetingApiCall(List<String> names, MeetingTime meetingTime) {
            Random rand = new Random();
            if (rand.nextFloat() < 0.25) throw new RuntimeException("❌");
            System.out.printf("SIDE-EFFECT");
        }
    }

    def calendarEntries(name: String): IO[List[MeetingTime]] = {
        IO.delay(calendarEntriesApiCall(name))
    }
    def castTheDieImpure(): Int = {
        static int castTheDieImpure() {
            System.out.println("The die is cast");
            Random rand = new Random();
            return rand.nextInt(6) + 1;
        }
    }
    def castTheDie(): IO[Int] = IO.delay(castTheDieImpure())

    import cats.effect.unsafe.implicits.global

    // 8.15 practice
    import ch08_SchedulingMeetings.calendarEntriesApiCall
    def calendarEntries(name: String): IO[List[MeetingTime]] = {
        IO.delay(calendarEntriesApiCall(name))
    }
    import ch08_SchedulingMeetings.createMeetingApiCall
    def createMeeting(names: List[String], meetingTime: MeetingTime): IO[Unit] = {
        IO.delay(createMeetingApiCall(names, meetingTime))
    }
    def scheduledMeetings(person1: String, person2: String): IO[List[MeetingTime]] = {
        for {
            person1Entries <- calendarEntries(person1)
            person2Entries <- calendarEntries(person2)
        } yield person1Entries.appendedAll(person2Entries)
    }
    def meetingsOverlap(meeting1: MeetingTime, meeting2: MeetingTime): Boolean = {
        meeting1.endHour > meeting2.startHour && meeting2.endHour > meeting1.startHour
    }
    def possibleMeetings(existingMeetings: List[MeetingTime], startHour: Int, endHour: Int, lengthHours: Int): List[MeetingTime] = {
        val slots = List.range(startHour, endHour - lengthHours + 1)
                        .map(startHour => MeetingTime(startHour, startHour + lengthHours))
        slots.filter(slot =>
            existingMeetings.forall(meeting => !meetingsOverlap(meeting, slot)))
    }
    def schedule(person1: String, person2: String, lengthHours: Int): IO[Option[MeetingTime]] = {
        for {
            existingMeetings <- scheduledMeetings(person1, person2)
            meetings = possibleMeetings(existingMeetings, 8, 16, lengthHours)
        } yield meetings.headOption
    }

    // 8.27 practice
    import ch08_CardGame._
    IO.delay(castTheDie()).orElse(IO.pure(0))
    IO.delay(drawPointCard()).orElse(IO.delay(castTheDie()))
    IO.delay(castTheDie())orElse(IO.delay(castTheDie())).orElse(IO.pure(0))
    for {
        die <- IO.delay(castTheDie()).orElse(IO.pure(0))
        card <- IO.delay(drawPointCard()).orElse(IO.pure(0))
    } yield die + card
    (for {
        card <- IO.delay(drawPointCard())
        die1 <- IO.delay(castTheDie())
        die2 <- IO.delay(castTheDie())
    } yield card + die1 + die2).orElse(IO.pure(0))

    // 8.33 caffee break
    def schedule(person1: String, person2: String, lengthHours: Int): IO[Option[MeetingTime]] = {
        for {
            existingMeetings <- scheduledMeetings(person1, person2)
                .orElse(scheduledMeetings(person1, person2))
                .orElse(IO.pure(List.empty))
            meetings = possibleMeetings(existingMeetings, 8, 16, lengthHours)
            possibleMeeting = meetings.headOption
            _ <- possibleMeeting match {
                case Some(meeting) => createMeeting(List(person1, person2), meeting)
                case None          => IO.unit
            }
        } yield possibleMeeting
    }
    def retry[A](action: IO[A], maxRetries: Int): IO[A] = {
        List.range(0, maxRetries)
            .map(_ => action)
            .foldLeft(action)((program, retryAction) => {
                program.orElse(retryAction)
            })
    }

    def scheduledMeetings(attendees: List[String]): IO[List[MeetingTime]] = {
        attendees.map(attendee => retry(calendarEntries(attendee), 10))
                 .sequence
                 .map(_.flatten)
    }
    def schedule(attendees: List[String], lengthHours: Int): IO[Option[MeetingTime]] = {
        for {
            existingMeetings <- scheduledMeetings(attendees)
            possibleMeeting = possibleMeetings(existingMeetings, 8, 16, lengthHours).headOption
            _ <- possibleMeeting match {
                case Some(meeting) => createMeeting(attendees, meeting)
                case None          => IO.unit
            }
        } yield possibleMeeting
    }

    // 8.38 practice
    def f01[A, B](x: IO[A], f: A => B): IO[B] =
                x.map(f)
    def f02[A](x: IO[IO[A]]): IO[A] =
                x.flatten
    def f03[A, B](x: IO[A], f: A => IO[B]): IO[B] =
                x.flatMap(f)
    def f04[A](x: A): IO[A] =
                IO.pure(x)
    def f05[A](impureAction: () => A): IO[A] =
                IO.delay(impureAction())
    def f06[A](x: IO[A], alternative: IO[A]): IO[A] =
                x.orElse(alternative)
    def f07[A](x: List[IO[A]]): IO[List[A]] =
                x.sequence
    def f08[A](x: Option[IO[A]]): IO[Option[A]] =
                x.sequence
    def f09[A, B](x: List[A], y: List[A]): List[A] =
                x.appendedAll(y)
    def f10[A](x: List[A], f: A => Boolean): List[A] =
                x.filter(f)
    def f11[A](x: List[A], zero: A, f: (A, A) => A): A =
                x.foldLeft(zero)(f)
    def f12[A](x: List[List[A]]): List[A] =
                x.flatten
    def f13[A, B](x: List[A], f: A => List[B]): List[B] =
                x.flatMap(f)
    def f14[A](x: List[A], f: A => Boolean): Boolean =
                x.forall(f)
    def f15[A, B](x: Set[A], f: A => B): Set[B] =
                x.map(f)
    def f16[A](x: Set[A], f: A => Boolean): Set[A] =
                x.filter(f)
    def f17[A](x: Set[A], zero: A, f: (A, A) => A): A =
                x.foldLeft(zero)(f)
    def f18[A](x: Set[Set[A]]): Set[A] =
                x.flatten
    def f19[A, B](x: Set[A], f: A => Set[B]): Set[B] =
                x.flatMap(f)
    def f20[A](x: Set[A], f: A => Boolean): Boolean =
                x.forall(f)
    def f21[A, B](x: Option[A], f: A => B): Option[B] =
                x.map(f)
    def f22[A](x: Option[A], f: A => Boolean): Option[A] =
                x.filter(f)
    def f23[A](x: Option[A], zero: A, f: (A, A) => A): A =
                x.foldLeft(zero)(f)
    def f24[A](x: Option[Option[A]]): Option[A] =
                x.flatten
    def f25[A, B](x: Option[A], f: A => Option[B]): Option[B] =
                x.flatMap(f)
    def f26[A](x: Option[A], f: A => Boolean): Boolean =
                x.forall(f)
    def f27(x: String): Option[Int] =
                x.toIntOption
    def f28[A](x: Option[A], alternative: Option[A]): Option[A] =
                x.orElse(alternative)
    def f29[A, B](x: Option[A], y: B): Either[B, A] =
                x.toRight(y)
    def f30[A, B](x: Option[A], y: B): Either[A, B] =
                x.toLeft(y)
    def f31[A](x: List[Option[A]]): Option[List[A]] =
                x.sequence
    def f32[A, B, C](x: Either[A, B], f: B => C): Either[A, C] =
                x.map(f)
    def f33[A, B, C](x: Either[A, B], zero: C, f: (C, B) => C): C =
                x.foldLeft(zero)(f)
    def f34[A, B](x: Either[A, Either[A, B]]): Either[A, B] =
                x.flatten
    def f35[A, B, C](x: Either[A, B], f: B => Either[A, C]): Either[A, C] =
                x.flatMap(f)
    def f36[A, B](x: Either[A, B], f: B => Boolean): Boolean =
                x.forall(f)
    def f37[A, B](x: Either[A, B], alternative: Either[A, B]): Either[A, B] =
                x.orElse(alternative)
    def f38[A, B](x: Either[A, B]): Option[B] =
                x.toOption
    def f39[A, B](x: List[Either[A, B]]): Either[A, List[B]] =
                x.sequence
    def f40[A, B](x: Either[A, List[B]]): List[Either[A, B]] =
                x.sequence
}
