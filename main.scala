type Command = (String, Int, Seq[String]) => Unit 

val commands: Map[Int, (String, Command)] = Map(
  0 -> ("Print usage" -> usage),
  1 -> ("Find course name of course code" -> courseNamesOfCourseCodes),
)

val commandNumbers = commands.keySet

val baseUrl = "https://kurser.lth.se/kursplaner/senaste"

def courseUrl(code: String): String = s"$baseUrl/$code.html"

val delimiters: Seq[Char] = Seq(' ', '/')

val requiredETSN15: Set[String] = "EDA260/EDA270/EDA321/EDA322/EDAF45/EDAG05/EDAN80/ETS032/ETS140/ETS141/ETS160/ETS180/ETS312/ETSA01/ETSA02/ETSA03/ETSA05/ETSF01/ETSF20/ETSF25/ETSN05/MAMN01".split("/").toSet

extension (s: String) 
  def splitArg: Seq[String] =
      val xs = delimiters.zipWithIndex.map((c, i) => (c, i, s.contains(c))).filter(_._3)
      if xs.isEmpty then Seq(s)else
        (for (c, i, has) <- xs if has yield s.split(c).toSeq).flatten
  def cap: String = s.toLowerCase.capitalize

  def pastedFromLadokAvklarat = 
    val xss: Seq[Seq[String]] = 
      s.split("\n\t\n").toSeq
        .map(_.split("\n").toSeq)
        .map(xs => xs.map(s => s.trim))
    val flat = for xs <- xss; x <- xs yield x
    val grouped = flat.filterNot(x => x == "*").grouped(5).toSeq
    grouped.map(xs => xs(2)).filter(s => requiredETSN15.contains(s))

def usage(msg: String, cmdNum: Int, args: Seq[String]) = 
  println(s"$cmdNum $msg\nscala-cli run . -- command args\n\nAvailable commands:")
  for (i, (msg, cmd)) <- commands do println(s"$i $msg")

case class Course(code: String, name: String, overlaps: Seq[String], latestYear: String, lines: Seq[String], text: String):
  def show: String = s"${code.toUpperCase};$latestYear;$name;${overlaps.mkString(",")}"
object Course:
  def headings = "kurskod;senast;namn;överlappar"
  def download(code: String): Course = 
    val sourceOpt = scala.util.Try(scala.io.Source.fromURL(courseUrl(code))).toOption
    val htmlSoupOpt: Option[String] = sourceOpt.map: source => 
      try source.getLines().mkString("\n") 
      finally source.close
    val text = org.jsoup.Jsoup
      .parse(htmlSoupOpt.getOrElse(s"  ERROR: download failed from ${courseUrl(code)}"))
      .wholeText()
    val lines = text.split("\n").filter(_.nonEmpty).toSeq
    val first = lines.lift(0).getOrElse("ERROR första raden SAKNAS").cap
    val second = lines.lift(1).getOrElse("ERROR andra raden SAKNAS").cap.stripSuffix(code.toLowerCase)
    val third = lines.lift(2).getOrElse("ERROR tredje raden SAKNAS").cap.stripSuffix(code.toLowerCase)
    val isNew: Boolean = second.trim.endsWith("för")
    //if isNew then println("==== NYA FORMATET") else println("**** GAMLA KONSTIGA")
    //println(s"===== rad 1: $first\n  === rad 2: $second\n  === rad 3: $third")
    val year = if isNew then first.split(' ').lift(1).getOrElse("??? år saknas") else
      first.cap.stripSuffix(code.toLowerCase).stripPrefix("Kursplaner").stripPrefix("Kursplan").trim
    val name = third
    val overlaps = lines
        .filter(_.contains("överlappar")).mkString.trim
        .stripPrefix("Kursen överlappar följande kurser:")
        .split(",").toSeq
        .map(_.trim)
    Course(code = code, name = name.toLowerCase.capitalize, overlaps = overlaps, latestYear = year, lines = lines, text = text)
  end download
end Course

def courseNamesOfCourseCodes(msg: String, cmdNum: Int, args: Seq[String]): Unit = 
  println(s"  Course codes in args:\n  ${args.mkString(",")}\n  Downloaded course names: ")
  println(Course.headings)
  val cs = for code <- args yield Course.download(code)
  for c <- cs do println(c.show)
  val overlappingCodes = cs.flatMap(_.overlaps).distinct.sorted.filter(_.nonEmpty)
  println(s"\nAlla överlappande:\n${overlappingCodes.mkString("/")}")
  val overlappingCourses = overlappingCodes.map(Course.download)
  val all: Seq[Course] = (cs ++ overlappingCourses).distinctBy(_.code).sortBy(_.code)
  val allShown = all.map(_.show).mkString("\n")
  println(s"\nAlla koder inkl överlappande\n$allShown")
  println(s"\n${all.map(_.code).mkString("/")}")
    

@main def run(args: String*) = 
  val cmdNum: Int = args.lift(0).flatMap(_.toIntOption).getOrElse(-1)
  if !commandNumbers.contains(cmdNum) then usage(commands(0)._1, 0, args)
  else 
    val (msg, command) = commands(cmdNum)
    println(s"Command $cmdNum: $msg")
    val expandedArgs = args.drop(1).map(_.splitArg).flatten
    command(msg, cmdNum, expandedArgs)
