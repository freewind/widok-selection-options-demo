package example

import org.widok._
import org.widok.html._

object Main extends PageApplication {

  private val offices: Buffer[Office] = Buffer(
    Office("office 1", Seq(
      Team("team 1", Seq(Member("aaa"), Member("bbb"))),
      Team("team 2", Seq(Member("ccc"), Member("ddd")))
    )),
    Office("office 2", Seq(
      Team("team 1", Seq(Member("aaa"), Member("bbb"))),
      Team("team 2", Seq(Member("ccc"), Member("ddd")))
    ))
  )

  private val showForm = Var(false)
  private val selectedTeam = Opt[Team]()
  private val newMemberName = Var("")

  def view() = span(
    h1("All the teams and members"),
    div(
      button("+ Add new member").onClick(_ => showForm := true)
    ),
    div(
      h2("Choose team"),
      div("current" + selectedTeam.toOption.map(_.toString).getOrElse[String]("")),
      select().options(prepareOptions()),
      text().placeholder("member name").bind(newMemberName),
      button("create").onClick(_ => createMember())
    ).show(showForm),
    div(
      offices.map { office =>
        div(
          h2(office.name),
          office.teams.map { team =>
            div(
              h3(team.name),
              ul(team.members.map(_.name).map(n => li(n)): _*)
            )
          }
        )
      }
    )
  )

  private def prepareOptions() = {
    offices.get.flatMap(_.teams.flatMap { team =>
      select.Option(team.name).enabled(value = false) +: team.members.map(m => select.Option("-- " + m.name).onClick(_ => selectedTeam := team))
    })
  }

  private def createMember(): Unit = {
    val team = selectedTeam.get
    val newMember = Member(newMemberName.get)
    offices.update { office =>
      office.copy(teams = office.teams.map { t =>
        if (t == team) {
          t.copy(members = t.members :+ newMember)
        } else {
          team
        }
      })
    }
  }

  def ready() {
    log("Page loaded.")
  }

}

case class Office(name: String, teams: Seq[Team])
case class Team(name: String, members: Seq[Member])
case class Member(name: String)
