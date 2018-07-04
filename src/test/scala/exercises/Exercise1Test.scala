package exercises

import cats.data.OptionT
import model.domain.{Cat, Sex}
import model.infra.{Breeds, Cats}
import slick.dbio.DBIO
import slick.jdbc.H2Profile.api._
import util.WorkshopTest

import scala.concurrent.ExecutionContext.Implicits.global

class Exercise1Test extends WorkshopTest {

  /**
    * Exercise 1 - Play with slick queries
    *
    * In this exercise you will learn some basic ways of fetching data with use of slick queries.
    * Test cases below have method left for you to implement. Good luck!
    *
    * hint: https://bartosz822.github.io/slick-cats-workshop-presentation/#/8/7
    */

  /** find all cats that are older than 5*/

  def findOldCats: DBIO[Seq[Cat]] = {
    Cats.query.filter(_.age > 5).result
  }

  /** find all male cats that are older than 5*/
  def findOldMaleCats: DBIO[Seq[Cat]] = {
    Cats.query.filter(a => a.age > 5 && a.sex === Sex.Male).result
  }

  /** find all persian cats*/
  def findPersianCats: DBIO[Seq[Cat]] = {
   {for{
      breed <-Breeds.query.filter(a => a.name === "Persian")
      cat <- Cats.query if breed.id === cat.breedId
    } yield{
      cat
    }}.result
  }

  /** find how many calories are needed to feed all old cats*/
  def findCaloricNeedsForOldCats: DBIO[BigDecimal] = {
  {
    for{
      breed <-Breeds.query
      cat <- Cats.query if breed.id === cat.breedId && cat.age > 5
    }yield{
      breed.caloriesPerDay
    }
  }.sum
    .result
    .map(_.get)
  }

  /** find how many calories are needed for cats grouped by their breed*/
  def findCaloricNedsForBreeds: DBIO[Seq[(String, BigDecimal)]] = {
    val q = (for {
      cat <- Cats.query
      breed <- Breeds.query if breed.id === cat.breedId
    } yield (cat, breed)).groupBy(_._2.name).map { case (bread, b) => (bread, b.map(_._2.caloriesPerDay).sum.get)}
        q.result
  }

  "findOldCats" should "return cats with age greater than 5" in rollbackWithTestData {
    for {
      foundCats <- findOldCats
    } yield {
      foundCats.map(_.name) should contain theSameElementsAs Seq("Shadow", "Bailey", "Marley", "Boo")
    }
  }

  "findOldMaleCats" should "return male cats with age greater than 5" in rollbackWithTestData {
    for {
      foundCats <- findOldMaleCats
    } yield {
      foundCats.map(_.name) should contain theSameElementsAs Seq("Shadow", "Bailey", "Marley")
    }
  }

  "findPersianCats" should "return persian cats" in rollbackWithTestData {
    for {
      foundCats <- findPersianCats
    } yield {
      foundCats.map(_.name) should contain theSameElementsAs Seq("Gizmo", "Cali", "Noodle")
    }
  }

  "findCaloricNeedsForOldCats" should "sum of daily caloric needs for old cats" in rollbackWithTestData {
    for {
      caloriesSum <- findCaloricNeedsForOldCats
    } yield {
      caloriesSum shouldBe 2480
    }
  }

  "findCaloricNeedsForBreeds" should "sum of daily caloric needs for old cats" in rollbackWithTestData {
    for {
      caloriesSum <- findCaloricNedsForBreeds
    } yield {
      caloriesSum should contain theSameElementsAs Seq(
        "Abyssinian" -> 1000,
        "Devon Rex" -> 800,
        "Maine Coon" -> 2280,
        "Norwegian Forest Cat" -> 1220,
        "Ocicat" -> 1080,
        "Persian" -> 1860,
        "Russian Blue" ->1180,
        "Siamese" -> 1120,
        "Ukrainian Levkoy" -> 1300
      )
    }
  }

}
