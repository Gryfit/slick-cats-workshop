package exercises

import cats.implicits._
import instances.DbioInstances.dbioMonad
import model.domain.Breed
import model.infra.Breeds
import slick.dbio.DBIO
import util.WorkshopTest
import slick.jdbc.H2Profile.api._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Success

class Exercise6Test extends WorkshopTest {
  import module._

  /**
    * Exercise 6 - Learning traverse
    *
    * In this exercise you will learn how to use traverse to help you with
    * a lot of basic task. Implement all breed-fetching functions so that tests pass.
    * Use repositories imported from module.
    *
    * hint: https://bartosz822.github.io/slick-cats-workshop-presentation/#/10/16
    */

  /**
    * List of breeds for list of names? this one is easy, I'm sure you can do it
    *
    */

  val breedsNames = List("Abyssinian", "Devon Rex", "Maine Coon", "Norwegian Forest Cat", "Ocicat")

  def featchBreed(x: String):DBIO[Option[Breed]] = {
    val tmp = Breeds.query.filter(_.name === x)
    tmp.result.headOption
  }
  def findBreeds(names: List[String]): DBIO[List[Option[Breed]]] = {
    names.traverse(featchBreed(_))
  }

  "findBreeds" should "find breeds with given name" in rollbackWithTestData {
    for {
      breeds <- findBreeds(breedsNames)
    } yield {
      breeds.flatten.map(_.name) should contain theSameElementsAs breedsNames
    }
  }

  /**
    * How about fetching a breed for a name that is optional?
    *
    * Hint (use flatTraverse)
    *
    */

  val optionalBreedName = Some("Norwegian Forest Cat")

  def findBreed(name: Option[String]): DBIO[Option[Breed]] = {
    name.flatTraverse(featchBreed(_))
  }

  "findBreed" should "find breed for an optional name" in rollbackWithTestData {
    for {
      breed <- findBreed(optionalBreedName)
    } yield {
      breed.get.name shouldBe optionalBreedName.get
    }
  }

  val nameOrId: Either[Int, String] = Right("Maine Coon")

  /**
    * I'm sure you'll find it useful that traverse works for eithers as well.
    *
    */
  def featchBreedEither(x: String):DBIO[Breed]= {
    val tmp = Breeds.query.filter(_.name === x)
    tmp.result.head
  }
  def findBreed(name: Either[Int, String]): DBIO[Either[Int, Breed]] = {
    name.traverse(featchBreedEither(_))
  }

  "findBreed" should "find breed for name in right" in rollbackWithTestData {
    for {
      breed <- findBreed(nameOrId)
    } yield {
      breed.right.get.name shouldBe nameOrId.right.get
    }
  }

  /**
    * Fetching a breed for name from future? This one is tricky
    *
    */

  val nameFromFuture = Future.successful("Ocicat")

  def findBreed(name: Future[String]): DBIO[Option[Breed]] = {
    DBIO.from(name).flatMap(featchBreed(_))
  }

  "findBreed" should "find breed for name from future" in rollbackWithTestData {
    for {
      breed <- findBreed(nameFromFuture)
    } yield {
      breed.get.name shouldBe Await.result(nameFromFuture, Duration.Inf)
    }
  }

}
