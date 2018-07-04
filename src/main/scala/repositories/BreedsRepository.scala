package repositories

import model.domain.Breed
import model.infra.Breeds
import slick.dbio.DBIO
import slick.jdbc.H2Profile.api._

import scala.concurrent.ExecutionContext


class BreedsRepository {

  private val query = Breeds.query
  private val byId = Compiled { (id: Rep[Long]) => query.filter(_.id === id) }
  private val byName = Compiled { (name: Rep[String]) => query.filter(_.name === name) }

  /** This method should fetch Breed from the database based on given ID
    *
    * @param id id of Breed to be fetched
    * @return opional breed found in database */
  def findById(id: Long)(implicit ec: ExecutionContext): DBIO[Option[Breed]] = {
    byId(id).result.headOption
  }

  /** This method should fetch Breed from the database based on given ID
    *
    * @param id id of Breed to be fetched
    * @return existing breed found in database*/
  def findExistingById(id: Long)(implicit ec: ExecutionContext): DBIO[Breed] = {
    byId(id).result.headOption.map(_.get)
  }

  /** This method should fetch Breed from the database based on name
    *
    * @param name name of Breed to be fetched
    * @return breed found in database*/
  def findByName(name: String)(implicit ec: ExecutionContext): DBIO[Option[Breed]] = {
    byName(name).result.headOption
  }

  /** This method should save breed passed as an argument
    *
    * @param breed Breed to be saved
    * @return ID of saved breed*/
  def save(breed: Breed)(implicit ec: ExecutionContext): DBIO[Long] = {
    query returning query.map(_.id) += breed
  }

  /** This method should delete breed with given Id
    *
    * @param id ID of breed to be deleted*/
  def delete(id: Long)(implicit ec: ExecutionContext): DBIO[Unit] = {
    byId(id).delete.map(_=>()) // void??
  }

  /** This method should fetch breeds with name containing given text
    *
    * @param text part of breed name to be matched
    * @return breeds with matching names*/
  def search(text: String)(implicit ec: ExecutionContext): DBIO[Seq[Breed]] = {
   query.filter(a=> (a.name indexOf text)  =!= -1).result
  }

}
