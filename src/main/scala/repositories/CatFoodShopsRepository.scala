package repositories

import model.domain.{CatFood, CatFoodPrice, CatFoodShop, PriceList}
import model.infra.{CatFoodPrices, CatFoodShops, CatFoods}
import slick.jdbc.H2Profile.api._

import slick.dbio.DBIO
import cats.implicits._
import cats.mtl.implicits._
import scala.concurrent.ExecutionContext.Implicits
import scala.concurrent.ExecutionContext

class CatFoodShopsRepository(
  catFoodsRepository: CatFoodsRepository
) extends IdRepository[CatFoodShop, CatFoodShops](CatFoodShops.query) {

  def findByName(name: String)(implicit ec: ExecutionContext): DBIO[Option[CatFoodShop]] = {
    query.filter(_.name === name).result.headOption
  }

  /**
    * Write a method that will compose PriceList for a shop with given Id
    *
    * Hint: you can try for {} yield {} syntax on DBIO
    * Hint#2: try DBIO.sequence
    * @param shopId id of shop for which prices should be found
    */
  def getPriceList(shopId: Long)(implicit ec: ExecutionContext): DBIO[PriceList] = {
    {for{
      shop <- CatFoodShops.query.filter(_.id === shopId)
      price <- CatFoodPrices.query if shop.id === price.shopId
      food <- CatFoods.query if price.foodId === food.id
    }yield {
      (shop,food,price.price)
    }}.result.map{
      s => PriceList(s.head._1 , s.map{
        case (s,f,p) => PriceList.Entry(f,p)
      })
    }
  }

  private def getEntries(shopId: Long)(implicit ec: ExecutionContext): DBIO[Seq[PriceList.Entry]] = {
  ???
//    {
//      for{
//      shop <- CatFoodShops.query.filter(_.id === shopId)
//      price <- CatFoodPrices.query if shop.id === price.shopId
//      }yield {
//        price
//      }
//    }.result
//      .flatMap(
//        prices => prices.toList.traverse(toEntry)
//      )
  }

  private def toEntry(catFoodPrice: CatFoodPrice)(implicit ec: ExecutionContext): DBIO[PriceList.Entry] = {
    {
      for{
        food <- CatFoods.query if food.id === catFoodPrice.foodId
      }yield {
        food
      }
    }.result
      .head
      .map(
        food => PriceList.Entry(food,catFoodPrice.pricePerGram)
      )
  }

}
