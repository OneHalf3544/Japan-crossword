package ru.onehalf.japancrossword

import model.{Orientation, JapanCrosswordModel, Metadata}
import Orientation._
import io.Source
import java.util.Properties
import scala.collection.convert.wrapAsScala._


/**
 * <p/>
 * <p/>
 * Created: 18.05.13 14:43
 * <p/>
 * @author OneHalf
 */
object CrosswordLoader {

  def parseParams(param: String): Array[Int] = (param split "\\s+").map (_.toInt)

  def parseLine(orientation: Orientation, string: String) : Metadata = {
    new Metadata(orientation, (string split ",\\s*").map(parseParams(_)))
  }

  def loadCrosswords(propertiesFile: String): Array[JapanCrosswordModel] = {
    val properties = new Properties()
    properties.load(Source.fromURL(getClass.getResource(propertiesFile), "ISO-8859-1").bufferedReader())

    propertiesAsScalaMap(properties)
      .groupBy(tuple => tuple._1.split("\\.")(0))
      .map(tuple => new JapanCrosswordModel(
          tuple._2(tuple._1 + ".name"),
          parseLine(HORIZONTAL, tuple._2(tuple._1 + ".horizontal")),
          parseLine(VERTICAL,   tuple._2(tuple._1 + ".vertical"))))
      .toArray
      .sortBy(_.name)
  }
}