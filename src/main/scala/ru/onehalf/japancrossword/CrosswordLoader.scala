package ru.onehalf.japancrossword

import model.{LineMetadata, Orientation, Model, ModelMetadata}
import Orientation._
import io.Source
import java.util.Properties
import scala.collection.convert.wrapAsScala._
import java.awt.Color


/**
  * Loads crosswords descriptions from *.property file.
  *
  * @since 18.05.13 14:43
  * @author OneHalf
  */
object CrosswordLoader {

  def parseParams(param: String) = new LineMetadata((param split "\\s+").map (_.toInt))

  def parseLine(orientation: Orientation, string: String) : ModelMetadata = {
    new ModelMetadata(orientation, (string split ",\\s*").map(parseParams(_)))
  }

  def loadCrosswords(propertiesFile: String): Array[Model] = {
    val properties = new Properties()
    properties.load(Source.fromURL(getClass.getResource(propertiesFile), "ISO-8859-1").bufferedReader())

    propertiesAsScalaMap(properties)
      .groupBy(tuple => tuple._1.split("\\.")(0))
      .map(tuple => new Model(
          tuple._2(tuple._1 + ".name"),
          parseLine(HORIZONTAL, tuple._2(tuple._1 + ".horizontal")),
          parseLine(VERTICAL,   tuple._2(tuple._1 + ".vertical")),
          Set(Color.BLACK)))
      .toArray
      .sortBy(_.name)
  }
}