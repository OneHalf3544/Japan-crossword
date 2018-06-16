package ru.onehalf.japancrossword

import model.{JapanCrosswordModel, Metadata, Orientation}
import Orientation._
import java.util.Properties

import ru.onehalf.japancrossword.model.line.LineMetadata

import scala.collection.convert.wrapAsScala._
import scala.io.Source


/**
  * Loads crosswords descriptions from *.property file.
  *
  * @since 18.05.13 14:43
  * @author OneHalf
  */
object CrosswordLoader {

  def parseParams(param: String): LineMetadata = new LineMetadata((param split "\\s+").map (_.toInt))

  def parseLine(orientation: Orientation, metadataAsString: String) : Metadata = {
    new Metadata(orientation, (metadataAsString split ",\\s*").map(parseParams))
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