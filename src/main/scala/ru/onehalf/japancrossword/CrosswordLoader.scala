package ru.onehalf.japancrossword

import model.{LineMetadata, Orientation, Model, ModelMetadata}
import Orientation._

import io.Source
import java.util.Properties

import ru.onehalf.japancrossword.model.line.LineMetadata

import scala.collection.convert.wrapAsScala._
import java.awt.Color


/**
  * Loads crosswords descriptions from *.property file.
  *
  * @since 18.05.13 14:43
  * @author OneHalf
  */
object CrosswordLoader {

  def loadCrosswords(propertiesFile: String): Array[Model] = {
    val properties = new Properties()
    properties.load(Source.fromURL(getClass.getResource(propertiesFile), "ISO-8859-1").bufferedReader())

    val crosswordsProps: List[Map[String, String]] = propertiesAsScalaMap(properties).toMap
      .groupBy(tuple => tuple._1.split("\\.")(0))
      .map(entry => deletePrefix(entry._2)).toList

    crosswordsProps.map(createModelByProperties(_))
      .toArray
      .sortBy(_.name)
  }

  def getColors(props: Map[String, String]): Map[String, Color] = {
    val colors = props.filter(_._1 startsWith ("color."))
    if (colors.isEmpty) {
      return Map("" -> Color.BLACK)
    }

    deletePrefix(colors).mapValues(Color.decode(_)).map(v => if (v._1 == "default") ("", v._2) else v)
  }

  def createModelByProperties(params: Map[String, String]): Model = {
    val colors = getColors(params)

    new Model(
      params("name"),
      parseLine(HORIZONTAL, params("horizontal"), colors),
      parseLine(VERTICAL, params("vertical"), colors),
      colors.values.toSet)
  }

  def deletePrefix(props: Map[String,String]): Map[String,String] = {
    props.map(v => (v._1.dropWhile(_ != '.').drop(1), v._2))
  }

  def parseLine(orientation: Orientation, string: String) : ModelMetadata = {
    new ModelMetadata(orientation, (string split ",\\s*").map(parseParams(_, Map("" -> Color.BLACK))))
  }

  def parseLine(orientation: Orientation, string: String, colors: Map[String, Color]) : ModelMetadata = {
    new ModelMetadata(orientation, (string split ",\\s*").map(parseParams(_, colors)))
  }

  def parseParams(param: String, colors: Map[String, Color]): LineMetadata =
    new LineMetadata((param split "\\s+").map (v => (v.takeWhile(_.isDigit).toInt, colors(v.dropWhile(_.isDigit)))))
}