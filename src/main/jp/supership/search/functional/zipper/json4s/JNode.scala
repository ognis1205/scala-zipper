/*
 * Copyright (C) 2016- Supership Inc.
 */
package jp.supership.search.functional.zipper.json4s

import org.json4s.JsonAST.{JArray, JNull, JObject, JValue}

/**
  * This trait specifies the inheriting class represents the context within the JSON zipper monad.
  * @author Shingo OKAWA
  */
sealed trait JNode {
  /** Returns the assigned JSON value. */
  def value: JValue

  /**
    * Filters out the satisfying the given filtration condition.
    * @param  filtration the filtration condition.
    * @return this if the filtration condition is satisfiable.
    */
  def filter(filtration: JValue => Boolean) = {
    if (filtration(value)) {
      this
    } else {
      JNode.empty
    }
  }

  /**
    * Returns true if the handling [[JValue]] is [[JArray]].
    * @return true if the handling value is [[JArray]].
    */
  def isArray = value match {
    case _: JArray => true
    case _         => false
  }

  /**
    * Returns true if the handling [[JValue]] is [[JObject]].
    * @return true if the handling value is [[JArray]].
    */
  def isObject = value match {
    case _: JObject => true
    case _          => false
  }

  /**
    * Returns true if the handling [[JValue]] is supposed to be empty.
    * @return true if the handling value is empty.
    */
  def isEmpty = value match {
    case JObject(fields) if (fields.isEmpty) => true
    case JArray(values) if (values.isEmpty)  => true
    case _                                   => false
  }

  /**
    * Returns true if the handling [[JValue]] is supposed to be plain value.
    * @return true if the handling value is plain value.
    */
  def isValue = value match {
    case _: JObject => false
    case _: JArray  => false
    case _          => false
  }
}

/**
  * This object is responsible for providing basic functionalities of the context within JSON zipper modad.
  * @author Shingo OKAWA
  */
object JNode {
  /** Holds the empty [[JNode]] singleton. */
  val empty = JNode.Empty

  /**
    * Defines the empty node representation.
    */
  case object Empty extends JNode {
    override val value = JNull
  }

  /**
    * Defines the erroneous node representation.
    * @param error the internal error representation which will be handled.
    */
  // TODO: Add Xpath Implementation here to identify the erroneous location.
  case class Error(error: (String)) extends JNode {
    override val value = JNull
  }

  /**
    * Maps [[(String, JValue)]] into [[JNode]].
    * @param  key the key string of which the corresnponding map handles.
    * @param  value the [[JValue]] of which the corresnponding map handles.
    * @return the corresponding [[NamedJNode]] instance.
    */
  def apply(key: String, value: JValue): JNode = NamedJNode(key, value)

  /**
    * Maps [[JValue]] into [[JNode]].
    * @param  value the [[JValue]] of which the corresnponding map handles.
    * @return the corresponding [[PlainJNode]] instance.
    */
  def apply(value: JValue): JNode = ValueJNode(value)

  /**
    * Maps [[JNode]] into [[JValue]].
    * @param  node the [[JNode]] of which the corresnponding map handles.
    * @return the corresponding [[JValue]] instance.
    */
  def unapply(node: JNode): Option[JValue] = Some(node.value)

  /**
    * Copies [[JNode]] into the another [[JNode]].
    * @param  node the [[JNode]] to be copied.
    * @return the copied [[JNode]] instance.
    */
  def copy(node: JNode) = node match {
    case JNode.Empty            => JNode.Empty
    case FieldJNode(key, value) => FieldJNode(key, value)
    case ValueJNode(value)      => ValueJNode(value)
    case Error(error)           => Error(error)
  }

  /**
    * Copies [[JNode]] into the another [[JNode]] with assigned new value.
    * @param  node the [[JNode]] to be copied.
    * @param  value the [[JValue]] to be newly assigned.
    * @return the copied [[JNode]] instance.
    */
  def copy(node: JNode, value: JValue) = node match {
    case JNode.Empty        => JNode.Empty
    case FieldJNode(key, _) => FieldJNode(key, value)
    case ValueJNode(_)      => ValueJNode(value)
    case Error(error)       => Error(error)
  }

  /**
    * Copies [[FieldJNode]] into the another [[FieldJNode]] with assigned new value.
    * @param  node the [[JNode]] to be copied.
    * @param  value the ([[String]], [[JValue]]) to be newly assigned.
    * @return the copied [[Element]] instance.
    */
  def copyNamedElement(node: JNode, value: (String, JValue)) = node match {
    case FieldJNode(_, _) => FieldJNode(value._1, value._2)
    case _                => node
  }
}

/**
  * Represents named node, which is responsible for handling map entries.
  * @param key the key string of which the corresnponding map handles.
  * @param value the [[JValue]] of which the corresnponding map handles.
  */
case class FieldJNode(val key: String, override val value: JValue) extends JNode

/**
  * Represents plain node, which is responsible for handling solitaly entries.
  * @param value the [[JValue]] of which the corresnponding map handles.
  */
case class ValueJNode(override value: JValue) extends JNode
