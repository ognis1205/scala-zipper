/*
 * Copyright (C) 2016- Supership Inc.
 */
package jp.supership.search.functional.zipper.json4s

import org.json4s.JsonAST.{JArray, JNull, JObject, JValue}

/**
  * This trait specifies the extending class has JSON zipper context.
  * @author Shingo OKAWA
  */
trait ZipperContext {
  import ZipperContext._

  /** Returns the currently handling node reference. */
  def focus: JNode

  /** Returns the currently handling node's left siblings' [[Stream]]. */
  def leftSiblings: Siblings

  /** Returns the currently handling node's right siblings' [[Stream]]. */
  def rightSiblings: Siblings

  /** Returns the currently handling node's parents' [[Stream]]. */
  def ancestors: Ancestors

  /** Maps this to the currently handling [[JNode]]'s value. */
  def value = focus.value

  /**
    * Returns true if the context is neighther [[IsEmpty]] nor [[HasError]].
    * @return true if this instance is supposed to be valid.
    */
  def isValid = this match {
    case _: IsEmpty  => false
    case _: HasError => false
    case _           => true
  }

  /**
    * Returns true if the context is [[IsEmpty]].
    * @return true if this instance is empty.
    */
  def isEmpty = this match {
    case _: IsEmpty => true
    case _          => false
  }

  /**
    * Returns true if the context is [[IsError]].
    * @return true if this instance is erroneous.
    */
  def isError = this match {
    case _: IsError => true
    case _          => false
  }

  /**
    * Returns true if the context is valid and focuses on [[JArray]].
    * @return true if this instance is valid and focuses on array node.
    */
  def isArray = this.isValid && (focus.value match {
    case _: JArray => true
    case _         => false
  })

  /**
    * Returns true if the context is valid and focuses on [[JObject]].
    * @return true if this instance is valid and focuses on object node.
    */
  def isObject = this.isValid && (focus.value match {
    case _: JObject => true
    case _          => false
  })

  /**
    * Returns true if the context is valid and not focused on list-wise instances.
    * @return true if this instance is valid and focues on list-wise node.
    */
  def isLeaf = this.isValid && (focus.value match {
    case _: JObject => false
    case _: JArray  => false
    case _          => true
  })

  /**
    * Maps the current [[ZipperContext]] to the upwards.
    * @return the upwised zipper context.
    */
  def up: ZipperContext = ancestors match {
    case (parentLeftSiblings, parent, parentRightSiblings) #:: grandAncestors =>
      val jValue = contextToJSON(focus, leftSiblings, rightSiblings, parent)
      ZipperContext(JNode.copy(parent, jValue), parentLeftSiblings, parentRightSiblings, grandAncestors)
    case Stream.Empty =>
      ZipperContext.Empty
  }

  /**
    * Maps the current [[ZipperContext]] to the downwards.
    * @return the downwised zipper context.
    */
  def down: ZipperContext = focus.value match {
    case JObject(fields) => fields.toList match {
      case Nil                  => ZipperContext.Empty
      case (key, value) :: tail => ZipperContext(
        JNode(key, value),
        Stream.empty,
        fieldsToRightSiblings(tail),
        (leftSiblings, focus, rightSiblings) #:: ancestors
      )
    }
    case JArray(values)  => values.toList match {
      case Nil          => ZipperContext.Empty
      case head :: tail => ZipperContext(
        JNode(head),
        Stream.empty,
        valuesToRightSiblings(tail),
        (leftSiblings, focus, rightSiblings) #:: ancestors
      )
    }
    case value           => ZipperContext.Empty
  }

  /**
    * Maps the current [[ZipperContext]] to the leftwards.
    * @return the leftwised zipper context.
    */
  def left: ZipperContext = leftSiblings match {
    case head #:: tail => ZipperContext(head, tail, focus #:: rightSiblings, ancestors)
    case Stream.Empty  => ZipperContext.Empty
  }

  /**
    * Maps the current [[ZipperContext]] to the rightwards.
    * @return the rightwised zipper context.
    */
  def right: ZipperContext = rightSiblings match {
    case head #:: tail => ZipperContext(headm focus #:: leftSiblings, tail, ancestors)
    case Stream.Empty  => ZipperContext.Empty
  }
}

/**
  * This traint specifies the extending class represents JSON zipper context, especially the empty one.
  * @author Shingo OKAWA
  */
trait IsEmpty extends ZipperContext {
  /** Holds the currently handling node reference. */
  val focus: JNode = JNode.Empty

  /** Holds the currently handling node's left siblings' [[Stream]]. */
  val leftSiblings: ZipperContext.Siblings = Stream.Empty

  /** Holds the currently handling node's right siblings' [[Stream]]. */
  val rightSiblings: ZipperContext.Siblings = Stream.Empty

  /** Holds the currently handling node's parents' [[Stream]]. */
  val ancestors: ZipperContext.Ancestors = Stream.Empty

  /** {@inheritDoc} */
  override def toString = s"""IsEmpty(
                            | focus=$focus,
                            | leftSiblings=$leftSiblings,
                            | rightSiblings=$rightSiblings,
                            | ancestors=$ancestors )""".stripMargin.replaceAll("\n", " ")
}

/**
  * This traint specifies the extending class represents the JSON zipper context, especially the erroneous one.
  * @author Shingo OKAWA
  */
// TODO: Add XPath Implementation here to identify the erroneous location.
trait HasError extends ZipperContext {
  /** Holds the corresponding error. */
  val error: (String)

  /** Holds the currently handling node reference. */
  val focus: JNode = JNode.Error(error)

  /** Holds the currently handling node's left siblings' [[Stream]]. */
  val leftSiblings: ZipperContext.Siblings = Stream.Empty

  /** Holds the currently handling node's right siblings' [[Stream]]. */
  val rightSiblings: ZipperContext.Siblings = Stream.Empty

  /** Holds the currently handling node's parents' [[Stream]]. */
  val ancestors: ZipperContext.Ancestors = Stream.Empty

  /** {@inheritDoc} */
  override def toString = s"""HasError(
                            | focus=$this.focus,
                            | leftSiblings=$this.leftSiblings,
                            | rightSiblings=$this.rightSiblings,
                            | ancestors=$this.ancestors )""".stripMargin.replaceAll("\n", " ")
}

/**
  * This trait specifies the extending class has JSON zipper operations.
  * @author Shingo OKAWA
  */
trait WithIntermediateDataTypes {
  /** Aliases [[Stream[JNode]]] as zipper node [[Siblings]]. */
  type Siblings = Stream[JNode]

  /** Aliases [[(Siblings, Node, Siblings)]] as zipper node [[Ancestor]]. */
  type Ancestor = (Siblings, JNode, Siblings)

  /** Aliases [[Stream[Ancestor]]] as zipper node [[Ancestors]]. */
  type Ancestors = Stream[Ancestor]

  /**
    * Maps [[Seq[(String, JValue)]]] into right [[Siblings]].
    * @param  fields the JSON fields to be rendered.
    * @return the corresponding [[Stream[JNode]]] instance.
    */
  def fieldsToRightSiblings(fields: Seq[(String, JValue)]) = {
    def loop(fields: Seq[(String, JValue)]): Siblings = fields match {
      case Nil          => Stream.empty
      case head :: tail => JNode(head._1, head._2) #:: loop(tail)
    }
    loop(fields)
  }

  /**
    * Maps [[Seq[JValue]]] into right [[Siblings]].
    * @param  values the JSON values to be rendered.
    * @return the corresponding [[Stream[JNode]]] instance.
    */
  def valuesToRightSiblings(values: Seq[JValue]) = {
    def loop(values: Seq[JValue]): Siblings = values match {
      case Nil          => Stream.empty
      case head :: tail => JNode(head) #:: loop(tail)
    }
    loop(elements)
  }

  /**
    * Maps left [[Siblngs]] into [[JObject]].
    * @param  sibling the left siblings to be rendered.
    * @return the corresponding [[JObject]] instance.
    */
  def leftSiblingsToFields(siblings: Siblings): JObject = JObject(
    siblings.foldLeft(Seq[(String, JValue)]()) { (accumulator, sibling) => sibling match {
      case FieldJNode(key, value) => (key -> value) +: accumulator
      case _                      => accumulator
    }}
  )

  /**
    * Maps right [[Siblngs]] into [[JObject]].
    * @param  sibling the right siblings to be rendered.
    * @return the corresponding [[JObject]] instance.
    */
  def rightSiblingsToFields(siblings: Siblings): JObject = JObject(
    siblings.foldLeft(Seq[(String, JValue)]()) { (accumulator, sibling) => sibling match {
      case FieldJNode(key, value) => accumulator :+ (key -> value)
      case _                      => accumulator
    }}
  )

  /**
    * Maps left [[Siblngs]] into [[JArray]].
    * @param  sibling the left siblings to be rendered.
    * @return the corresponding [[JArray]] instance.
    */
  def leftSiblingsToValues(siblings: Siblings): JArray = JArray(
    siblings.foldLeft(Seq[JValue]()) { (accumulator, sibling) => sibling match {
      case ValueJNode(value) => value +: accumulator
      case _                 => accumulator
    }}
  )

  /**
    * Maps right [[Siblngs]] into [[JArray]].
    * @param  sibling the right siblings to be rendered.
    * @return the corresponding [[JArray]] instance.
    */
  def rightSiblingsToValues(siblings: Siblings): JArray = JArray(
    siblings.foldLeft(Seq[JValue]()) { (accumulator, sibling) => sibling match {
      case ValueJNode(value) => accumulator :+ value
      case _                 => accumulator
    }}
  )

  /**
    * Maps zipper context into [[JValue]].
    * @param  node the target node to be rendered.
    * @param  leftSiblings the target node's left siblings.
    * @param  rightSiblings the target node's right siblings.
    * @param  parent the target node's parent.
    * @return the corresponding [[JValue]] instance.
    */
  def contextToJSON(node: JNode, leftSiblings: Siblings, rightSiblings: Siblings, parent: JNode): JValue = (parent.value, node) match {
    case (JObject(_), FieldJNode(key, value)) => (leftSiblingsToFields(leftSiblings) :+ (key -> value)) ++ rightSiblingsToFields(rightSiblings)
    case (JArray(_), ValueJNode(value))       => (leftSiblingsToValues(leftSiblings) :+ value) ++ rightSiblingsToValues(rightSiblings)
    case _                                    => throw new RuntimeException("could not resolve zipper context.")
  }
}

/**
  * This object is responsible for providing basic functionalities of the context within JSON zipper modad.
  * @author Shingo OKAWA
  */
object ZipperContext extends WithIntermediateDataTypes {
  /** The empty context terminal object. */
  case object Empty extends IsEmpty

  /** Maps to the erroneous context. */
  // TODO: Add XPath Implementation here to identify the erroneous location.
  case class Error(override val error: (String)) extends HasError

  /**
    * Maps [[JValue]] into [[ZipperContext]].
    * @param  jValue the [[JValue]] which will be focused.
    * @return the corresponding [[ZipperContext]] instance which is not connected with any others.
    */
  def apply(jValue: JValue) = new ZipperContext {
    val focus = JNode(jValue)
    val leftSiblings = Stream.empty
    val rightSiblings = Stream.empty
    val ancestors = Stream.empty
  }

  /**
    * Maps [[JNode]] into [[ZipperContext]].
    * @param  focus the focusing node.
    * @param  leftSiblings the associating left siblings' stream.
    * @param  rightSiblings the associating right siblings' stream.
    * @param  ancestors the associating ancestors' stream.
    * @return the corresponding [[ZipperContext]] which is contextized with the given values.
    */
  def apply(focus: JNode, leftSiblings: Siblings, rightSiblings: Siblings, ancestors: Ancestors) = new ZipperContext {
    val focus = focus
    val leftSiblings = leftSiblings
    val rightSiblings = rightSiblings
    val ancestors = ancestors
  }

  /**
    * Maps [[ZipperContext]] into [[JNode]].
    * @param  context the [[ZipperContext]] to be unwrapped.
    * @return the corresponding [[JNode]] instance.
    */
  def unapply(context: ZipperContext): Option[(JNode, Siblings, Siblings, Ancestors)] =
    Some((context.focus, context.leftSiblings, context.rightSiblings, context.ancestors))
}
