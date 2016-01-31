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
    * Maps the current [[ZipperContext]] to the downwords.
    * @return the downwised zipper context.
    */
  def down: ZipperContext = focus.value match {
    case JObject(fields) => fields.toList match {
      case Nil                  => ZipperContext.Empty
      case (key, value) :: tail => ZipperContext(
        JNode(key, value),
        Stream.empty,

      )
    }
    case jArray(values)  => values.toList match {
    }
    case value           => 
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
trait ZipperOperations {
  // PLACEHOLDER.
}

/**
  * This object is responsible for providing basic functionalities of the context within JSON zipper modad.
  * @author Shingo OKAWA
  */
object ZipperContext extends ZipperOperations {
  /** Aliases [[Stream[JNode]]] as zipper node [[Siblings]]. */
  type Siblings = Stream[JNode]

  /** Aliases [[(Siblings, Node, Siblings)]] as zipper node [[Ancestor]]. */
  type Ancestor = (Siblings, JNode, Siblings)

  /** Aliases [[Stream[Ancestor]]] as zipper node [[Ancestors]]. */
  type Ancestors = Stream[Ancestor]

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

  /**
    * Maps [[(String, JValue)]] into [[JNode]].
    * @param  context the [[ZipperContext]] to be unwrapped.
    * @return the corresponding [[Stream[JNode]]] instance.
    */
  def objectsToStream(objects: Seq[(String, JValue)]) = {
    def loop(objects: Seq[(String, JValue)]): Siblings = objects match {
      case Nil          => Stream.empty
      case head :: tail => JNode(head._1, head._2) #:: loop(tail)
    }
    loop(elements)
  }
}
