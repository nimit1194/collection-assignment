package com.knoldus.kip.service

import com.knoldus.kip.RamDatabase
import com.knoldus.kip.models.{Gender, ScoreCard, Student}
import RamDatabase._

trait CollectionAssignment {


  def getScorecardsByName(name: String): List[ScoreCard] = {

    val map = generateScorecards

    map(name) match {
      case x: ScoreCard => List(x)
      case x: List[ScoreCard] => x.sortBy(_.studentId)
    }
  }

  //Collection Based - Assignment 1

  def computeMarks(id: Long): Map[Long, Float] = {
    val marks = marksList.filter(id == _.studentId)

    marks.map(x => x.subjectId.toLong -> x.marksObtained).toMap[Long, Float]
  }

  def generateScorecards: Map[String, AnyRef] = {

    def computeMarks(id: Long): Map[Long, Float] = {
      val marks = marksList.filter(id == _.studentId)

      marks.map(x => x.subjectId.toLong -> x.marksObtained).toMap[Long, Float]
    }

  }

  def computePercentage(id: Long) = {
    val marks: List[Float] = marksList.filter(id == _.studentId).map(_.marksObtained)
    marks.sum / marks.length
  }

  def computeScoreCard(student: List[Student]): List[ScoreCard] = {
    for (s <- student)
      yield new ScoreCard(s.id, computeMarks(s.id), computePercentage(s.id))
  }

  def compute(list: List[Student], map: Map[String, AnyRef]): Map[String, AnyRef] = {

    if (list.isEmpty) map

    else if (!list.tail.exists(_.name == list.head.name)) {

      val newmap = map + (list.head.name ->
        ScoreCard(list.head.id, computeMarks(list.head.id), computePercentage(list.head.id)))
      compute(list.tail, newmap)

    }
    else {

      val newmap = map + (list.head.name -> computeScoreCard(list.head :: list.tail.filter(_.name == list.head.name)))
      compute(list.tail.filter(_.name != list.head.name), newmap)

    }
    compute(studentList, Map[String, AnyRef]())

  }

  //Collection Based - Assignment 2
  def getScoreCardByGender: (List[ScoreCard], List[ScoreCard]) = {

    val (mStud: List[Student], fStud: List[Student]) = RamDatabase.studentList.partition(_.gender == Gender.MALE)

    val malescorecards = mStud.map(_.name).distinct.flatMap(x => getScorecardsByName(x))

    val femalescorecards = fStud.map(_.name).distinct.flatMap(x => getScorecardsByName(x))

    (malescorecards, femalescorecards)
  }

  def getScoreCardsWithHigherPercentage: (List[ScoreCard], List[ScoreCard]) = {

    val (mScoreCard, fScoreCard) = getScoreCardByGender

    (mScoreCard.filter(_.percentage >= 50), fScoreCard.filter(_.percentage >= 50))

  }

  //Internally calls getScoreCardByGender
  def getSimilarPercentageBwGroups: List[((String, ScoreCard), (String, ScoreCard))] = {

    def getStudentName(st_Id: Long): String = studentList.find(_.id == st_Id).get.name

    val (mScoreCard, fScoreCard) = getScoreCardByGender

    mScoreCard.flatMap(x => fScoreCard.filter(_.percentage == x.percentage).map(y =>
      ((getStudentName(x.studentId), x), (getStudentName(y.studentId), y))))
  }

  def femalePercentageNotInMales: List[(String, ScoreCard)] = {
    def getStudentName(st_Id: Long): String = studentList.find(_.id == st_Id).get.name

    val (mScoreCard, fScoreCard) = getScoreCardByGender


    for (fs <- fScoreCard if !mScoreCard.exists(_.percentage == fs.percentage))
      yield (getStudentName(fs.studentId), fs)
  }

}
