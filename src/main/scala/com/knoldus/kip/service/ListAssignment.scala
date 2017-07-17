package com.knoldus.kip.service

import com.knoldus.kip.models.{Marks, Student}
import com.knoldus.kip.RamDatabase.marksList
import com.knoldus.kip.RamDatabase.studentList

trait ListAssignment {

  //Assignment -1
  def failedStudents(subjectId: Long, percentage: Double, passOrFail: String): Int = {

    val pass: Int = marksList.count(a => (a.subjectId == subjectId && a.marksObtained >= percentage))

    val fail: Int = marksList.count(a => (a.subjectId == subjectId && a.marksObtained < percentage))

    passOrFail.toLowerCase match {

      case "pass" => pass

      case "fail" => fail
    }

  }

  def topBottomStudents(subjectId: Long, count: Int, topOrBottom: String): List[Student] = {

    val topperList = marksList.filter(_.subjectId == subjectId).sortBy(_.marksObtained).takeRight(count)

    val bottomList = marksList.filter(_.subjectId == subjectId).sortBy(_.marksObtained).take(count)

    topOrBottom.toLowerCase match {

      case "top" => studentList.filter(_.id == topperList.map(_.studentId))

      case "bottom" => studentList.filter(_.id == bottomList.map(_.studentId))

    }
  }


  def topAndLeastScorers(topOrBottom: String, count: Int): List[Student] = {

    topOrBottom match {
      case "top" => {
        val studentIdWithMarks = studentList.map(_.id).
          zip(studentList.map(x => marksList.filter(x.id == _.studentId).
            map(_.marksObtained).sum)).sortBy(_._2).reverse
        val topStudents = studentIdWithMarks.take(count)
        val topStudentIds = topStudents.map(_._1)
        studentList.filter(topStudentIds contains _.id)
      }
      case "bottom" => {
        val studentIdWithMarks = studentList.map(_.id).
          zip(studentList.map(x => marksList.filter(x.id == _.studentId).
            map(_.marksObtained).sum)).sortBy(_._2)
        val bottomStudents = studentIdWithMarks.take(count)
        val bottomStudentIds = bottomStudents.map(_._1)
        studentList.filter(bottomStudentIds contains _.id)
      }
    }
  }

  def getScholarshipGroups(percentage: Float, goodScholarship: Int, normalScholarship: Int)
  : (List[(Student, Int)], List[(Student, Int)]) = {
    val inputPercentage = percentage * 5
    val studentIdWithMarks = studentList.map(_.id).
      zip(studentList.map(x => marksList.filter(x.id == _.studentId).
        map(_.marksObtained).sum))
    val (studentIdWithGoodScholarship, studentIdWithNormalScholarship) = studentIdWithMarks.
      partition(_._2 >= inputPercentage)
    val studentWithGoodScholarship = studentList.filter(studentIdWithGoodScholarship.
      map(_._1) contains _.id)
    val studentWithNormalScholarship = studentList.filter(studentIdWithNormalScholarship.
      map(_._1) contains _.id)
    val listOfGoodScholarship = List.fill(studentWithGoodScholarship.length)(goodScholarship)
    val listOfNormalScholarship = List.fill(studentWithNormalScholarship.length)(normalScholarship)
    val zippedListGoodScholarship = studentWithGoodScholarship zip listOfGoodScholarship
    val zippedListNormalScholarship = studentWithNormalScholarship zip listOfNormalScholarship
    (zippedListGoodScholarship, zippedListNormalScholarship)
  }

  def passedOrFailed(passOrFail: String, percentage: Float): List[Student] = {

    val studentIdWithMarks = studentList.map(_.id).
      zip(studentList.map(x => marksList.filter(x.id == _.studentId).
        map(_.marksObtained).sum))
    val inputPercentage = percentage * 5

    passOrFail match {
      case "pass" => {
        val filteredStudentWithMarks = studentIdWithMarks.filter(_._2 >= inputPercentage)
        studentList.filter(filteredStudentWithMarks.map(_._1) contains _.id)
      }
      case "fail" => {
        val filteredStudentWithMarks = studentIdWithMarks.filter(_._2 < inputPercentage)
        studentList.filter(filteredStudentWithMarks.map(_._1) contains _.id)
      }
    }

  }

  def studentsWithMoreThan95: List[Student] = {
    val studentIdWithMarks = studentList.map(_.id).
      zip(studentList.map(x => marksList.filter(x.id == _.studentId).
        map(_.marksObtained).sum))
    val filteredStudentWithMarks = studentIdWithMarks.filter(_._2 >= 475)
    studentList.filter(filteredStudentWithMarks.map(_._1) contains _.id)

  }

  def generateReport: List[(String, List[Int])] = {
    val listOfListOfMarks = marksList.groupBy(_.studentId).values.map(_.map(_.marksObtained))
    val setOfKeys = marksList.groupBy(_.studentId).mapValues(_.map(_.marksObtained)).keys
    val listOfListOfNames = for {l <- setOfKeys.toList
                                 listOfIndividualNames = studentList.filter(_.id == l).map(_.name)
    } yield listOfIndividualNames
    val listOfNames = listOfListOfNames.flatten
    listOfNames zip listOfListOfMarks
  }

  //Assignment - 2
  def getLastElementWithIndex(list: List[String]): (String, Int) = {
    def lastElementWithIndex(list: List[String], str: String, index: Int): (String, Int) = {
      if (list.isEmpty) {
        (str, index)
      }
      else {
        val str1 = list.head
        lastElementWithIndex(list.tail, str1, index + 1)
      }
    }

    lastElementWithIndex(list, str = "", index = 0)

  }

  def printTable(list: List[Long]): List[Long] = {
    val x = 1 to 10 toList

    list.flatMap(y => x.map(_ * y))
  }


  def aggregateLists(list1: List[String], list2: List[Long]): List[List[(String, Long)]] = {

    val zipList = list1 zip list2
    zipList.map(t => List[(String, Long)]((t._1, t._2)))
  }

  def getSumOfList(list: List[Long]): Long = {

    if (list.isEmpty) 0
    else
      list.head + getSumOfList(list.tail)
  }

  def getMultiplicationOfList(list: List[Long]): Long = {
    if (list.isEmpty) 1
    else
      list.head * getMultiplicationOfList(list.tail)
  }

  def quickSortList(list: List[Long]): List[Long] = {
    if (list.length <= 1) list
    else {
      val pivot = list(list.length / 2)
      quickSortList(list filter (_ < pivot)) ::: (list filter (_ == pivot)) ::: quickSortList(list filter (_ > pivot))
    }
  }


  def mergeSortList(list: List[Long]): List[Long] = {

    def merge(left: List[Long], right: List[Long]): List[Long] =
      (left, right) match {
        case (left, Nil) => left
        case (Nil, right) => right
        case (leftHead :: leftTail, rightHead :: rightTail) => {
          if (leftHead < rightHead)
            {leftHead :: merge(leftTail, right)
          else rightHead :: merge(left, rightTail)
        }
      }

    def mergeSort(list: List[Long]): List[Long] = {
      val n = list.length / 2
      if (n == 0) {
        list
      } // i.e. if list is empty or single value, no sorting needed
      else {
        val (left, right) = list.splitAt(n)
        merge(mergeSort(left), mergeSort(right))
      }
    }

    mergeSort(list)

  }

}
