object Q2 extends App {
    def validateInput(name: String, marks: Int, totalMarks: Int): (Boolean, Option[String]) = {
        if (name.isEmpty || !name.matches("^[a-zA-Z\\s]+$")) (false, Some("Name cannot be empty and can only contain alphabets"))
        else if (marks <= 0 || marks >= totalMarks) (false, Some("Marks have to be zero or positive and less than or equal to total marks"))
        else if (totalMarks < 0) (false, Some("Total makrs cannot be negative"))
        else (true, None)
    }

    def getStudentInfo(): (String, Int, Int, Double, Char) = {
    var name = ""
    var marks = -1
    var totPossibleMarks = -1       
    
    print("Enter student name: ")
    name = scala.io.StdIn.readLine().trim().replaceAll("\\s+", " ")            

    print("Enter total possible marks: ")
    totPossibleMarks = scala.io.StdIn.readInt()

    print("Enter student marks: ")
    marks = scala.io.StdIn.readInt()

    val percentage = (marks * 100.0) / totPossibleMarks


    val grade = percentage match {
        case p if p >= 90 => 'A'
        case p if p >= 75 => 'B'
        case p if p >= 50 => 'C'
        case _ => 'D'
    }

    printStudentRecord(name, totPossibleMarks, marks, percentage, grade)

    val (isValid, error) = validateInput(name, marks, totPossibleMarks)
    if (!isValid) {
        error.foreach(println)
        return ("", -1, -1, 0.0, 'D')
    }

    

    (name, totPossibleMarks, marks, percentage, grade)
}


    def printStudentRecord(student: (String, Int, Int, Double, Char)): Unit = {
        println(s"----------Student Info----------")
        println(s"Name: ${student._1}")
        println(s"Total Possible Marks: ${student._2}")
        println(s"Marks Obtained: ${student._3}")
        println(f"Percentage: ${student._4}%.2f%%") 
        println(s"Grade: ${student._5}")
    }

    def getStudentInfoWithRetry(): (String, Int, Int, Double, Char) = {
        var isValid = false
        var studentInfo: (String, Int, Int, Double, Char) = ("", 0, 0, 0.0, 'F')

        while(!isValid) {
            studentInfo = getStudentInfo()
            val(name, totPossibleMarks, marks, _, _) = studentInfo
            val(valid, error) = validateInput(name, marks, totPossibleMarks)

            if(valid) isValid = true
            else error.foreach(println)
        }

        studentInfo
    }




    getStudentInfoWithRetry()
}
