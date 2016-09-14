  def setTSDPParameter[T](est: T, confAsl: String): T = {
    import com.spss.qa.ac.function._
    import com.ibm.spss.ml.common.params._
    import com.ibm.spss.ml.forecasting.params._
    import com.ibm.spss.ml.datapreparation.params._
    val r = JsonUtil.toJobject(confAsl)
    val f = r.getFields.iterator()
    var estimator = est
    val methods = est.getClass.getMethods

    while (f.hasNext()) {
      val key = f.next()
      //println("K:" + key)
      val temp = key.substring(1, key.length())
      val t = JsonUtil.makeParametersForTS(key, r)

      for (loop <- methods) {
        if (loop.getName == ("set" + key(0).toUpper + temp)) {
          t match {
            case x: Array[Int] =>
              println("Array(Int)")
              loop.invoke(estimator, x)
            case x: Array[Double] =>
              println("Array(Double)")
              loop.invoke(estimator, x)
            case x: Array[Boolean] =>
              println("Array(Boolean)")
              loop.invoke(estimator, x)
            case x: Array[String] =>
              println("Array(String)")
              if (x.length > 0) {
                loop.invoke(estimator, x)
              } else {
                if(key=="seriesFilterList"){
                  loop.invoke(estimator, List[List[SeriesFilter]]())
                }else{
                  loop.invoke(estimator, Array[String]())
                }

              }
            case y: ArrayList[ArrayList[(String, Any)]] =>
              println("ArrayList[ArrayList(String, Any)]")
              if (key == "groupType") {
                val y_t = y.asInstanceOf[ArrayList[(String, Any)]]
                println("set groupType" + y_t.size())
                var temp = List[(String, String)]()
                for (loop2 <- 0 until y_t.size()) {
                  val item = y_t.get(loop2)
                  temp = temp :+ (item._1, String.valueOf(item._2))

                }
                val value = GroupType(temp)
                loop.invoke(estimator, value)
              } else if (key == "aggregationType") {
                println("set aggregationType")
                val y_t = y.asInstanceOf[ArrayList[(String, Any)]]
                var temp = List[(String, String)]()
                for (loop2 <- 0 until y_t.size()) {
                  val item = y_t.get(loop2)
                  temp = temp :+ (item._1, String.valueOf(item._2))

                }
                val value = AggregationType(temp)
                loop.invoke(estimator, value)
              } else if (key == "distributionType") {
                println("set distributionType")
                val y_t = y.asInstanceOf[ArrayList[(String, Any)]]
                var temp = List[(String, String)]()
                for (loop2 <- 0 until y_t.size()) {
                  val item = y_t.get(loop2)
                  temp = temp :+ (item._1, String.valueOf(item._2))

                }
                val value = DistributionType(temp)
                loop.invoke(estimator, value)

              } else if (key == "seriesFilterList") {

                var dimension: Option[String] = None
                var values: List[String] = Nil
                var excludes: List[String] = Nil
                var metrics: List[String] = Nil
                var excludeMetrics: List[String] = Nil
                var seFilter: List[List[SeriesFilter]] = List[List[SeriesFilter]]()
                var tempseFilter = List[SeriesFilter]()

                println("set seriesFilterList:" + y.size())
                for (loop2 <- 0 until y.size()) {
                  val item = y.get(loop2)
                  println("xxx" + item.size)
                  for (loop3 <- 0 until item.size()) {
                    val temp = item.get(loop3)
                    println("temp:" + temp)
                    temp._1 match {
                      case "dimension" =>
                        println("set dimension")
                        dimension = Some(String.valueOf(temp._2))
                      case "values" =>
                        println("set values")
                        values = temp._2.asInstanceOf[Array[String]].toList
                      case "excludes" =>
                        println("set excludes")
                        excludes = temp._2.asInstanceOf[Array[String]].toList
                      case "metrics" =>
                        println("set metrics")
                        metrics = temp._2.asInstanceOf[Array[String]].toList
                      case "excludeMetrics" =>
                        println("set excludeMetrics")
                      case _ => println("Others")

                    }

                  }
                  val se = SeriesFilter(dimension, values, excludes, metrics, excludeMetrics)
                  tempseFilter = tempseFilter :+ se
                  seFilter = seFilter :+ tempseFilter
                }
                loop.invoke(estimator, seFilter)
              } else if (key == "inputFieldListForTarget") {
                var inputFieldListForTarget = List[InputFieldListForTarget]()
                println("set inputFieldListForTarget:" + y.size())
                for (loop2 <- 0 until y.size()) {
                  var targetName: String = ""
                  var inputFieldList = List[String]()
                  val item = y.get(loop2)
                  println("xxx" + item.size)
                  for (loop3 <- 0 until item.size()) {
                    val temp = item.get(loop3)
                    println("temp:" + temp)
                    temp._1 match {
                      case "targetName" =>
                        println("set targetName")
                        targetName = String.valueOf(temp._2)
                      case "inputFieldList" =>
                        println("set inputFieldList")
                        inputFieldList = temp._2.asInstanceOf[Array[String]].toList
                      case _ => println("Others")

                    }

                  }

                  val ift = InputFieldListForTarget(targetName: String, inputFieldList: List[String])
                  inputFieldListForTarget = inputFieldListForTarget :+ ift
                }
                loop.invoke(estimator, inputFieldListForTarget)

              } else if (key == "customerPairs") {
                println("set customerPairs")
                var value = List[(String, String)]()
                for (lop <- 0 until y.size()) {
                  val temp = y.get(lop)
                  value = value :+ (String.valueOf(temp.get(0)._2), String.valueOf(temp.get(1)._2))
                }
                println(value)
                loop.invoke(estimator, value)
              }
            case x: Int     => loop.invoke(estimator, x.asInstanceOf[Object])
            case x: String  => loop.invoke(estimator, x.asInstanceOf[Object])
            case x: Boolean => loop.invoke(estimator, x.asInstanceOf[Object])
            case x: Double  => loop.invoke(estimator, x.asInstanceOf[Object])
            case _:Throwable =>
              println("Dont support:" + key + "," + t)
          }

        }
      }
    }

    estimator
  }