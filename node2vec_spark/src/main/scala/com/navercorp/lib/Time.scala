/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.navercorp.lib

import org.joda.time._
import org.joda.time.format.DateTimeFormat
import org.joda.time.format.DateTimeFormatter

object Time extends Serializable{
  val PT_FORMAT = "yyyyMMdd"

  val alpha = alphaForHalf(24 * 7) //一周半衰期

  val alpha_1_day = alphaForHalf(18)

  def decay(ts: String): Double = {
    val hours = Hours.hoursBetween(dtP1.parseDateTime(ts), DateTime.now).getHours
    1.0 / Math.exp(alpha_1_day * hours)
  }

  def decay(interval:Int):Double = {
    1.0 / Math.exp(interval * alpha)
  }

  def decay(interval:Int, alpha:Double):Double = {
    1.0 / Math.exp(interval * alpha)
  }

  def decay(origin:Double, interval:Int):Double = {
    origin / Math.exp(interval * alpha)
  }

  def alphaForHalf(num: Int): Double = {
    Math.log(2.0) / num
  }

  val dtP1 = DateTimeFormat.forPattern("yyyyMMddHHmmss")
  val dtP2 = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss")
  val dtP3 = DateTimeFormat.forPattern("yyyy-MM-dd")
  val dtP4 = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss.SSS")

  def week(time:String, dtp:DateTimeFormatter): Int = {
    try {
      val t = dtp.parseDateTime(time)

      Weeks.weeksBetween(t, DateTime.now()).getWeeks
    } catch {
      case ex:Exception => 1
    }
  }


  def dayDtP3(time:String): Int = {
    val t = dtP3.parseDateTime(time)
    Days.daysBetween(t, DateTime.now()).getDays
  }


  def day(time:String, dtp:DateTimeFormatter): Int = {
    val t = dtp.parseDateTime(time)

    Days.daysBetween(t,DateTime.now()).getDays
  }

  def day(time:Long, dtp:DateTimeFormatter): Int = {
    val t = new DateTime(time)

    Days.daysBetween(t,DateTime.now()).getDays
  }


  def hour(time:String, dtp:DateTimeFormatter): Int = {
    val t = dtp.parseDateTime(time)

    Hours.hoursBetween(t,DateTime.now()).getHours
  }

  def minute(time:String, dtp:DateTimeFormatter) :Int = {
    val t = dtp.parseDateTime(time)

    Minutes.minutesBetween(t, DateTime.now()).getMinutes
  }

  def somePt(n:Int):String =  DateTime.now().plusDays(n).toString(PT_FORMAT)

  //  def score(vt: Double, td: Double, ts: String): Double = {
  //    var star = .0
  //    val alpha = 0.5
  //    val beta = 0.5
  //    if (vt <= 5) star = 0
  //    else {
  //      val wScore = Math.pow(vt, 1.1) / 35
  //      var pScore = .0
  //      pScore = Math.min(vt / td, 1)
  //      star = alpha * pScore + beta * wScore
  //    }
  //
  //    val interestRatio = decay(ts)
  //    star * interestRatio
  //  }

  def score(vt: Double, td: Double, ts: String): Double = {
    var star = 0.0
    if (vt <= 7) star = 0.01
    else if (vt < 15) star = 1
    else if (vt < 30) star = 1.5
    else star = 2

    val interestRatio = decay(ts)
    star * interestRatio
  }


  def main(args: Array[String]): Unit = {
    println(alphaForHalf(24 * 8))
  }
}
