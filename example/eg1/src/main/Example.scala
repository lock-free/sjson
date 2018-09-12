package io.github.shopee.idata.json.example

import io.github.shopee.idata.sjson.JSON

object Main {
  def main(args: Array[String]) {
    println(JSON.stringify(List(1,2,3)))
  }
}
