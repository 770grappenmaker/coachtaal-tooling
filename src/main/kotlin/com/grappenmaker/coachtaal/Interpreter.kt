package com.grappenmaker.coachtaal

class Interpreter(val parsed: List<Expr>) {
    var ptr = 0
    val memory = mutableMapOf<String, Float>()

    fun call(name: String, arguments: List<Float>): Float? = TODO()
}