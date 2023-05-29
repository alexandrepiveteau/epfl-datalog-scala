package io.github.alexandrepiveteau.datalog.core

case class NoStratificationException() extends IllegalArgumentException("This program can not be stratified.")
