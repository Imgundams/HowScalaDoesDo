/*
Created by DRain on 18/07/2017.
*/
name := "HowScalaDoesDo"

version := "1.0"

scalaVersion := "2.12.1"

wartremoverErrors ++= Warts.unsafe
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
