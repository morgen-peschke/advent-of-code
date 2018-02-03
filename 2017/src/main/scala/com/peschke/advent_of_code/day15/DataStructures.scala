package com.peschke.advent_of_code
package day15

case class Generator(name: String, seed: BigInt, factor: BigInt)

case class GeneratorWithFilter(base: Generator, filterMod: BigInt)

case class Judge(a: () => Iterator[BigInt], b: () => Iterator[BigInt], mask: BigInt)
