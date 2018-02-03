package com.peschke.advent_of_code

import cats.data.{Validated, NonEmptyList}

package object day8 {
  type Error = String
  type ErrorOr[T] = Either[Error,T]
  type CompileErrors = NonEmptyList[Error]
  type CompileErrorsOr[T] = Validated[CompileErrors,T]
}
