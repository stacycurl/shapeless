/*
 * Copyright (c) 2013 Miles Sabin 
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package shapeless
package ops

import poly._

object unified {
  trait Nil[A]

  object Nil {
    def apply[A](implicit nil: Nil[A]): Nil[A] = nil

    implicit val coproductNil: Nil[CNil] = new Nil[CNil] {}
    implicit val hlistNil: Nil[HNil] = new Nil[HNil] {}
    implicit val tupleNil: Nil[Unit] = new Nil[Unit] {}
  }

/*
  trait Cons[A, H, T <: A] {
    type Out <: A
  }

  object Cons {
    def apply[A, H, T <: A]
      (implicit cons: Cons[A, H, T]): Aux[A, H, T, cons.Out] = cons

    type Aux[A, +H, +T <: A, +Out0 <: A] = Cons[A, H, T] { type Out = Out0 }

    implicit def coproductCons[H, T <: Coproduct]: Aux[Coproduct, H, T, H :+: T] =
      new Cons[Coproduct, H, T] {
        type Out = H :+: T
      }

    implicit def hlistCons[H, T <: HList]: Aux[HList, H, T, H :: T] = new Cons[HList, H, T] {
      type Out = H :: T
    }
  }

  trait IsCons[A, U <: A] {
    type H
    type T <: A
  }

  object IsCons {
    def apply[A, U <: A](implicit isCons: IsCons[A, U]): Aux[A, U, isCons.H, isCons.T] = isCons

    type Aux[A, U <: A, H0, T0 <: A] = IsCons[A, U] { type H = H0; type T = T0 }

    implicit def coproductIsCons[H0, T0 <: Coproduct]: Aux[Coproduct, H0 :+: T0, H0, T0] =
      new IsCons[Coproduct, H0 :+: T0] {
        type H = H0
        type T = T0
      }

   implicit def hlistIsCons[H0, T0 <: HList]: Aux[HList, H0 :: T0, H0, T0] =
     new IsCons[HList, H0 :: T0] {
       type H = H0
       type T = T0
     }
  }
  */

  trait UnpackCons[PP] {
    type F[_, _]
    type H
    type T
  }

  object UnpackCons {
    def apply[PP](implicit unpackCons: UnpackCons[PP])
      : Aux[PP, unpackCons.F, unpackCons.H, unpackCons.T] = unpackCons

    type Aux[PP, F0[_, _], H0, T0] = UnpackCons[PP] {
      type F[H, T] = F0[H, T]
      type H = H0
      type T = T0
    }

/*
    implicit def unpackCoproduct[H0, T0 <: Coproduct]: Aux[H0 :+: T0, :+:, H0, T0] =
      new UnpackCons[H0 :+: T0] {
        type F[H, T] = H :+: T
        type H = H0
        type T = T0
      }

    implicit def unpackHList[H0, T0 <: HList]: Aux[H0 :: T0, ::, H0, T0] =
      new UnpackCons[H0 :: T0] {
        type F[H, T] = shapeless.::[H, T]
        type H = H0
        type T = T0
      }
*/

    implicit def unpackTuple2[H0, T0]: Aux[(H0, T0), Tuple2, H0, T0] =
      new UnpackCons[(H0, T0)] {
        type F[H, T] = (H, T)
        type H = H0
        type T = T0
      }
  }

  trait LengthU[U] extends DepFn0 { type Out <: Nat }

  object LengthU {
    def apply[U](implicit lengthU: LengthU[U]): Aux[U, lengthU.Out] = lengthU

    type Aux[U, Out0 <: Nat] = LengthU[U] { type Out = Out0 }

    implicit def nilLength[U](implicit nil: Nil[U]): Aux[U, Nat._0] =
      new LengthU[U] {
        type Out = Nat._0

        def apply(): Out = Nat._0
      }

    implicit def consLength[HT, C[_, _], H, T, N <: Nat](
      implicit
        cons: UnpackCons.Aux[HT, C, H, T]
        , length: Aux[T, N]
        , sn: Witness.Aux[Succ[N]]
    ): Aux[HT, Succ[N]] = new LengthU[HT] {
    //): Aux[HT, Nat._0] = new LengthU[HT] {
      type Out = Succ[N]

      def apply(): Out = sn.value
    }
  }




/*
  trait Something[A] {
    type Nil <: A
    type Cons[H, T <: A] <: A
  }

  object Something {
    def apply[A]
      (implicit something: Something[A]): Aux[A, something.Nil, something.Cons] = something

    type Aux[A, Nil0 <: A, Cons0[H, T <: A] <: A] = Something[A] {
      type Nil = Nil0
      type Cons[H, T <: A] = Cons0[H, T]
    }

    implicit def coproductSomething: Aux[Coproduct, CNil, :+:] = new Something[Coproduct] {
      type Nil = CNil
      type Cons[H, T <: Coproduct] = H :+: T
    }

    implicit def productSomething: Aux[HList, HNil, ::] = new Something[HList] {
      type Nil = HNil
      type Cons[H, T <: HList] = H :: T
    }
  }

    implicit def consLength[A, H, T <: A, HT <: A, TLength <: Nat](
      implicit
        cons: Cons.Aux[A, H, T, HT],
        length: Aux[A, T, TLength],
        sn: Witness.Aux[Succ[TLength]]
    ): Aux[A, HT, Succ[TLength]] = new LengthU[A, HT] {
      type Out = Succ[TLength]

      def apply(): Out = sn.value
    }
    */

/*
    implicit def nilLength[Nil0 <: SMC, SMC <: { type Nil = Nil0; type Cons[H, T <: SMC] <: SMC }]
      : Aux[SMC, Nil0, Nat._0] =
      new LengthU[SMC, Nil0] {
        type Out = Nat._0

        def apply(): Out = Nat._0
      }

    implicit def consLength[SMC, H, T <: SMC, N <: Nat](
      implicit something: Something[SMC], lt: Aux[SMC, T, N], sn: Witness.Aux[Succ[N]]
    ): Aux[SMC, something.Cons[H, T], Succ[N]] = new LengthU[SMC, something.Cons[H, T]] {
      type Out = Succ[N]

      def apply(): Out = sn.value
    }
    */

  implicit class UnifiedOps[A](a: A) {
    def lengthU(implicit length: LengthU[A]): length.Out = length()
  }

  def testLength {
    def assertTypedEquals[A](expected: A, actual: A): Unit =
      if (expected != actual) sys.error(s"$expected != $actual")

    val coproductNil = Nil.apply[CNil]
    val hlistNil     = Nil.apply[HNil]

    val cnl = LengthU.nilLength[CNil]
    assertTypedEquals[Nat._0](Nat._0, cnl.apply())

    val hnl = LengthU.nilLength[HNil]
    assertTypedEquals[Nat._0](Nat._0, hnl.apply())


/*
    assertTypedEquals[Nat._0](Nat._0, LengthU.apply[Coproduct, CNil].apply())
    assertTypedEquals[Nat._0](Nat._0, LengthU.apply[HList, HNil].apply())

    assertTypedEquals[Nat._0](Nat._0, (HNil: HNil).lengthU)
    */

    //val uncons = UnpackCons.unpack[Coproduct, :+:, Int, CNil]
    val w1 = implicitly[Witness.Aux[Succ[Nat._0]]]

    //val ccons = LengthU.consLength[Coproduct, Int :+: CNil, Int, CNil, Nat._0, :+:] /*(
    //val ccons = LengthU.consLength[Int :+: CNil, :+:, Int, CNil]
    /*(
      implicitly[UnpackCons[Coproduct, Int :+: CNil, :+:, Int, CNil]],
      //UnpackCons.unpack[Coproduct, :+:, Int, CNil],
      implicitly[LengthU.Aux[Coproduct, CNil, Nat._0]],
      Witness.witnessN[_0]
    )*/

//    assertTypedEquals[Nat._1](Nat._1, ccons.apply())

    //val hcons = LengthU.consLength[HList, Int :: HNil, Int, HNil, Nat._0, ::] /*(
    //val hcons = LengthU.consLength[Int :: HNil, ::, Int, HNil]
    /*(
      implicitly[UnpackCons[HList, Int :: HNil, ::, Int, HNil]],
      //UnpackCons.unpack[HList, ::, Int, HNil],
      implicitly[LengthU.Aux[HList, HNil, Nat._0]],
      Witness.witnessN[_0]
    )*/

 //   assertTypedEquals[Nat._1](Nat._1, hcons.apply())

    //assertTypedEquals[Nat._1](Nat._1, LengthU.apply[Coproduct, Int :+: CNil](ccons).apply())
    //assertTypedEquals[Nat._1](Nat._1, LengthU.apply[HList, Int :: HNil](hcons).apply())

    //UnpackCons.apply[Int :: HNil]
    UnpackCons.apply[(Int, Unit)]
    UnpackCons.unpackTuple2[Int, Unit]

    LengthU.consLength[(Int, Unit), Tuple2, Int, Unit, Nat._0]

    val tlength = LengthU.apply[(Int, Unit)]

    assertTypedEquals[Nat._1](Nat._1, (1, ()).lengthU)

    assertTypedEquals[Nat._2](Nat._2, (1, ("foo", ())).lengthU)
//    LengthU.apply[Int :: HNil]
    //LengthU.apply[Coproduct, Int :+: CNil]

    //assertTypedEquals[Nat._1](Nat._1, (1 :: HNil).lengthU)
    //assertTypedEquals[Nat._1](Nat._1, Coproduct[Int :+: CNil](1).lengthU)
    /*
    assertTypedEquals[Nat._1](Nat._1, Coproduct[Int :+: CNil](123).length)
    assertTypedEquals[Nat._2](Nat._2, Coproduct[Int :+: String :+: CNil](123).length)
    assertTypedEquals[Nat._3](Nat._3, Coproduct[Int :+: String :+: Double :+: CNil](123).length)
    assertTypedEquals[Nat._4](Nat._4, Coproduct[Int :+: String :+: Double :+: Char :+: CNil](123).length)
    */
  }
}

