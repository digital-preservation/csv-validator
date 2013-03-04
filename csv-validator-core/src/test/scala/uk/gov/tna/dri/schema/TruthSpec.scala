package uk.gov.tna.dri.schema

import org.specs2.mutable.Specification
import org.specs2.matcher.DataTables

class TruthSpec extends Specification with DataTables {

  "logic" should {

    "work for a OR b AND c" in {

           "a"  |  "b"  | "c"   | "d"   |
          false ! false ! false ! false |
          true  ! false ! false ! true  |
          true  ! true  ! false ! true  |
          false ! true  ! false ! false |
          false ! true  ! true  ! true  |
          true  ! true  ! true  ! true  |> {

          (a, b, c, d) =>  a || b && c must_== d
        }
    }

    "work for a AND b OR c" in {

         "a"  |  "b"  | "c"   | "d"   |
        false ! false ! false ! false |
        true  ! false ! false ! false |
        true  ! true  ! false ! true  |
        false ! true  ! false ! false |
        false ! true  ! true  ! true  |
        true  ! true  ! true  ! true  |> {

        (a, b, c, d) =>  a && b || c must_== d
      }
    }

    "work for a OR b AND c OR D" in {

      "a"  |  "b"  | "c"   | "d" |
     true  ! true ! false ! true |> {

        (a, b, c, d) =>  a || b && c must_== d
      }

    }

  }


}
