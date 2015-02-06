import org.specs2._

import model._
import model.Fitnesses._

class FitnessesSpec extends Specification { def is = s2"""

This is a specificiation to check the different fitness evaluation functions.

The linear function should
    produce a score of 0.0 if there are no matches to the target                $lin0
    produce a score of 0.125 if 1 out of 8 values match the target              $lin1
    produce a score of 0.25 if 1 out of 8 values match the target               $lin2
    produce a score of 0.375 if 1 out of 8 values match the target              $lin3
    produce a score of 0.5 if 1 out of 8 values match the target                $lin4
    produce a score of 0.625 if 1 out of 8 values match the target              $lin5
    produce a score of 0.75 if 1 out of 8 values match the target               $lin6
    produce a score of 0.875 if 1 out of 8 values match the target              $lin7
    produce a score of 1.0 if 1 out of 8 values match the target                $lin8

The exponential function should
    produce a score of 0 if there are no matches to the target                  $exp0
    produce a score of 0.015625 if 1 out of 8 match the target                  $exp1
    produce a score of 0.0625 if 2 out of 8 match the target                    $exp2
    produce a score of 0.140625 if 3 out fo 8 match the target                  $exp3
    produce a score of 0.25 if 4 out fo 8 match the target                      $exp4
    produce a score of 0.390625 if 5 out fo 8 match the target                  $exp5
    produce a score of 0.5625 if 6 out fo 8 match the target                    $exp6
    produce a score of 0.765625 if 7 out fo 8 match the target                  $exp7
    produce a score of 1 if it is a perfect match to the target                 $exp8

                                                                                """

    val lin = linear("12345678")_
    def lin0 = lin(DNA("00000000")) === 0.0
    def lin1 = lin(DNA("11111111")) === 0.125
    def lin2 = lin(DNA("12111111")) === 0.25
    def lin3 = lin(DNA("12311111")) === 0.375
    def lin4 = lin(DNA("12341111")) === 0.5
    def lin5 = lin(DNA("12345111")) === 0.625
    def lin6 = lin(DNA("12345611")) === 0.75
    def lin7 = lin(DNA("12345671")) === 0.875
    def lin8 = lin(DNA("12345678")) === 1.0

    val exp = exponential("12345678")_
    def exp0 = exp(DNA("00000000")) === 0.0
    def exp1 = exp(DNA("11111111")) === 0.015625
    def exp2 = exp(DNA("12111111")) === 0.0625
    def exp3 = exp(DNA("12311111")) === 0.140625
    def exp4 = exp(DNA("12341111")) === 0.25
    def exp5 = exp(DNA("12345111")) === 0.390625
    def exp6 = exp(DNA("12345611")) === 0.5625
    def exp7 = exp(DNA("12345671")) === 0.765625
    def exp8 = exp(DNA("12345678")) === 1.0
}