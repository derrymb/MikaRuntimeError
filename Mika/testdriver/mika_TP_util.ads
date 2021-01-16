pragma Ada_95;                                                  --because the code under test may not be
package mika_TP_util is
  Overall : Boolean;                                            --Overall test run success
  maxAbsoluteError : float := 0.001;                            --was 0.00001;
  maxRelativeError : float := 0.001;                            --was  0.00001;
  --comparison of 2 floats A, B with margin of error
  function is_nearly_equal(A, B : in float) return Boolean;
  --comparison of the expected result Value against the actual result DriverName
  --side effect: Overall maybe be assigned false
  procedure compare_expected(Outcome: in Boolean;               --true if the actual result matches the expected value
                             ElementNameString: in String;      --string representation of the element being compared, used in error message only
                             ActualResultString: in String;     --string representation of the actual result, used in error message only
                             ExpectedValueString: in String     --string representation of the expected value, used in error message only
                            );

end mika_TP_util;