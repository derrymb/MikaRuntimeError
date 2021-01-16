pragma Ada_95;
with Text_IO; use Text_IO;
package body mika_TP_util is

  procedure compare_expected(Outcome: in Boolean;
                             ElementNameString: in String;
                             ActualResultString: in String;
                             ExpectedValueString: in String
                            )
  is
  begin
    if not(Outcome) then
      Overall := False;
      put("      During execution ");
      put(ElementNameString);
      put(" is equal to");
      put(ActualResultString);
      put(" instead of being equal to ");
      put(ExpectedValueString);
      put_line(" as indicated in the test case");
    end if;
  end compare_expected;

  function is_nearly_equal(A, B : in float) return Boolean is
  --adapted from "Comparing floating point numbers" by Bruce Dawson
    begin
      if (abs(A - B) < maxAbsoluteError) then
        return true;
      end if;
      declare
        relativeError : float;
      begin
        if (abs(B) > abs(A)) then
          relativeError := abs((A - B) / B);
        else
          relativeError := abs((A - B) / A);
        end if;
        return (relativeError <= maxRelativeError);
      end;
    end is_nearly_equal;
end mika_TP_util;