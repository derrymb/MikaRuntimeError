package date is
  type name_t is (mon, tue, wed, thu, fri, sat, sun);
  type day_t is range 1..31;
  type month_t is (jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec);
  type year_t is range 1900..3000;
  type date_t is 
    record
      name : name_t;
      day : day_t;
      month : month_t;
      year : year_t;
    end record;

procedure tomorrow(date: in date_t; next_date: in out date_t);
procedure Mika_Test_Point(Test_number : in Integer);
end date;
