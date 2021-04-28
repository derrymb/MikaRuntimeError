package array_date is
  type name_t is (mon, tue, wed, thu, fri, sat, sun);
  type year_t is range 1900 .. 3000;
  type day_t is range 1..31;
  type month_t is (jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec);
  type date_t is 
    record
      name : name_t;
      day : day_t;
      month : month_t;
      year : year_t;
    end record;
  type index is range 1..50;
  type list is array(index) of date_t;
  procedure InsertionSort(L: in out list);
  procedure Mika_Test_Point(Test_number : in Integer);
end array_date;
