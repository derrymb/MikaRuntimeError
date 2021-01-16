package body array_date is

function is_leap(y : year_t) return boolean is
begin
  return  (y mod 4 = 0 and y mod 100 /= 0) or y mod 400 = 0;
end is_leap;

procedure tomorrow(date: in date_t; next_date: out date_t) is
begin
  next_date := date;
  if date.name = name_t'last then
    next_date.name := name_t'first;
  else
    next_date.name := name_t'succ(date.name);
  end if;
  if (date.month = dec and date.day = 31) then
    next_date.day := 1;
    next_date.month := jan;
    next_date.year := date.year +1;
  elsif (date.day = 28 and date.month = feb) then
    if is_leap(next_date.year) then
      next_date.day := 29;
    else
      next_date.day := 1;
      next_date.month := mar;
    end if;
  elsif (date.day = 31 or (date.day = 29 and date.month = feb) or
    (date.day = 30 and (date.month = apr or date.month = jun or date.month = sep or date.month = nov))) then
    next_date.day := 1;
    next_date.month := month_t'succ(date.month);
  else
    next_date.day := date.day+1;
  end if;
end tomorrow;

function preceeds(d1, d2 : date_t) return boolean
is
  p : boolean;
begin
  if d1.year < d2.year then
    p := true;
  elsif d1.year > d2.year then
    p := false;
  elsif d1.month < d2.month then
    p := true;
  elsif d1.month > d2.month then
    p := false;
  elsif d1.day < d2.day then
    p := true;
  elsif d1.day > d2.day then
    p := false;
  else
    p := false;
  end if;
  return p;
end preceeds;

procedure InsertionSort(L: in out list) is
  place : index;
  current : date_t;
  found : boolean;
begin
for firstunsorted in index range index'succ(index'first) .. index'last loop
  if preceeds(L(firstunsorted), L(index'pred(firstunsorted))) then
    place := firstunsorted;
    current := L(firstunsorted);
    loop
      place := index'pred(place);
      L(index'succ(place)) := L(place);
      if place = index'first then
        found := true;
      else
        found := preceeds(L(index'pred(place)), current);
      end if;
      exit when found;
    end loop;
    L(place) := current;
  end if;
end loop;
end InsertionSort;

procedure Mika_Test_Point(Test_number : in Integer) is separate;

begin
  null;
end array_date;

