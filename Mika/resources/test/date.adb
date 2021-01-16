package body date is

function is_leap(y : year_t) return boolean is
begin
  return  (y mod 4 = 0 and y mod 100 /= 0) or y mod 400 = 0;
end is_leap;

procedure tomorrow(date: in date_t; next_date: in out date_t) is
  year : year_t;
  --is_leap : Boolean;
begin
--initialisation
	next_date := date;

--calculate the next name day
	if date.name = name_t'last then 
		next_date.name := name_t'first;
	else 
		next_date.name := name_t'succ(date.name);
	end if;

--calculate the next_date
	if (date.month = dec and date.day = 31) then
		next_date.day := 1;
		next_date.month := jan;
		next_date.year := date.year +1;
	elsif (date.day = 28 and date.month = feb) then
	        year := next_date.year;
		if is_leap(year) then
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

procedure Mika_Test_Point(Test_number : in Integer) is separate;
end date;
