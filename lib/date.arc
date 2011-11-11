(require "lib/lang.arc")

(def parse-date (str)
  (timedate
    (perl subprocess “
	  use Date::Language;
	  arcnum(Date::Language->new('English')->str2time(«str»));
	  ”)))
