(load "lib/lang.arc")

(def parse-date (str)
  (timedate (int
	      (perl subprocess “
		    use Date::Parse;
		    use Date::Language;
		    
		    my $lang = Date::Language->new('English');
		    my $time = $lang->str2time(«str»);
		    ”))))

