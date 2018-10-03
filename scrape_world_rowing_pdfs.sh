# Code by Chuck Rai
# Small Adjustements made by Dani Chu

# move to current directory and run to in 
# Windows Power Shell to scrape all pdfs of results for world rowing championships
# pdfs include media strat list, results and Race Data (GPS)

# http://www.worldrowing.com/events/2007-world-championships/schedule-results
# Example URL navigation
head=http://www.worldrowing.com

# for years when world rowing started collecting data
for year in {2010..2018}
do
	# directory name
	dir="./scraped_pdfs/"$year"_world_championships/"

	# create directory if it does not exist already
	mkdir -p $dir

	# the url changes in 2014 so greater than 2014
	# needs a different url path
	world_champs_text="-world-championships"
	if [[ $year -gt 2013 ]]
		then
		world_champs_text="-world-rowing-championships"
	fi

	for category in $(curl -s $head"/events/"$year$world_champs_text"/schedule-results" | grep "fa fa-chevron-right" | grep -o 'href="[^"]*' | grep -o '[^"]*$')
	do
		caturl=$head$category
		for section in $(curl -s $caturl | grep -o '<a.*class="toUpper".*</a>' | grep -o '>[^<]*' | cut -c2- | tr '[:upper:]' '[:lower:]')
		do
			securl=$caturl$section"/"
			for file in $(curl -s $securl | grep -o '[^"]*\.pdf')
			do
				name=$(echo $head$file | sed -e "s/\&amp\;/\&/g")
				save=$(echo $file | grep -o "[^/]*\.pdf")
				curl -s $name > $dir$save
			done
		done
	done
done
