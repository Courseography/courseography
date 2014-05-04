#! /usr/bin/env python3

from urllib.request import urlopen
from html.parser import HTMLParser
import re

# Extract data from Time Table
html = urlopen('http://www.artsandscience.utoronto.ca/ofr/timetable/winter/csc.html')
htmlData = html.read()

# The regular expressions for all 6 fields
course = re.compile('^CSC\d{3}(Y|H)1$')
section = re.compile('^[FSY]{1}$')
lecture = re.compile('^([TL]){1}[0123456789]{2}0{1}\d{1}$')
time = re.compile('^[MWTRF]{1,5}\d{0,2}[-]?\d{0,2}$|Cancel|TBA')
instructor = re.compile('^\w\.|Tba|Staff')
location = re.compile('^\w{2} \d{1,4}|SS')

# Different cases for extracted data
class MyHTMLParser(HTMLParser):

	# Variables to remember data fields and later print them at appropriate times.
    courseMemory = ''
    sectionMemory = ''
    typeMemory = 'stringsAgainstIndexErrors!'
    timeMemory = ''
    locationMemory = ''
    instructorMemory = ''

    # Switches that are turned on and off, according to specific data that is reead.
    roomChangeMemory = False
    roomSwitch = False

    # This switch is for "Y" section courses, and is turned on after a course is read.
    # This way, we know that the "Y" that is being read is not a waitlist "Y".
    switch = False
    fieldMemory = 0

    # Takes in data from HTML parser and sorts it accordingly.
    def handle_data(self, data):

    	# Checks if the data is a course.
    	if course.match(data):
	        if self.fieldMemory == 4 and self.typeMemory[1] != '2':
	        	print('<tr class=timeTableRow><td></td><td class=timeTableBlock></td>'
	        		+'<td class=timeTableBlockType>'
	        		+ self.typeMemory 
	        		+'</td><td class=timeTableBlockTime>' 
	        		+ self.timeMemory
	        		+ '</td><td class=timeTableBlockLocation>'
	        		+self.locationMemory
	        		+'</td><td class=timeTableBlockInstructor>'
	        		+ self.instructorMemory
	        		+'</td>')
	        self.switch = True
	        self.fieldMemory = 0
	        self.courseMemory = data
    	
    	# Checks if the data is a section, either "F", "S" or "Y". 
    	# The switch indicates that this field comes directly after the course field, 
    	# to prevent waitlist fields from being entered.
    	elif section.match(data) and self.switch:
	        if data == "F":
		        print('</table><table class="searchClass" cellspacing="0">'+ ' <tr class=timeTableRow><td class=timeTableCourse id='
		        	+ self.courseMemory 
		        	+' style="background: backgroundInstertion">' 
		        	+ self.courseMemory 
		        	+'</td><td class=timeTableBlockSectionFall>' 
		        	+ "Fall" 
		        	+'</td>\n')
	        elif data == "S":
		        print('</table><table class="searchClass" cellspacing="0">'+ ' <tr class=timeTableRow><td class=timeTableCourse id='
		        	+ self.courseMemory 
		        	+' style="background: backgroundInstertion">' 
		        	+ self.courseMemory 
		        	+'</td><td class=timeTableBlockSectionSpring>' 
		        	+ "Spring"
		        	+'</td>\n')
	        elif data == "Y":
		        print('</table><table class="searchClass" cellspacing="0">'+ ' <tr class=timeTableRow><td class=timeTableCourse id='
		        	+ self.courseMemory 
		        	+' style="background: backgroundInstertion">' 
		        	+ self.courseMemory 
		        	+'</td><td class=timeTableBlockSectionYear>' 
		        	+ "Full Year" 
		        	+'</td>\n')
	        self.sectionMemory = data
	        self.fieldMemory = 1
	        self.switch = False
    	
    	# Checks the data that has" note: room change".
    	elif "change" in data:
    		self.roomChangeMemory = True
    	
    	# Checks if this is a lecture/tutorial, and remembers it.
    	elif lecture.match(data):
	        self.typeMemory = data
	        self.fieldMemory = 2
	        self.roomChangeMemory = False
	        self.roomSwitch = False
    	
    	# Checks if data is a time.
    	elif time.match(data):
	        self.timeMemory = data
	        self.fieldMemory = 3
	        # For Tutorials. (Either 'L'ecture or 'T'utorial) 
	        # Time is the last field for a tutorial section.
	        if self.typeMemory[0] == 'T': #and data!= 'Cancel':
	            print('<tr class=timeTableRow><td class=timeTableBlock></td><td class=timeTableBlock></td><td class=timeTableBlockTypeTutorial>'
	            	+ self.typeMemory 
	            	+'</td><td class=timeTableBlockTutorialTime>' 
	            	+ data
	            	+'</td><td class=timeTableBlock></td><td class=timeTableBlock></td>')

	        # For cancelled courses. Currently enabled.
	        elif data == 'Cancel' and self.typeMemory[1] != '2':
	        	print('<tr class=timeTableRow><td class=timeTableBlock></td>'
	        		+'<td class=timeTableBlock></td><td class=timeTableBlockType>' 
	        		+ self.typeMemory +'</td><td class=timeTableBlockTime>' 
	        		+ data
	        		+'</td><td class=timeTableBlock></td><td class=timeTableBlock></td>')
	        
	        if self.roomChangeMemory == True:
	        	self.roomSwitch = True
    	# Checks if data is a location.
    	elif location.match(data):
	        self.locationMemory = data
	        if self.roomSwitch == True and self.typeMemory[1] != '2':
	        	print('<tr class=timeTableRow><td></td><td class=timeTableBlock></td>'
	        		+'<td class=timeTableBlockType>'
	        		+ self.typeMemory 
	        		+'</td><td class=timeTableBlockTime>' 
	        		+ self.timeMemory
	        		+ '</td><td class=timeTableBlockLocation>'
	        		+self.locationMemory
	        		+'</td><td class=timeTableBlockInstructor>'
	        		+ self.instructorMemory
	        		+'</td>')
	        self.roomChangeMemory = False
	        self.roomSwitch = False
	        self.fieldMemory = 4
    	
    	# Checks if data is an instructor.
    	elif instructor.match(data):
	        if self.typeMemory[1] != '2':
	        	print('<tr class=timeTableRow><td class=timeTableBlock></td>' 
	        		+'<td class=timeTableBlock></td><td class=timeTableBlockType>'
	        		+ self.typeMemory 
	        		+'</td><td class=timeTableBlockTime>' 
	        		+ self.timeMemory
	        		+'</td><td class=timeTableBlockLocation>'
	        		+self.locationMemory
	        		+'</td><td class=timeTableBlockInstructor>'
	        		+data
	        		+'</td>')
	        self.fieldMemory = 5
	        self.instructorMemory = data
	        

# The html parser that parses the data.
parser = MyHTMLParser(strict=False)

# Everything is printed, and called in this block.
print('<table class=\"timeTable\" cellspacing="0">')
print('<tr class=timeTableRow>'
	+ '<td class=timeTableHeader>Course</td>'
	+ '<td class=timeTableHeader>Section</td>'
	+ '<td class=timeTableHeader>Type</td>'
	+ '<td class=timeTableHeader>Time</td>'
	+ '<td class=timeTableHeader>Location</td>'
	+ '<td class=timeTableHeader>Instructor</td>'
	+ '</tr><tr><table class="lonelyTable">')
parser.feed(str(htmlData))
print('</table></div></table>')