from urllib.request import urlopen
import json

coursesUrl = "https://timetable.iit.artsci.utoronto.ca/api/20229/courses?org=&code=&section=F,S,Y&studyyear=&daytime=&weekday=&prof=&breadth=&deliverymode=&online=&waitlist=&available=&fyfcourse=&title="
response = urlopen(coursesUrl)

jsonData = json.loads(response.read())
jsonString = json.dumps(jsonData)
coursesFile = open("courses.json", "w")
coursesFile.write(jsonString)
coursesFile.close()
