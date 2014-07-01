from bs4 import BeautifulSoup
from urllib.request import urlopen
import re
import json

fasCalendar = 'http://www.artsandscience.utoronto.ca/ofr/calendar/'

def parseFASCalendar():
  fas = urlopen(fasCalendar)
  fasData = fas.read()
  fasSoup = BeautifulSoup(fasData)
  depts = fasSoup.find('div', class_='items').find_all('li')

  titleParser = re.compile('(.{8})\s*(.*?)[\(\[]')

  for dept in depts:
    print(dept.a['href'])
    deptSoup = BeautifulSoup(urlopen(fasCalendar + dept.a['href']).read())
    heading = deptSoup.find_all('h2')[-1]
    courses = heading.find_all_next(
                lambda elem: 
                  elem.get('name') != None and
                  len(elem.get('name').strip()) == 8)
    for elem in courses:
      if elem.next_sibling.string:
        course_string = elem.next_sibling.string.strip()
      else:
        continue
        
      if course_string == '':
        continue
      result = titleParser.match(course_string + '[')

      if result is None:
        print('---Couldn\'t parse!---', course_string)
        continue

      name = result.group(1)
      title = result.group(2)

      try:
        with open('../../res/courses/' + name + '.txt', 'r', encoding='utf-8') as coursefile:
          data = json.load(coursefile)
          data['title'] = title
        with open('../../res/courses/' + name + '.txt', 'w', encoding='utf-8') as coursefile:
          json.dump(data, coursefile)
      except FileNotFoundError:
        pass