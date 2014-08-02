from bs4 import BeautifulSoup
from urllib.request import urlopen
import re
import json

fasCalendar = 'http://www.artsandscience.utoronto.ca/ofr/calendar/'

EXCL = 'Exclusion:'
DIST = 'Distribution Requirement Status:'
BREADTH = 'Breadth Requirement:'
REC = 'Recommended Preparation:'
SUG = 'Suggested Preparation:'
PREREQ = 'Prerequisite:'

labels = (EXCL,
          DIST,
          BREADTH,
          REC,
          SUG,
          PREREQ,
          )


def parseFASCalendar():
  fas = urlopen(fasCalendar)
  fasData = fas.read()
  fasSoup = BeautifulSoup(fasData)
  depts = fasSoup.find('div', class_='items').find_all('li')

  titleParser = re.compile('(.{8})\s*(.*?)[\(\[]')

  for dept in depts:
    print('Parsing calendar department', dept.a['href'])
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

      if elem.parent.name == 'strong':
        curr_elem = elem.parent.parent.next_sibling
      else:
        curr_elem = elem.next_sibling.next_sibling
      s = ''

      while curr_elem and (curr_elem.name != 'a' or curr_elem.get('name') is None):
        if curr_elem.name is None:
          line = ' '.join(curr_elem.string.split())
          if line.startswith(labels):
            s += '\n'
          s += line
        curr_elem = curr_elem.next_element

      lines = s.split('\n')
      new_data = {
        'name': name,
        'title': title,
        'exclusions': None,
        'distribution': None,
        'breadth': None,
        'prep': None,
        'prereqString': None,
        'description': ''
      }

      for line in lines:
        new_data['distribution'] = (new_data['distribution'] or
                                    read_field(line, DIST))
        new_data['breadth'] = (new_data['breadth'] or
                              read_field(line, BREADTH))
        new_data['prep'] = new_data['prep'] or read_field(line, REC)
        new_data['prep'] = new_data['prep'] or read_field(line, SUG)
        new_data['prereqString'] = (new_data['prereqString'] or
                                    read_field(line, PREREQ))
        if not line.startswith(labels):
          new_data['description'] += line.strip()

      course_path = '../../res/courses/' + name + '.txt'

      try:
        with open(course_path, 'r', encoding='utf-8') as coursefile:
          data = json.load(coursefile)
          data['title'] = title
        with open(course_path, 'w', encoding='utf-8') as coursefile:
          json.dump(data, coursefile)
      except FileNotFoundError:
        data = {}

      data.update(new_data)

      with open(course_path, 'w+', encoding='utf-8') as coursefile:
        json.dump(data, coursefile)


def read_field(s, field):
  """
  Reads data in form '<field>: <data>' and returns data string.
  Returns none if s doesn't start with field.
  """

  if s.startswith(field):
    return s[len(field):].strip()
  else:
    return None

if __name__ == '__main__':
  parseFASCalendar()
