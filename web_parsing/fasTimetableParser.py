import json
import csv
from urllib.request import urlopen
from bs4 import BeautifulSoup
from grid import *

timetablePath = '../res/fastimetable2014.csv'

fasTimetableDir = 'http://www.artsandscience.utoronto.ca/ofr/timetable/winter/'
fasTimetableList = fasTimetableDir + 'sponsors.htm'
courses = []

class Timetable:
  code = 0
  session = 1
  title = 2
  section = 3
  time = 5
  instructor = 7

def downloadTimetables():
  print('Downloading timetable list...')
  fas = urlopen(fasTimetableList)
  fasData = fas.read()

  fasSoup = BeautifulSoup(fasData)

  with open(timetablePath, 'w+', encoding='utf8', newline='') as csvfile:
    for li in fasSoup.find_all('li'):
      html = fasTimetableDir + li.a['href']
      print('Reading ', html)

      try:
        htmlData = urlopen(html).read()
      except Exception:
        print('Couldn\'t read from ', html)
        continue

      soup = BeautifulSoup(htmlData)

      rows = soup.find_all('tr')

      for row in rows:
        cols = row.find_all('td')
        newcols = []
        for col in cols:
          newcols.append(col.get_text(strip=True))
          x = col.get('colspan')
          if x is not None:
            newcols = newcols + (int(x) - 1) * ['']
        cols = newcols
        if len(cols) == 0 or cols[0] == 'Course' or (len(cols[0]) != 8 and (len(cols) < 6 or not cols[5])):
          continue

        csv.writer(csvfile, dialect='excel').writerow(cols)
        

def parseTimetable():
  '''
  Parse timetable from internal CSV (stored in timetablePath)
  '''

  with open(timetablePath, 'r', newline='') as timetableFile:
    course = {}
    course['name'] = ''

    reader = csv.reader(timetableFile)
    for data in reader:
      if len(data) < Timetable.instructor:
        continue
      code = data[Timetable.code]

      # Check if code is not an actual course code
      if len(code) > 8:
        print('Ignoring code:', code)
        data[Timetable.code] = ''
        code = ''


      # New course?
      if code and code != course['name']:
        # Save old course
        if course['name']:
          finaliseCourse(course)
        # Initialize new course
        course = addCourse(data)
        session = addSession(data, course)
      # New session?
      elif code and code == course['name']:
        session = addSession(data, course)        
      # New section
      elif data[Timetable.section]:
        addSection(data, course[session], course)
      else:
        addToSection(data, course[session])
    
    # Add last course
    finaliseCourse(course)


def finaliseCourse(course):
  ''' Add course to the list of courses. Also handle reserved seats. '''

  # Check if not a research project course
  code = course['name']
  if code and not (len(code) >= 6 and code[4] == '9' and code[5] != '0'):
    with open('../res/courses/' + code + '.txt', 'w+') as output:
      json.dump(course, output)
    courses.append(course)


def addCourse(data):
  return {
    'name': data[Timetable.code][:8],
    'title': data[Timetable.title],
    'manualTutorialEnrolment': False
  }


def addSession(data, course):
  ''' 
  Adds a new session to the current course. Returns the name of the session. 
  '''
  session = data[Timetable.session]
  course[session] = {'lectures': [], 'tutorials': []}
  addSection(data, course[session], course)
  return session


def addSection(data, session, course):
  ''' Adds lecture/tutorial section; updates manualTutorialEnrolment. '''
  if isLecture(data):
    session['lectures'].append(makeLecture(data))
  elif isTutorial(data):
    course['manualTutorialEnrolment'] = data[Timetable.section].startswith('T') or data[Timetable.section].startswith('P')
    session['tutorials'].append(makeTutorial(data))


def addToSection(data, session):
  ''' 
  Add extra meeting time (lec/tut) to an existing section.
  Called when data[Timetable.section] is empty.
  '''
  if isLecture(data):
    lecture = session['lectures'][-1]
    # Update time, cap, instructor
    if not lecture['time']:
      lecture['time'] = parseTimeSlots(data[Timetable.time])[0]
    else:
      lecture['time'] += lecture['time'] + parseTimeSlots(data[Timetable.time])[0]
    
    if not lecture['instructor']:
      lecture['instructor'] = data[Timetable.instructor]
  elif isTutorial(data):
    session['tutorials'].append(makeTutorial(data))
  else:
    print('Error on row', data)
  

def makeLecture(data):
  ''' Create a record of a lecture from a CSV line '''
  if data[Timetable.section].startswith('L'):
    return {
      'section': data[Timetable.section],
      'time': '' if isTutorial(data) else parseTimeSlots(data[Timetable.time])[0],
      'instructor': data[Timetable.instructor],
    }
  else:
    print('makeLecture called incorrectly on row', data)


def makeTutorial(data):
  ''' Create a record of a tutorial from a CSV line '''
  # Practical or Tutorial
  if data[Timetable.section].startswith('P') or data[Timetable.section].startswith('T'):
    return [data[Timetable.section], parseTimeSlots(data[Timetable.time])[0]]
  else:
    return parseTimeSlots(data[Timetable.time])[0]

def isTutorial(data):
  ''' Returns true if data row represents a tutorial '''
  section = data[Timetable.section]
  return section is None or (len(section) == 5 and section[0] in 'PT')

def isLecture(data):
  ''' Returns true if data row represents a lecture '''
  section = data[Timetable.section]
  return section is not None and section != '' and section[0] == 'L'