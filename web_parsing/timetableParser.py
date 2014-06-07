# Warning: needs xlrd package to run (read Excel files)
# Download from http://www.python-excel.org/
# Of course, the "master.xlsx" file is also required (distributed offline)

import json
from grid import *
import xlrd

courses             = []

timetablePath       = '../res/timetable2014.csv'
timetableOutputPath = '../res/timetableHTML2014.html'

fallGridPath        = '../res/fallGrid.html'
springGridPath      = '../res/springGrid.html'

excelPath           = '../res/master.xlsx'

class TimetableData:
  code       = 0
  session    = 1
  title      = 2
  section    = 3
  time       = 4
  kind       = 5
  instructor = 6
  cap        = 7

##################################################
# WORK WITH EXCEL FILE
##################################################

def generateCSV():
  '''
  Take the Excel spreadsheet and generate a csv from it
  '''
  book = xlrd.open_workbook(excelPath)
  sh = book.sheet_by_index(0)
  with open(timetablePath, 'w+') as csv:
    for row in range(2, sh.nrows):
      if sh.cell_value(rowx=row, colx=6) and \
         sh.cell_value(rowx=row, colx=4) != 'not offered':
        for col in [1, 2, 3, 4, 6, 7, 8, 11]:
          if col == 11:
            cap = sh.cell_value(rowx=row, colx=col)
            if cap:
              cap = int(cap)
            csv.write(sanitize(cap))
          else:
            csv.write(sanitize(sh.cell_value(rowx=row, colx=col)) + ',')
        csv.write('\n')

def sanitize(s):
  ''' Really only for the Borodin/Boutillier cell '''
  return str(s).replace('\n', '/')



##################################################
# GENERATE MODEL
##################################################

def parseTimetable():
  '''
  Parse timetable from internal CSV (stored in timetablePath)
  Each line has the following fields:
  <Check Prereqs>, <Code>, <Session>, <Title>, <Section>, <Wait List>, 
  <Time>, <Kind>, <Instructor>, <I>, <Controls>, <Large>, <Location>, <Notes>
  '''

  with open(timetablePath, 'r') as timetableFile:
    course = {}
    course['name'] = ''
    for line in timetableFile:
      data = line.split(',')
      code = data[TimetableData.code][:8]

      # Check if research project course
      if code and code[4] == '9' and code[5] != '0':
        continue

      # New course?
      if code and code != course['name']:
        # Save old course
        if course['name']:
        #  with open('../res/courses/timetable/' + course['name'] + 'TimeTable.txt', 'w+') as output:
        #    json.dump(course, output)
          courses.append(course)

        # Initialize new course
        course = addCourse(data)
        session = addSession(data, course)

      # New session?
      elif code and code == course['name']:
        session = addSession(data, course)
        
      # New section
      elif data[TimetableData.section]:
        addSection(data, course[session], course)
      else:
        addToSection(data, course[session])
    
    # Add last course
    courses.append(course)
  

def outputJSON():
  for course in courses:

    with open('../res/courses/timetable/' + course['name'] + 'TimeTable.txt', 'w+') as output:
      json.dump(course, output)

def addCourse(data):
  return {
    'name': data[TimetableData.code][:8],
    'title': data[TimetableData.title],
    'manualTutorialEnrolment': False
  }


def addSession(data, course):
  ''' 
  Adds a new session to the current course. Returns the name of the session. 
  '''
  session = data[TimetableData.session]
  course[session] = {'lectures': [], 'tutorials': []}
  addSection(data, course[session], course)
  return session


def addSection(data, session, course):
  ''' Adds lecture/tutorial section; updates manualTutorialEnrolment. '''
  if data[TimetableData.section].startswith('L'):
    session['lectures'].append(makeLecture(data))
    
  if isTutorial(data):
    course['manualTutorialEnrolment'] = data[TimetableData.section].startswith('T')
    session['tutorials'].append(makeTutorial(data))


def addToSection(data, session):
  ''' 
  Add extra meeting time (lec/tut) to an existing section.
  Called when data[TimetableData.section] is empty.
  '''
  if data[TimetableData.kind] == 'L':
    lecture = session['lectures'][-1]
    # Update time, cap, instructor
    if not lecture['time']:
      lecture['time'] = data[TimetableData.time]
    else:
      lecture['time'] += ',' + data[TimetableData.time]
    if not lecture['cap'] or not lecture['cap'].strip().isdigit():
      lecture['cap'] = data[TimetableData.cap]
    if not lecture['instructor']:
      lecture['instructor'] = data[TimetableData.instructor]
  elif isTutorial(data):
    session['tutorials'].append(makeTutorial(data))
  else:
    print('Error on row', data)
  

def makeLecture(data):
  ''' Create a record of a lecture from a CSV line '''
  if data[TimetableData.section].startswith('L'):
    return {
      'section': data[TimetableData.section],
      'time': '' if isTutorial(data) else data[TimetableData.time],
      'cap': data[TimetableData.cap],
      'instructor': data[TimetableData.instructor],
      'extraCap': 0
    }
  else:
    print('makeLecture called incorrectly on row', data)


def makeTutorial(data):
  ''' Create a record of a tutorial from a CSV line '''
  if data[TimetableData.section].startswith('T'):
    return [data[TimetableData.section], data[TimetableData.time]]
  else:
    return data[TimetableData.time]

def isTutorial(data):
  ''' Returns true if data row represents a tutorial '''
  return data[TimetableData.kind] in ['T', 'LAB']



##################################################
# GENERATE HTML
##################################################

def generateRows(course):
  ''' Generate all html rows for one course. '''
  rows = []

  for term in ['Y', 'F', 'S']:
    if term in course:
      start = '<td class="{}Offering" colspan="{}""><table class="courseTable" border>' \
              .format(term, 2 if term == 'Y' else 1)
      rows.append(start)
      termRows = []

      lecs = course[term]['lectures']
      tuts = course[term]['tutorials']
      for i, lec, in enumerate(lecs):

        # Don't display enrolment control sections
        if lec['section'].startswith('L2'):
          continue

        tutString = '<span style="float: right">{}</span>'
        if not course['manualTutorialEnrolment'] and i < len(tuts):
          tutString = tutString.format('(' + tuts[i] + ')')
        else:
          tutString = tutString.format('')
        
        termRows.append(('<tr>' + 
              '<td class="timetableSection">{}</td>' +
              '<td class="timetableTime">{} {}</td>' +
              '<td class="timetableInstructor">{}</td>' +
              '<td class="timetableCap">{}{}</td></tr>') \
              .format(
                lec['section'], 
                lec['time'], tutString,
                lec['instructor'], 
                lec['cap'], '+ ' + lec['extraCap'] if lec['extraCap'] > 0 else ''
              ))

      # Add separate tutorial sections, if necessary
      if course['manualTutorialEnrolment']:
        for tut in tuts:
          termRows.append(
            ('<tr><td class="timetableSection">{}</td>' +
            '<td class="timetableTime">{}</td></tr>')
            .format(tut[0], tut[1]))

      rows = (rows + termRows)
      rows.append('</table></td>')


      if term == 'Y':
        break

    # Section not offered that term
    elif term == 'F' or term == 'S':
      rows.append('<td class="{}Offering"></td>'.format(term))

  # TODO: remove inline styling
  rows = (['<tr class="searchClass">', 
    '<td class="timetableCourseName" style="vertical-align:top">{}</td>'
    .format(course['name'])] + rows + ['</tr>'])

  return rows
        

def generateHTML():
  with open(timetableOutputPath, 'w+') as htmlOutput:
    htmlOutput.write('<table id="timetableMain" border><tr>' +
      '<td class="timetableCourseName">Course</td>' +
      '<th class="sessionHeader FOffering">FALL</th>' + 
      '<th class="sessionHeader SOffering">SPRING</th></tr>'
      )
    
    # Header row
    htmlOutput.write('<tr><td class="timetableCourseName"></td>' + 
      '<td class="FOffering"><table class="courseTable">' +
      '<tr><th class="timetableSection">Sec</th>' + 
      '<th class="timetableTime">Time</th>' +
      '<th class="timetableInstructor">Instructor</th>' +
      '<th class="timetableCap">Cap</th></tr></table></td>' +
      '<td class="SOffering"><table class="courseTable">' + 
      '<tr><th class="timetableSection">Sec</th>' + 
      '<th class="timetableTime">Time</th>' +
      '<th class="timetableInstructor">Instructor</th>' +
      '<th class="timetableCap">Cap</th></tr></table></td>'
      )

    for course in courses:
      htmlOutput.writelines(iter(generateRows(course)))
    htmlOutput.write('</table>')



##################################################
# GENERATE Timetable Grid
##################################################

def generateFallGrid():
  generateGrid(['F','Y'], fallGridPath)

def generateSpringGrid():
  generateGrid(['S','Y'], springGridPath)

def generateGrid(terms, file):
  courseTimes = []
  for course in courses:
    for term in terms:
      if term in course:
        for lec in course[term]['lectures']:
          if not lec['section'].startswith('L2'):
            allSlots = parseTimeSlots(lec['time'])[0]
            if allSlots:
              courseTimes.append((course['name'], allSlots))

  grid = buildGrid(courseTimes)
  renderGrid(grid, file)

if __name__ == '__main__':
  generateCSV()
  parseTimetable()
  generateHTML()
  generateFallGrid()
  generateSpringGrid()
  outputJSON()