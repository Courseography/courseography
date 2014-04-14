import json

courses = []

timetablePath = 'res/timetable2014.csv'
timetableOutputPath = 'res/timetableHTML2014.html'

class TimetableData:
  code = 1
  session = 2
  title = 3
  section = 4
  time = 6
  kind = 7
  instructor = 8
  cap = 12

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
      # Ignore 'not offered' courses, lines that don't represent courses (no 'time')
      if data[TimetableData.section] == 'not offered' or data[TimetableData.time] == '':
        #print('Ignored line: ' + line)
        continue

      code = data[TimetableData.code][:8]

      # Check if research project course
      if len(code) > 0 and code[4] == '9' and code[5] != '0':
        continue

      if code != course['name'] and code != '' :
        # Save old course
        if course['name'] != '':
          courses.append(course)

        # Initialize new course
        course = addCourse(data)
        session = addSession(data, course)

      # New session
      elif data[TimetableData.code][:8] == course['name']:
        session = addSession(data, course)
        
      # New section
      elif data[TimetableData.section]:
        addSection(data, course[session])
        if data[TimetableData.section].startswith('T'):
          course['manualTutorialEnrolment'] = True
      else:
        #print(course)
        addToSection(data, course[session])
    
    # Add last course
    courses.append(course)

def addCourse(data):
  course = {}
  course['name'] = data[TimetableData.code][:8]
  course['title'] = data[TimetableData.title]
  course['manualTutorialEnrolment'] = False
  return course


def addSession(data, course):
  ''' Adds a new session to the current course. Returns the name of the session. '''
  session = data[TimetableData.session]
  course[session] = {}
  course[session]['lectures'] = []
  course[session]['tutorials'] = []
  
  addSection(data, course[session])
  if data[TimetableData.section].startswith('T'):
    course['manualTutorialEnrolment'] = True

  return session


def addSection(data, session):
  ''' When a new course section is encountered adds it to the list of lectures/tutorials '''
  if data[TimetableData.section].startswith('L') or data[TimetableData.kind] == 'L':
    session['lectures'].append(makeLecture(data))
    
  if data[TimetableData.kind] == 'T':
    session['tutorials'].append(makeTutorial(data))


def addToSection(data, session):
  ''' Used when data doesn't specify the section, so what's being added is either a tutorial or lecture 
  for an existing section '''
  if data[TimetableData.kind] == 'L':
    if session['lectures']:
      lecture = makeLecture(data)
      lecture['section'] = session['lectures'][-1]['section']
      session['lectures'][-1] = lecture
    else:
      session['lectures'].append(makeLecture(data))
  elif data[TimetableData.kind] == 'T':
    session['tutorials'].append(makeTutorial(data))


def makeLecture(data):
  ''' Create a record of a lecture from a CSV line '''
  if data[TimetableData.kind] == 'L' or data[TimetableData.kind] == '':
    return {
      'section': data[TimetableData.section],
      'time': data[TimetableData.time],
      'cap': data[TimetableData.cap],
      'instructor': data[TimetableData.instructor],
      'extraCap': 0
    }
  elif data[TimetableData.kind] == 'T' and data[TimetableData.section].startswith('L'):
    return { 'section': data[TimetableData.section] }
  else:
    return {}


def makeTutorial(data):
  ''' Create a record of a tutorial from a CSV line '''
  if data[TimetableData.section].startswith('T'):
    return [data[TimetableData.section], data[TimetableData.time]]
  else:
    return data[TimetableData.time]





def generateRows(course):
  rows = []

  for term in ['Y', 'F', 'S']:
    if term in course:
      if term == 'Y':
        start = '<td class="' + term + 'Offering" colspan="2">'
      else:
        start = '<td class="' + term + 'Offering">'
      start += '<table class="courseTable">';
      rows.append(start)
      termRows = []


      for i, lec in enumerate(course[term]['lectures']):
        # Don't display enrolment control sections
        if lec['section'].startswith('L2'):
          continue

        if course['manualTutorialEnrolment'] or i >= len(course[term]['tutorials']):
          tutString = ''
        else:
          tutString = '<span style="float: right">({})</span>'.format(course[term]['tutorials'][i])
        row = '<tr>'

        print(str(course) + ' ' + str(lec))
        row += (
            '<td class="timetableSection">{}</td>' +
            '<td class="timetableTime">{} {}</td>' +
            '<td class="timetableInstructor">{}</td>' +
            '<td class="timetableCap">{}') \
            .format(
              lec['section'], lec['time'], 
              tutString,
              lec['instructor'], lec['cap'])
        if lec['extraCap'] > 0:
          row += ' + {}'.format(lec['extraCap'])
        
        row += '</td></tr>'
        termRows.append(row)

      if course['manualTutorialEnrolment']:
        for tut in course[term]['tutorials']:
          termRows.append(('<tr><td class="timetableSection">{}</td>' +
            '<td class="timetableTime">{}</td></tr>')
            .format(tut[0], tut[1]))

      rows = (rows + termRows)
      rows.append('</table></td>')

      if term == 'Y':
        break

    elif term == 'F' or term == 'S':
      rows.append('<td class="' + term + 'Offering"></td>')



  rows = (['<tr class="searchClass">', 
    '<td class="timetableCourseName" style="vertical-align:top">{}</td>'
    .format(course['name'])] + rows + ['</tr>'])


  return rows

        

def generateHTML():
  with open(timetableOutputPath, 'w+') as htmlOutput:
    htmlOutput.write('<table id="timetableMain"><tr>' +
      '<td class="timetableCourseName"></td>' +
      '<th class="sessionHeader FOffering">FALL</th>' + 
      '<th class="sessionHeader SOffering">SPRING</th></tr>'
      )
    
    # Header row
    htmlOutput.write('<tr><td class="timetableCourseName"></td>' + 
      '<td class="FOffering"><table class="courseTable"><tr><th class="timetableSection">Sec</th>' + 
      '<th class="timetableTime">Time</th>' +
      '<th class="timetableInstructor">Instructor</th>' +
      '<th class="timetableCap">Cap</th></tr></table></td>' +
      '<td class="SOffering"><table class="courseTable"><tr><th class="timetableSection">Sec</th>' + 
      '<th class="timetableTime">Time</th>' +
      '<th class="timetableInstructor">Instructor</th>' +
      '<th class="timetableCap">Cap</th></tr></table></td>'
      )

    for course in courses:
      print(course)
      htmlOutput.writelines(iter(generateRows(course)))
    htmlOutput.write('</table>')

if __name__ == '__main__':
  parseTimetable()
  generateHTML()