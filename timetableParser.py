import json

courses = []

def parseTimetable():
  with open('res/timetable.csv', 'r') as timetableFile:
    course = {}
    course['name'] = ''
    for line in timetableFile:
      data = line.split(',')
      if len(data) < 9:
        print('Too short: ' + line)
        continue

      if data[0][:8] != course['name']:
        # New course
        if (course['name'] != ''):
          courses.append(course)

        # Put in data for new course
        course = {}
        course['name'] = data[0][:8] # Course code
        course[data[1]] = {}
        course[data[1]]['lectures'] = [{
          'section': data[2],
          'time': data[4],
          'cap': data[3],
          'instructor': data[8],
          'extraCap': 0
          }]
        course['manualTutorialEnrolment'] = data[6] == ''
        if not course['manualTutorialEnrolment']:
          course[data[1]]['tutorials'] = [data[6].strip()]
        else:
          course[data[1]]['tutorials'] = []
      elif data[2].startswith('L') and data[3]:
          # Another lecture section
          # Check if it's a repeat section
          if data[1] not in course:
            course[data[1]] = {}
            course[data[1]]['lectures'] = []
            course[data[1]]['tutorials'] = []

          
          if data[2][1] != '2':
            # Add a section
            course[data[1]]['lectures'].append({
              'section': data[2],
              'time': data[4],
              'cap': data[3],
              'instructor': data[8],
              'extraCap': 0
              })
            if not course['manualTutorialEnrolment'] and data[6].strip():
              course[data[1]]['tutorials'].append(data[6].strip())
          else:
            for lec in course[data[1]]['lectures']:
              if lec['time'] == data[4]:
                if len(data[3]) == 0:
                  print('ERROR?! ' + line)
                else:
                  lec['extraCap'] = lec['extraCap'] + int(data[3])
                break
      
      elif data[2].startswith('T'):
        course['manualTutorialEnrolment'] = data[3] != ''
        if course['manualTutorialEnrolment']:
          course[data[1]]['tutorials'].append([data[2], data[6].strip()])
        elif data[6].strip() and (not (data[6].strip() in course[data[1]]['tutorials'])):
          course[data[1]]['tutorials'].append(data[6].strip())


def generateRows(course):
  rows = []
  for term in ['Fall', 'Winter']:
    if term in course: 
      rows.append('<td><table class="courseTable">')
      termRows = []

      for i, lec in enumerate(course[term]['lectures']):
        if course['manualTutorialEnrolment']:
          tutString = ''
        else:
          tutString = '<span style="float: right">(Tut {})</span>'.format(course[term]['tutorials'][i])
        row = '<tr>'

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
    else:
      rows.append('<td></td>')
  rows = (['<tr class="searchClass">', 
    '<td class="timetableCourseName" style="vertical-align:top">{}</td>'
    .format(course['name'])] + rows + ['</tr>'])


  return rows

        

def generateHTML():
  with open('res/timetableHTML.html', 'w+') as htmlOutput:
    htmlOutput.write('<table id="timetableMain"><tr>' +
      '<td class="timetableCourseName"></td>' +
      '<th class="sessionHeader">FALL</th>' + 
      '<th class="sessionHeader">SPRING</th></tr>'
      )
    
    # Header row
    htmlOutput.write('<tr><td class="timetableCourseName"></td>' + 
      '<td><table><tr><th class="timetableSection">Sec</th>' + 
      '<th class="timetableTime">Time</th>' +
      '<th class="timetableInstructor">Instructor</th>' +
      '<th class="timetableCap">Cap</th></tr></table></td>' +
      '<td><table><tr><th class="timetableSection">Sec</th>' + 
      '<th class="timetableTime">Time</th>' +
      '<th class="timetableInstructor">Instructor</th>' +
      '<th class="timetableCap">Cap</th></tr></table></td>'
      )

    for course in courses:
      htmlOutput.writelines(iter(generateRows(course)))
    htmlOutput.write('</table>')

if __name__ == '__main__':
  parseTimetable()
  generateHTML()